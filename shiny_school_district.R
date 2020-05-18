library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(dbplyr)
library(educationdata)
library(ggminthemes)
library(glue)
library(ggtext)
library(tigris)
library(leaflet)
library(sf)



path <- "/Users/rap168/ccd_2001-2016.sqlite"

ccd <- DBI::dbConnect(RSQLite::SQLite(), path)

# Modify the ggplot theme!
theme_set(
  theme_minima() +
    theme(text = element_text(color = "#471323"),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 16),
          plot.caption = element_text(size = 12),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          plot.background = element_rect(fill = "#f2e9e4"),
          panel.background = element_rect(fill = "#f2e9e4"))
)

# Table of districts leaid, name, fips, and year
dir_dat <- tbl(ccd, "ccd_district_dir") %>% 
  filter(year %in% 2010:2016, number_of_schools > 1) %>% 
  select(year:fips) %>% 
  collect() %>% 
  mutate(lea_name = str_to_title(lea_name),
         leaid = as.numeric(leaid))

states <- state.name[-11]
dir_dat <- dir_dat %>% filter(fips %in% states)

seda_math <- read_csv("seda_math.csv") %>% 
  mutate(leaidC = as.numeric(leaidC)) 

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  #heme = shinytheme("lumen"),
  includeCSS("www/CSS.css"),
  
  setBackgroundColor(
    color = c("#c9ada7", "#f2e9e4"),
    gradient = "linear",
    direction = "bottom"
  ),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),

  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #370B29;}"))
  ),
  
  #tags$style(HTML("
  #  body, pre { font-family: Roboto, Lato, sans-serif; }
  #")),

    # Application title
    titlePanel("The Landscape of Educational Opportunity"),
  
   fluidRow(
     
     column(2,
            selectInput(inputId = "state",
                        label = "State:",
                        choices = states, 
                        selected = "California"),
            selectInput(inputId = "year",
                        label = "Year:",
                        choices = 2010:2016,
                        selected = 2016),
            htmlOutput("district_selector")
            ),
     column(4,
            h3("District Demographics"),
            hr(),
            withSpinner(plotOutput("dem_p"), color = "#4a4e69")
            ),
     column(4,
            h3("District Boundary Map"),
            hr(),
            withSpinner(leafletOutput("d_map"), color = "#4a4e69")
     )
     ),
  
  fluidRow(
    
    column(5, 
           h3("District Opportunity Data"),
           hr(),
           tabsetPanel(
             type = "pills",
             tabPanel("District Segregation", withSpinner(plotOutput("seg_p"), color = "#4a4e69")),
             tabPanel("District Poverty", withSpinner(plotOutput("pov_p"), color = "#4a4e69")),
             tabPanel("Local Per-Student Revenue", withSpinner(plotOutput("loc_p"), color = "#4a4e69")),
             tabPanel("State Per-Student Revenue", withSpinner(plotOutput("state_p"), color = "#4a4e69"))
             #,
             #tabPanel("Tab", withSpinner(tableOutput("tab"), color = "#4a4e69"))
      )
      ),
    column(5, 
           h3("District Achievement Data"),
           hr(),
           tabsetPanel(
             type = "pills",
             tabPanel("District Math Achievement", withSpinner(plotOutput("mach_p"), color = "#4a4e69")),
             tabPanel("District Black-White Test Gap", withSpinner(plotOutput("bwg_p"), color = "#4a4e69")),
             tabPanel("District Hispanic-White Test Gap", withSpinner(plotOutput("hwg_p"), color = "#4a4e69"))
           )
    )
    )
  )


server = function(input, output) {
  
  # Populate the district dropdown menu based on state and year selection
  output$district_selector = renderUI({
    
    st_dir <- 
      tbl(ccd, "ccd_school_dir") %>% 
      filter(year == !!input$year,
             fips == !!input$state,
             school_level %in% c("Primary")) %>% 
      pull(leaid) %>% 
      unique() %>% 
      as.numeric()
    
    abbrs <- paste(c("(Isd", "Sd", "Cusd", "Esd", "Ccsd", "Csd", "Cud)"), collapse = "|")
    
    data_available <- dir_dat %>% 
      filter(fips == input$state, 
             year == input$year,
             leaid %in% st_dir) %>%
      pull(lea_name) %>% 
      unique() %>% 
      {gsub(abbrs, "\\U\\1", perl = T, .)} %>% 
      sort()
    
    selectInput(inputId = "district", 
                label = "School District:", 
                choices = sort(data_available),
                selected = data_available[1])
    
  })
  
  
  # DATA ----
  
  ## Need leaid number to select district
  lea_id <- reactive({
    dir_dat %>%
    filter(fips == input$state, 
           year == input$year, 
           lea_name == input$district) %>% 
    pull(leaid) %>% 
    unique() 
    })

  ## Geographic data
  geo_dat <- reactive({
    
    elem_geo <- try(school_districts(input$state, year = input$year, type = "elementary", class = "sf"))
    geo_dat <- school_districts(input$state, year = input$year, class = "sf")
    
    # Some states don't have elementary districts
    elem_test <- ifelse(is.null(nrow(elem_geo)), FALSE, nrow(elem_geo) != 0)
    
    if (elem_test & nrow(geo_dat) != 0) {
      geo_dat <- rbind_tigris(geo_dat, elem_geo)
    } else if (elem_test & nrow(geo_dat) == 0) {
      geo_dat <- elem_geo
    }
    
    if (str_detect(input$district, "New York City")) {
      geo_dat <- geo_dat %>%
        mutate(GEOID = as.numeric(GEOID)) %>% 
        filter(NAME == "New York City Department Of Education") 
    } else {
      geo_dat <- geo_dat %>% 
        mutate(GEOID = as.numeric(GEOID)) %>% 
        filter(GEOID == lea_id())
    }
    
  })
  
  # Statewide demographic data 
  st_dat <- reactive({
    
    st_dir <- 
      tbl(ccd, "ccd_school_dir") %>% 
        filter(year == !!input$year,
               fips == !!input$state,
               school_level %in% c("Primary")) %>% 
        pull(ncessch) %>% 
        as.numeric()
    
    tbl(ccd, "ccd_school_enroll") %>% 
      mutate(leaid = as.numeric(leaid),
             ncessch = as.numeric(ncessch)) %>% 
      filter(year == !!input$year,
             fips == !!input$state,
             !is.na(enrollment),
             !race %in% c("Two or more races", "Unknown"),
             ncessch %in% st_dir) %>%
      select(ncessch, leaid, race, enrollment) %>% 
      collect()
    
  })
  
  ## Districts with only 1 elementary school -- got to drop these
  
  districts_only_1_school <- reactive({
    st_dat() %>% 
      select(ncessch, leaid) %>% 
      distinct() %>% 
      count(leaid) %>% 
      filter(n < 2) %>% 
      pull(leaid) 
    })
  
  ## District-wide demographic data
  sch_dat <- reactive({
   
   tbl(ccd, "ccd_school_enroll") %>% 
     mutate(leaid = as.numeric(leaid),
            ncessch = as.numeric(ncessch)) %>% 
     filter(year == !!input$year,
            leaid == !!lea_id(),
            !is.na(enrollment),
            !race %in% c("Two or more races", "Unknown")) %>% 
     select(year:ncessch, fips, leaid, race, enrollment) %>% 
     collect()
   
  })
  
  ## Math achievement data
  st_mach <- reactive({
    
    st_abbr <- fips_codes %>% filter(state_name == input$state) %>% pull(state) %>% unique()
    
     seda_math %>% 
      filter(stateabb == st_abbr,
             year == input$year,
             !leaidC %in% districts_only_1_school())
    
  }) 
  
  ## Poverty data 
  pov_dat <- reactive({
    
    fipscd <- fips_codes %>% filter(state_name == input$state) %>% pull(state_code) %>% unique() %>% as.numeric()
    
    get_education_data(level = "school-districts",
                       source = "saipe",
                       filters = list(year = input$year,
                                      fips = fipscd)) %>% 
      mutate(leaid = as.numeric(leaid)) %>%
      select(leaid, est_population_5_17_poverty_pct) %>% 
      filter(!leaid %in% districts_only_1_school())
    
    })
  
  ## Finance data
  
  fin_dat <- reactive({
    
    fipscd <- fips_codes %>% filter(state_name == input$state) %>% pull(state_code) %>% unique() %>% as.numeric()
    
    get_education_data(level = "school-districts",
                       source = "ccd",
                       topic = "finance",
                       filters = list(year = input$year,
                                      fips = fipscd)) %>% 
      mutate(leaid = as.numeric(leaid)) %>%
      select(leaid, matches("rev_.+_total$")) %>% 
      filter(!leaid %in% districts_only_1_school()) %>% 
      na_if(-1) %>% 
      na_if(-2) %>% 
      na_if(-3)
    
  })
  
  ## Segregation data
  seg_dist <- reactive({

   st_dat() %>% 
     filter(!leaid %in% districts_only_1_school(),
            race != "Total") %>% 
     group_nest(leaid) %>% 
     mutate(seg_idx = map_dbl(data, ~ segregation::mutual_total(.x, "ncessch", "race", weight = "enrollment") %>% slice(2) %>% pull(est))) %>% 
     select(-data) %>% 
     mutate(scaled_seg = scale(log(seg_idx + 0.00001))[,1]) 
    
 })


 # OUTPUTS ----
 
  ## Map
 output$d_map = renderLeaflet({
   
   geo_dat() %>%
     st_transform(crs = "+init=epsg:4326") %>%
     leaflet(width = "100%") %>%
     addProviderTiles(providers$CartoDB.Voyager) %>%
     addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                 stroke = TRUE,
                 weight = 0.75,
                 smoothFactor = 0,
                 fillOpacity = 0.7,
                 color = "#0D0D36", 
                 fillColor = "#9a8c98")
   
 })
 
  ## Basic demographics
 output$dem_p = renderPlot({
  
   sch_dat() %>% 
     filter(race != "Total") %>% 
     group_by(leaid, race) %>% 
     summarize(total = sum(enrollment, na.rm = TRUE)) %>% 
     ungroup() %>% 
     mutate(prop = total/sum(total)) %>% 
     ggplot(aes(x = prop, y = factor(leaid))) +
     geom_col(aes(fill = race), width = 0.5) +
     labs(x = "", y = "",
          caption = "Data from the National Center for Education Statistics, accessed through the Urban Institute's API") +
     scale_fill_viridis_d(name = "Race/Ethnicity") +
     guides(fill = guide_legend(ncol = 2)) +
     theme(legend.position = "bottom",
           axis.text.y = element_blank(),
           axis.ticks.y = element_blank())

 })
 
 ## Segregation distribution
 output$seg_p = renderPlot({
  
   seg_ind <- seg_dist()$scaled_seg[which(seg_dist()$leaid == lea_id())]
   d_mn_seg <- mean(seg_dist()$scaled_seg, na.rm = TRUE)
   
   seg_ind <- ifelse(length(seg_ind) == 0, NA, seg_ind)
   
   if (is.na(seg_ind)) {
     ggplot() + 
       geom_density(data = seg_dist(), aes(x = scaled_seg),
                    fill = "#9a8c98", color = "#22223b", alpha = 0.5) +
       labs(x = "", y = "",
            title = glue("There is <span style=color:#D94A3D;'>no segregation index</span> for {input$district}"),
            caption = "Creator's calculation. Data from the National Center for Education Statistics, accessed through the Urban Institute's API") +
       theme(plot.title = element_markdown(lineheight = 1.1)) 
   } else {
     ttl <- glue("{input$district} has {case_when(seg_ind < d_mn_seg - 0.01 ~ \"<span style=color:#305CD7;'>below average</span>\", seg_ind > d_mn_seg + 0.01 ~ \"<span style=color:#D94A3D;'>above average</span>\", TRUE ~ \"<span style=color:#4DAF4A;'>about average</span>\")} racial segregation for {input$state}")
     ggplot() + 
       geom_density(data = seg_dist(), aes(x = scaled_seg),
                    fill = "#9a8c98", color = "#22223b", alpha = 0.5) +
       geom_vline(xintercept = seg_ind, color = "#0D0D36") +
       geom_vline(xintercept = d_mn_seg, color = "#D85A4F", linetype = 2) +
       labs(x = "", y = "",
            title = ttl,
            caption = "Creator's calculation. Data from the National Center for Education Statistics, accessed through the Urban Institute's API") +
       theme(plot.title = element_markdown(lineheight = 1.1))  
   }
   
   })
 
 
 
 ## Finance data
 
 output$loc_p = renderPlot({
   
   fin <- fin_dat() %>% 
     left_join(st_dat() %>% filter(race == "Total", enrollment != 0), by = "leaid") %>% 
     mutate_at(vars(contains("rev_")), ~ ./enrollment) %>% 
     mutate_at(vars(contains("rev_")), ~ log(. + 1)) %>% 
     mutate_at(vars(contains("rev_")), list(scaled = ~ scale(.)))
   
   
   loc_ind <- fin$rev_local_total[which(fin$leaid == lea_id())]
   d_mn_loc <- mean(fin$rev_local_total, na.rm = TRUE)
   
   loc_ind <- ifelse(length(loc_ind) == 0, NA, loc_ind)
   
   if (is.na(loc_ind)) {
     ggplot() + 
       geom_density(data = fin, aes(x = rev_local_total),
                    fill = "#9a8c98", color = "#22223b", alpha = 0.5) +
       labs(x = "", y = "",
            title = glue("There is <span style=color:#D94A3D;'>no finance information</span> for {input$district}"),
            caption = "Creator's calculation. Data from the National Center for Education Statistics, accessed through the Urban Institute's API") +
       theme(plot.title = element_markdown(lineheight = 1.1)) 
   } else {
     ttl <- glue("{input$district} has {case_when(loc_ind > d_mn_loc - 0.01 ~ \"<span style=color:#305CD7;'>above average</span>\", loc_ind < d_mn_loc + 0.01 ~ \"<span style=color:#D94A3D;'>below average</span>\", TRUE ~ \"<span style=color:#4DAF4A;'>about average</span>\")} per-student reveune from local sources for {input$state}")
     ggplot() + 
       geom_density(data = fin, aes(x = rev_local_total),
                    fill = "#9a8c98", color = "#22223b", alpha = 0.5) +
       geom_vline(xintercept = loc_ind, color = "#0D0D36") +
       geom_vline(xintercept = d_mn_loc, color = "#D85A4F", linetype = 2) +
       labs(x = "", y = "",
            title = ttl,
            caption = "Creator's calculation. Data from the National Center for Education Statistics, accessed through the Urban Institute's API") +
       theme(plot.title = element_markdown(lineheight = 1.1))  
   }
   
 })
 
 
 
 
 output$state_p = renderPlot({
   
   fin <- fin_dat() %>% 
     left_join(st_dat() %>% filter(race == "Total", enrollment != 0), by = "leaid") %>% 
     mutate_at(vars(contains("rev_")), ~ ./enrollment) %>% 
     mutate_at(vars(contains("rev_")), ~ log(. + 1)) %>% 
     mutate_at(vars(contains("rev_")), list(scaled = ~ scale(.)))
   
   
   loc_ind <- fin$rev_state_total[which(fin$leaid == lea_id())]
   d_mn_st <- mean(fin$rev_state_total, na.rm = TRUE)
   
   loc_ind <- ifelse(length(loc_ind) == 0, NA, loc_ind)
   
   if (is.na(loc_ind)) {
     ggplot() + 
       geom_density(data = fin, aes(x = rev_state_total),
                    fill = "#9a8c98", color = "#22223b", alpha = 0.5) +
       labs(x = "", y = "",
            title = glue("There is <span style=color:#D94A3D;'>no finance information</span> for {input$district}"),
            caption = "Creator's calculation. Data from the National Center for Education Statistics, accessed through the Urban Institute's API") +
       theme(plot.title = element_markdown(lineheight = 1.1)) 
   } else {
     ttl <- glue("{input$district} has {case_when(loc_ind > d_mn_st - 0.01 ~ \"<span style=color:#305CD7;'>above average</span>\", loc_ind < d_mn_st + 0.01 ~ \"<span style=color:#D94A3D;'>below average</span>\", TRUE ~ \"<span style=color:#4DAF4A;'>about average</span>\")} per-student reveune from state sources for {input$state}")
     ggplot() + 
       geom_density(data = fin, aes(x = rev_state_total),
                    fill = "#9a8c98", color = "#22223b", alpha = 0.5) +
       geom_vline(xintercept = loc_ind, color = "#0D0D36") +
       geom_vline(xintercept = d_mn_st, color = "#D85A4F", linetype = 2) +
       labs(x = "", y = "",
            title = ttl,
            caption = "Creator's calculation. Data from the National Center for Education Statistics, accessed through the Urban Institute's API") +
       theme(plot.title = element_markdown(lineheight = 1.1))  
   }
   
 })
 
 
 
 
  
 
 
 
 ## Poverty distribution
 output$pov_p = renderPlot({
   
   pov_ind <- pov_dat()$est_population_5_17_poverty_pct[which(pov_dat()$leaid == lea_id())]
   d_mn_pov <- mean(pov_dat()$est_population_5_17_poverty_pct, na.rm = TRUE)
   
   ttl <- glue("{input$district} has {case_when(pov_ind < d_mn_pov - 0.01 ~ \"<span style=color:#305CD7;'>below average</span>\", pov_ind > d_mn_pov + 0.01 ~ \"<span style=color:#D94A3D;'>above average</span>\", TRUE ~ \"<span style=color:#4DAF4A;'>about average</span>\")} poverty for {input$state}")
   
   ggplot() + 
     geom_density(data = pov_dat(), aes(x = est_population_5_17_poverty_pct),
                  fill = "#9a8c98", color = "#22223b", alpha = 0.5) +
     geom_vline(xintercept = pov_ind, color = "#0D0D36") +
     geom_vline(xintercept = d_mn_pov, color = "#D85A4F", linetype = 2) +
     labs(x = "", y = "",
          title = ttl,
          caption = "Data from the US Census's Small Area Income and Poverty Estimates, accessed through the Urban Institute's API") +
     theme(plot.title = element_markdown(lineheight = 1.1)) 
   
 })
 
 
 

 ## 5th grade math achievement
 output$mach_p = renderPlot({
  
   ach_dat <- st_mach()
   mach_ind <- ach_dat$mn_all[which(ach_dat$leaidC == lea_id())]
   d_mn_mach <- mean(ach_dat$mn_all, na.rm = TRUE)
   
   ttl <- glue("{input$district} has {case_when(mach_ind < d_mn_mach - 0.01 ~ \"<span style=color:#D94A3D;'>below average</span>\", mach_ind > d_mn_mach + 0.01 ~ \"<span style=color:#305CD7;'>above average</span>\", TRUE ~ \"<span style=color:#4DAF4A;'>about average</span>\")} 5th grade math achievement for {input$state}")
   
   if (nrow(ach_dat) != 0) {
     if (length(mach_ind) != 0) {
       ggplot() + 
         geom_density(data = ach_dat, aes(x = mn_all),
                      fill = "#9a8c98", color = "#22223b", alpha = 0.5) +
         geom_vline(xintercept = mach_ind, color = "#0D0D36") +
         geom_vline(xintercept = d_mn_mach, color = "#D85A4F", linetype = 2) +
         labs(x = "", y = "", 
              title = ttl,
              caption = "Data from the Stanford Education Data Archive (https://edopportunity.org/get-the-data/seda-archive-downloads/)") +
         theme(plot.title = element_markdown(lineheight = 1.1))
     } else {
       ggplot() + 
         geom_density(data = ach_dat, aes(x = mn_all),
                      fill = "#9a8c98", color = "#22223b", alpha = 0.5) +
         labs(x = "", y = "", 
              title = glue("There are {\"<span style=color:#D94A3D;'>no math achievement data</span>\"} for {input$district} in {input$year}"),
              caption = "Data from the Stanford Education Data Archive (https://edopportunity.org/get-the-data/seda-archive-downloads/)") +
         theme(plot.title = element_markdown(lineheight = 1.1))
     }
   } else {
     ggplot() + 
       annotate(geom = "text", x = 1, y = 1, 
                label = glue("There are no math achievement data for {input$state} for {input$year}"),
                color = "#D85A4F") +
       labs(x = "", y = "") +
       theme(axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks = element_blank())
     }
   
 })
 
 ## BW test score gap
 output$bwg_p = renderPlot({
   
   d_mn_wbg <- mean(st_mach()$mn_wbg, na.rm = TRUE)
   wbg_ind <- st_mach()$mn_wbg[which(st_mach()$leaidC == lea_id())]
   
   # Sometimes the district is missing, sometime it is present but has a missing value!
   wbg_ind <- ifelse(length(wbg_ind) == 0, NA, wbg_ind)
   
   ttl <- glue("{input$district} has {case_when(wbg_ind < d_mn_wbg - 0.01 ~ \"<span style=color:#305CD7;'>a below average</span>\", wbg_ind > d_mn_wbg + 0.01 ~ \"<span style=color:#D94A3D;'>an above average</span>\", TRUE ~ \"<span style=color:#4DAF4A;'>an about average</span>\")} Black-White test score gap for {input$state}")
   
   if (nrow(st_mach()) == 0) { 
     ggplot() + 
       annotate(geom = "text", x = 1, y = 1, 
                label = glue("There is no math achievement data for {input$state} for {input$year}"),
                color = "#D85A4F") +
       labs(x = "", y = "") +
       theme(axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks = element_blank()) 
   } else if (is.na(wbg_ind)) {
     ggplot() + 
       geom_density(data = st_mach(), aes(x = mn_wbg),
                    fill = "#9a8c98", color = "#22223b", alpha = 0.5) +
       labs(x = "", y = "", 
            title = glue("There is {\"<span style=color:#D94A3D;'>no data</span>\"} for the Black-White test score gap in {input$district}"),
            caption = "Data from the Stanford Education Data Archive (https://edopportunity.org/get-the-data/seda-archive-downloads/)") +
       theme(plot.title = element_markdown(lineheight = 1.1))
   } else {
     ggplot() + 
       geom_density(data = st_mach(), aes(x = mn_wbg),
                    fill = "#9a8c98", color = "#22223b", alpha = 0.5) +
       geom_vline(xintercept = wbg_ind, color = "#0D0D36") +
       geom_vline(xintercept = d_mn_wbg, color = "#D85A4F", linetype = 2) +
       labs(x = "", y = "", 
            title = ttl,
            caption = "Data from the Stanford Education Data Archive (https://edopportunity.org/get-the-data/seda-archive-downloads/)") +
       theme(plot.title = element_markdown(lineheight = 1.1))
   }
   
 })
 
 ## HW Test Score gap
 output$hwg_p = renderPlot({
   
   d_mn_whg <- mean(st_mach()$mn_whg, na.rm = TRUE)
   whg_ind <- st_mach()$mn_whg[which(st_mach()$leaidC == lea_id())]
   
   # Sometimes the district is missing, sometime it is present but has a missing value!
   whg_ind <- ifelse(length(whg_ind) == 0, NA, whg_ind)
   
   ttl <- glue("{input$district} has {case_when(whg_ind < d_mn_whg - 0.01 ~ \"<span style=color:#305CD7;'>a below average</span>\", whg_ind > d_mn_whg + 0.01 ~ \"<span style=color:#D94A3D;'>an above average</span>\", TRUE ~ \"<span style=color:#4DAF4A;'>an about average</span>\")} Hispanic-White test score gap for {input$state}")
   
   if (nrow(st_mach()) == 0) { 
     ggplot() + 
       annotate(geom = "text", x = 1, y = 1, 
                label = glue("There is no math achievement data for {input$state} for {input$year}"),
                color = "#D85A4F") +
       labs(x = "", y = "") +
       theme(axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks = element_blank()) 
   } else if (is.na(whg_ind)) {
     ggplot() + 
       geom_density(data = st_mach(), aes(x = mn_whg),
                    fill = "#9a8c98", color = "#22223b", alpha = 0.5) +
       labs(x = "", y = "", 
            title = glue("There is {\"<span style=color:#D94A3D;'>no data</span>\"} for the Hispanic-White test score gap in {input$district}"),
            caption = "Data from the Stanford Education Data Archive (https://edopportunity.org/get-the-data/seda-archive-downloads/)") +
       theme(plot.title = element_markdown(lineheight = 1.1))
   } else {
     ggplot() + 
       geom_density(data = st_mach(), aes(x = mn_whg),
                    fill = "#9a8c98", color = "#22223b", alpha = 0.5) +
       geom_vline(xintercept = whg_ind, color = "#0D0D36") +
       geom_vline(xintercept = d_mn_whg, color = "#D85A4F", linetype = 2) +
       labs(x = "", y = "", 
            title = ttl,
            caption = "Data from the Stanford Education Data Archive (https://edopportunity.org/get-the-data/seda-archive-downloads/)") +
       theme(plot.title = element_markdown(lineheight = 1.1))
   }
   
 })
 
 output$reg_p = renderPlot({
   
   
   
 })
 
# output$tab = renderTable({
#   fin_dat() %>% 
#     left_join(st_dat() %>% filter(race == "Total", enrollment != 0), by = "leaid") %>% 
#     mutate_at(vars(contains("rev_")), ~ ./enrollment) %>% 
#     mutate_at(vars(contains("rev_")), ~ log(. + 1)) %>% 
#     mutate_at(vars(contains("rev_")), list(scaled = ~ scale(.)))
#   
# })

 }

# Run the application 
shinyApp(ui = ui, server = server)
