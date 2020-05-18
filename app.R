library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(educationdata)
library(ggminthemes)
library(glue)
library(ggtext)
library(tigris)
library(leaflet)
library(sf)
library(plotly)

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
              legend.title = element_text(size = 14))
)

# DATA ----------------------

# Table of districts leaid, name, fips, and year
dir_dat <- read_csv("dir_dat.csv") %>% 
    filter(year %in% 2011:2016) %>% 
    select(year:fips) %>% 
    mutate(lea_name = str_to_title(lea_name),
           leaid = as.numeric(leaid))

states <- state.name[-11]
dir_dat <- dir_dat %>% filter(fips %in% states)

seda_math <- read_csv("seda_math.csv") %>% 
    mutate(leaidC = as.numeric(leaidC)) 

### State school directory
fs <- dir(pattern = "full_st_dir")

full_st_dir <- map_dfr(fs, read_csv)

### State enrollment
fs <- dir(pattern = "full_st_enroll")

full_st_enroll <- map_dfr(fs, read_csv)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    title = "Educational opportunity",

    
    # HEADER ----
    
    dashboardHeader(
        title = span(icon("school"), "Select State, Year, & District"),
        titleWidth = 300
        ),
    
    
    
    # SIDEBAR ----
    
    dashboardSidebar(
        width = 300,
        selectizeInput(
            inputId = "state",
            label = "State:",
            choices = states, 
            selected = "California"
            ),
        selectizeInput(
            inputId = "year",
            label = "Year:",
            choices = 2010:2016,
            selected = 2016
        ),
        
        conditionalPanel(
            "input.state != '' & input.year != ''", 
            htmlOutput("district_selector")
            )
        
        ),
    
    
    
    # BODY ----
    
    dashboardBody(
        
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        
        fluidRow(
            h2("The Landscape of Educational Opportunity", align = "center"),
            hr()
            ),
        
        fluidRow(
            column(6,
                   h1(textOutput("s_name"), align = "center"),
                   hr()),
            column(6,
                   h1(textOutput("d_name"), align = "center"),
                   hr()
            )
        ),
        
        fluidRow(
            valueBoxOutput("seg_box", width = 3),
            valueBoxOutput("pov_box", width = 3),
            valueBoxOutput("rev_box", width = 3),
            valueBoxOutput("ach_box", width = 3)
        ),
        
        fluidRow(
            box(title = tagList(icon("chart-bar"), "District Profile"), 
                tabPanel("Demographics", withSpinner(plotlyOutput("dem_p"), color = "#4a4e69"))),
            box(title = tagList(icon("map-marker-alt"), "District Boundaries"), 
                tabPanel("Boundary Map", withSpinner(leafletOutput("d_map"), color = "#4a4e69")))
        ),
        
        fluidRow(
            tabBox(title = tagList(icon("seedling"), "District Opportunity Data"),
                   tabPanel("Segregation", withSpinner(plotOutput("seg_p"), color = "#4a4e69")),
                   tabPanel("Poverty", withSpinner(plotOutput("pov_p"), color = "#4a4e69")),
                   tabPanel("Local Per-Student Revenue", withSpinner(plotOutput("loc_p"), color = "#4a4e69")),
                   tabPanel("State Per-Student Revenue", withSpinner(plotOutput("state_p"), color = "#4a4e69"))
                   ),
            tabBox(title = tagList(icon("book-open"), "District Achievement Data"), 
                   tabPanel("Math Achievement", withSpinner(plotOutput("mach_p"), color = "#4a4e69")),
                   tabPanel("Black-White Test Gap", withSpinner(plotOutput("bwg_p"), color = "#4a4e69")),
                   tabPanel("Hispanic-White Test Gap", withSpinner(plotOutput("hwg_p"), color = "#4a4e69"))
                )
            )
        )
    )

server = function(input, output) {
    
    # Populate the district dropdown menu based on state and year selection
    output$district_selector = renderUI({
        
        st_dir <- 
            full_st_dir %>% 
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
            full_st_dir %>% 
            filter(year == !!input$year,
                   fips == !!input$state,
                   school_level %in% c("Primary")) %>% 
            pull(ncessch) %>% 
            as.numeric()
        
        full_st_enroll %>% 
            mutate(leaid = as.numeric(leaid),
                   ncessch = as.numeric(ncessch)) %>% 
            filter(year == !!input$year,
                   fips == !!input$state,
                   !is.na(enrollment),
                   !race %in% c("Two or more races", "Unknown"),
                   ncessch %in% st_dir) %>%
            select(ncessch, leaid, race, enrollment)
        
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
        
        full_st_enroll %>% 
            mutate(leaid = as.numeric(leaid),
                   ncessch = as.numeric(ncessch)) %>% 
            filter(year == !!input$year,
                   leaid == !!lea_id(),
                   !is.na(enrollment),
                   !race %in% c("Two or more races", "Unknown")) %>% 
            select(year:ncessch, fips, leaid, race, enrollment) 
        
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
    
    ## State & district name 
    
    output$s_name <- renderText(paste0("State: ", input$state))
    output$d_name <- renderText(paste0("District: ", input$district))
    
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
                        color = "midnightblue", 
                        fillColor = "cornflowerblue")
        
    })
    
    ## Basic demographics
    output$dem_p = renderPlotly({
        
        p1 <- sch_dat() %>% 
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
        
        ggplotly(p1) %>% layout(legend = list(orientation = "h", tracegroupgap = 2))
        
    })
    
    
    
    ## Segregation distribution
    
    output$seg_p = renderPlot({
        
        seg_ind <- seg_dist()$scaled_seg[which(seg_dist()$leaid == lea_id())]
        d_mn_seg <- mean(seg_dist()$scaled_seg, na.rm = TRUE)
        
        seg_ind <- ifelse(length(seg_ind) == 0, NA, seg_ind)
        
        if (is.na(seg_ind)) {
            ggplot() + 
                geom_density(data = seg_dist(), aes(x = scaled_seg),
                             fill = "cornflowerblue", color = "midnightblue", alpha = 0.5) +
                labs(x = "", y = "",
                     title = glue("There is <span style=color:#d22d2d;'>no segregation index</span> for {input$district}"),
                     caption = "Creator's calculation. Data from the National Center for Education Statistics, accessed through the Urban Institute's API") +
                theme(plot.title = element_markdown(lineheight = 1.1)) 
        } else {
            ttl <- glue("{input$district} has {case_when(seg_ind < d_mn_seg - 0.01 ~ \"<span style=color:#0099ff;'>below average</span>\", seg_ind > d_mn_seg + 0.01 ~ \"<span style=color:#d22d2d;'>above average</span>\", TRUE ~ \"<span style=color:#4DAF4A;'>about average</span>\")} racial segregation for {input$state}")
            ggplot() + 
                geom_density(data = seg_dist(), aes(x = scaled_seg),
                             fill = "cornflowerblue", color = "midnightblue", alpha = 0.5) +
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
                             fill = "cornflowerblue", color = "midnightblue", alpha = 0.5) +
                labs(x = "", y = "",
                     title = glue("There is <span style=color:#d22d2d;'>no finance information</span> for {input$district}"),
                     caption = "Creator's calculation. Data from the National Center for Education Statistics, accessed through the Urban Institute's API") +
                theme(plot.title = element_markdown(lineheight = 1.1)) 
        } else {
            ttl <- glue("{input$district} has {case_when(loc_ind > d_mn_loc - 0.01 ~ \"<span style=color:#0099ff;'>above average</span>\", loc_ind < d_mn_loc + 0.01 ~ \"<span style=color:#d22d2d;'>below average</span>\", TRUE ~ \"<span style=color:#4DAF4A;'>about average</span>\")} per-student reveune from local sources for {input$state}")
            ggplot() + 
                geom_density(data = fin, aes(x = rev_local_total),
                             fill = "cornflowerblue", color = "midnightblue", alpha = 0.5) +
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
                             fill = "cornflowerblue", color = "midnightblue", alpha = 0.5) +
                labs(x = "", y = "",
                     title = glue("There is <span style=color:#d22d2d;'>no finance information</span> for {input$district}"),
                     caption = "Creator's calculation. Data from the National Center for Education Statistics, accessed through the Urban Institute's API") +
                theme(plot.title = element_markdown(lineheight = 1.1)) 
        } else {
            ttl <- glue("{input$district} has {case_when(loc_ind > d_mn_st - 0.01 ~ \"<span style=color:#0099ff;'>above average</span>\", loc_ind < d_mn_st + 0.01 ~ \"<span style=color:#d22d2d;'>below average</span>\", TRUE ~ \"<span style=color:#4DAF4A;'>about average</span>\")} per-student reveune from state sources for {input$state}")
            ggplot() + 
                geom_density(data = fin, aes(x = rev_state_total),
                             fill = "cornflowerblue", color = "midnightblue", alpha = 0.5) +
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
        
        ttl <- glue("{input$district} has {case_when(pov_ind < d_mn_pov - 0.01 ~ \"<span style=color:#0099ff;'>below average</span>\", pov_ind > d_mn_pov + 0.01 ~ \"<span style=color:#d22d2d;'>above average</span>\", TRUE ~ \"<span style=color:#4DAF4A;'>about average</span>\")} poverty for {input$state}")
        
        ggplot() + 
            geom_density(data = pov_dat(), aes(x = est_population_5_17_poverty_pct),
                         fill = "cornflowerblue", color = "midnightblue", alpha = 0.5) +
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
        
        ttl <- glue("{input$district} has {case_when(mach_ind < d_mn_mach - 0.01 ~ \"<span style=color:#d22d2d;'>below average</span>\", mach_ind > d_mn_mach + 0.01 ~ \"<span style=color:#0099ff;'>above average</span>\", TRUE ~ \"<span style=color:#4DAF4A;'>about average</span>\")} 5th grade math achievement for {input$state}")
        
        if (nrow(ach_dat) != 0) {
            if (length(mach_ind) != 0) {
                ggplot() + 
                    geom_density(data = ach_dat, aes(x = mn_all),
                                 fill = "cornflowerblue", color = "midnightblue", alpha = 0.5) +
                    geom_vline(xintercept = mach_ind, color = "#0D0D36") +
                    geom_vline(xintercept = d_mn_mach, color = "#D85A4F", linetype = 2) +
                    labs(x = "", y = "", 
                         title = ttl,
                         caption = "Data from the Stanford Education Data Archive (https://edopportunity.org/get-the-data/seda-archive-downloads/)") +
                    theme(plot.title = element_markdown(lineheight = 1.1))
            } else {
                ggplot() + 
                    geom_density(data = ach_dat, aes(x = mn_all),
                                 fill = "cornflowerblue", color = "midnightblue", alpha = 0.5) +
                    labs(x = "", y = "", 
                         title = glue("There are {\"<span style=color:#d22d2d;'>no math achievement data</span>\"} for {input$district} in {input$year}"),
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
        
        ttl <- glue("{input$district} has {case_when(wbg_ind < d_mn_wbg - 0.01 ~ \"<span style=color:#0099ff;'>a below average</span>\", wbg_ind > d_mn_wbg + 0.01 ~ \"<span style=color:#d22d2d;'>an above average</span>\", TRUE ~ \"<span style=color:#4DAF4A;'>an about average</span>\")} Black-White test score gap for {input$state}")
        
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
                             fill = "cornflowerblue", color = "midnightblue", alpha = 0.5) +
                labs(x = "", y = "", 
                     title = glue("There is {\"<span style=color:#d22d2d;'>no data</span>\"} for the Black-White test score gap in {input$district}"),
                     caption = "Data from the Stanford Education Data Archive (https://edopportunity.org/get-the-data/seda-archive-downloads/)") +
                theme(plot.title = element_markdown(lineheight = 1.1))
        } else {
            ggplot() + 
                geom_density(data = st_mach(), aes(x = mn_wbg),
                             fill = "cornflowerblue", color = "midnightblue", alpha = 0.5) +
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
        
        ttl <- glue("{input$district} has {case_when(whg_ind < d_mn_whg - 0.01 ~ \"<span style=color:#0099ff;'>a below average</span>\", whg_ind > d_mn_whg + 0.01 ~ \"<span style=color:#d22d2d;'>an above average</span>\", TRUE ~ \"<span style=color:#4DAF4A;'>an about average</span>\")} Hispanic-White test score gap for {input$state}")
        
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
                             fill = "cornflowerblue", color = "midnightblue", alpha = 0.5) +
                labs(x = "", y = "", 
                     title = glue("There is {\"<span style=color:#d22d2d;'>no data</span>\"} for the Hispanic-White test score gap in {input$district}"),
                     caption = "Data from the Stanford Education Data Archive (https://edopportunity.org/get-the-data/seda-archive-downloads/)") +
                theme(plot.title = element_markdown(lineheight = 1.1))
        } else {
            ggplot() + 
                geom_density(data = st_mach(), aes(x = mn_whg),
                             fill = "cornflowerblue", color = "midnightblue", alpha = 0.5) +
                geom_vline(xintercept = whg_ind, color = "#0D0D36") +
                geom_vline(xintercept = d_mn_whg, color = "#D85A4F", linetype = 2) +
                labs(x = "", y = "", 
                     title = ttl,
                     caption = "Data from the Stanford Education Data Archive (https://edopportunity.org/get-the-data/seda-archive-downloads/)") +
                theme(plot.title = element_markdown(lineheight = 1.1))
        }
        
    })
    
    
    
    ## Segregation progress box

    output$seg_box <- renderValueBox({
        
        seg_ind <- seg_dist()$scaled_seg[which(seg_dist()$leaid == lea_id())]
        d_mn_seg <- mean(seg_dist()$scaled_seg, na.rm = TRUE)
        
        seg_ind <- ifelse(length(seg_ind) == 0, NA, seg_ind)

        if (is.na(seg_ind)) {
            valueBox(
                value = tags$p("No data", style = "font-family: 'Permanent Marker'; font-size: 100%;"),, 
                subtitle = "Segregation", 
                icon = icon("map"),
                color = "purple"
                )
        } else if (seg_ind > (d_mn_seg + 0.01)) {
            valueBox(
                value = tags$p("Above average", style = "font-family: 'Permanent Marker'; font-size: 100%;"),
                subtitle = "Segregation", 
                icon = icon("map"),
                color = "red"
            ) 
            } else if (seg_ind < (d_mn_seg - 0.01)) {
                valueBox(
                    value = tags$p("Below average", style = "font-family: 'Permanent Marker'; font-size: 100%;"),
                    subtitle = "Segregation", 
                    icon = icon("map"),
                    color = "blue"
                ) 
            } else {
                valueBox(
                    value = tags$p("About average", style = "font-family: 'Permanent Marker'; font-size: 100%;"),
                    subtitle = "Segregation", 
                    icon = icon("map"),
                    color = "green"
                ) 
            }
        
    })
    
    output$rev_box <- renderValueBox({
        
        fin <- fin_dat() %>% 
            left_join(st_dat() %>% filter(race == "Total", enrollment != 0), by = "leaid") %>% 
            mutate_at(vars(contains("rev_")), ~ ./enrollment) %>% 
            mutate_at(vars(contains("rev_")), ~ log(. + 1)) %>% 
            mutate_at(vars(contains("rev_")), list(scaled = ~ scale(.)))
        
        
        loc_ind <- fin$rev_state_total[which(fin$leaid == lea_id())]
        d_mn_st <- mean(fin$rev_state_total, na.rm = TRUE)
        
        loc_ind <- ifelse(length(loc_ind) == 0, NA, loc_ind)
        
        if (is.na(loc_ind)) {
            valueBox(
                value = tags$p("No data", style = "font-family: 'Permanent Marker'; font-size: 100%;"),, 
                subtitle = "Local Revenue", 
                icon = icon("money-bill-alt"),
                color = "purple"
            )
        } else if (loc_ind > (d_mn_st + 1)) {
            valueBox(
                value = tags$p("Above average", style = "font-family: 'Permanent Marker'; font-size: 100%;"),
                subtitle = "Local Revenue", 
                icon = icon("money-bill-alt"),
                color = "blue"
            ) 
        } else if (loc_ind < (d_mn_st - 1)) {
            valueBox(
                value = tags$p("Below average", style = "font-family: 'Permanent Marker'; font-size: 100%;"),
                subtitle = "Local Revenue", 
                icon = icon("money-bill-alt"),
                color = "red"
            ) 
        } else {
            valueBox(
                value = tags$p("About average", style = "font-family: 'Permanent Marker'; font-size: 100%;"),
                subtitle = "Local Revenue", 
                icon = icon("money-bill-alt"),
                color = "green"
            ) 
        }
    })
    
    output$pov_box <- renderValueBox({
        
        pov_ind <- pov_dat()$est_population_5_17_poverty_pct[which(pov_dat()$leaid == lea_id())]
        d_mn_pov <- mean(pov_dat()$est_population_5_17_poverty_pct, na.rm = TRUE)
        
        pov_ind <- ifelse(length(pov_ind) == 0, NA, pov_ind)
        
        if (is.na(pov_ind)) {
            valueBox(
                value = tags$p("No data", style = "font-family: 'Permanent Marker'; font-size: 100%;"),, 
                subtitle = "Poverty",
                icon = icon("home"),
                color = "purple"
            )
        } else if (pov_ind > (d_mn_pov + 0.01)) {
            valueBox(
                value = tags$p("Above average", style = "font-family: 'Permanent Marker'; font-size: 100%;"),
                subtitle = "Poverty",
                icon = icon("home"),
                color = "red"
            ) 
        } else if (pov_ind < (d_mn_pov - 0.01)) {
            valueBox(
                value = tags$p("Below average", style = "font-family: 'Permanent Marker'; font-size: 100%;"),
                subtitle = "Poverty", 
                icon = icon("home"),
                color = "blue"
            ) 
        } else {
            valueBox(
                value = tags$p("About average", style = "font-family: 'Permanent Marker'; font-size: 100%;"),
                subtitle = "Poverty",
                icon = icon("home"),
                color = "green"
            ) 
        }
    })
    
    
    output$ach_box <- renderValueBox({
        
        ach_dat <- st_mach()
        mach_ind <- ach_dat$mn_all[which(ach_dat$leaidC == lea_id())]
        d_mn_mach <- mean(ach_dat$mn_all, na.rm = TRUE)
        
        mach_ind <- ifelse(length(mach_ind) == 0, NA, mach_ind)
        
        if (is.na(mach_ind)) {
            valueBox(
                value = tags$p("No data", style = "font-family: 'Permanent Marker'; font-size: 100%;"),
                subtitle = "5th Grade Math Performance",
                icon = icon("chart-line"),
                color = "purple"
            )
        } else if (mach_ind > (d_mn_mach + 0.01)) {
            valueBox(
                value = tags$p("Above average", style = "font-family: 'Permanent Marker'; font-size: 100%;"),
                subtitle = "5th Grade Math Performance",
                icon = icon("chart-line"),
                color = "blue"
            ) 
        } else if (mach_ind < (d_mn_mach - 0.01)) {
            valueBox(
                value = tags$p("Below average", style = "font-family: 'Permanent Marker'; font-size: 100%;"),
                subtitle = "5th Grade Math Performance", 
                icon = icon("chart-line"),
                color = "red"
            ) 
        } else {
            valueBox(
                value = tags$p("About average", style = "font-family: 'Permanent Marker'; font-size: 100%;"),
                subtitle = "5th Grade Math Performance",
                icon = icon("chart-line"),
                color = "green"
            ) 
        }
    })
}

shinyApp(ui = ui, server = server)