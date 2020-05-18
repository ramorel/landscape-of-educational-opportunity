library(tidyverse)
library(dbplyr)
library(educationdata)
library(ggminthemes)
library(glue)
library(ggtext)
library(tigris)

path <- "/Users/rap168/ccd_2001-2016.sqlite"

ccd <- DBI::dbConnect(RSQLite::SQLite(), path)

DBI::dbListTables(ccd)

# Table of districts leaid, name, fips, and year
dir_dat <- tbl(ccd, "ccd_district_dir") %>% 
  filter(year %in% 2010:2016, number_of_schools > 1) %>% 
  select(year:fips) %>% 
  collect() %>% 
  mutate(lea_name = str_to_title(lea_name),
         leaid = as.numeric(leaid))

yr <- 2016
st <- "Indiana"
fipscd <- fips_codes %>% filter(state_name == st) %>% pull(state_code) %>% unique() %>% as.numeric()
st_abbr <- fips_codes %>% filter(state_name == st) %>% pull(state) %>% unique()

st_dir <- tbl(ccd, "ccd_school_dir") %>% 
  filter(year == yr,
         fips == st,
         school_level %in% c("Primary")) %>% 
  pull(ncessch) %>% 
  as.numeric()

pov_dat <- get_education_data(level = "school-districts",
                              source = "saipe",
                              filters = list(year = yr,
                                             fips = fipscd)) %>% 
  mutate(leaid = as.numeric(leaid))

# Get ids for elementary schools
st_dir <- tbl(ccd, "ccd_school_dir") %>% 
  filter(year == yr,
         fips == st,
         school_level %in% c("Primary")) %>% 
  pull(ncessch) %>% 
  as.numeric()

# Get enrollment data for all elementary schools in the state
st_dat <- tbl(ccd, "ccd_school_enroll") %>% 
  mutate(leaid = as.numeric(leaid),
         ncessch = as.numeric(ncessch)) %>% 
  filter(year == yr,
         fips == st,
         !is.na(enrollment),
         !race %in% c("Total", "Two or more races", "Unknown"),
         ncessch %in% st_dir) %>%
  select(ncessch, leaid, race, enrollment) %>% 
  collect()

# Ids for districts with only one school--need to exclude these
districts_only_1_school <- st_dat %>% 
  select(ncessch, leaid) %>% 
  distinct() %>% 
  count(leaid) %>% 
  filter(n < 2) %>% 
  pull(leaid)

# Get achievement data for 5th grade students in state
st_mach <- read_csv("seda_math.csv") %>% 
  mutate(leaidC = as.numeric(leaidC)) %>% 
  filter(stateabb == st_abbr,
         year == yr,
         !leaidC %in% districts_only_1_school)

# Districts to choose from
dist_list <- dir_dat %>% filter(fips == st, !leaid %in% districts_only_1_school) %>% pull(lea_name)
dist_id_list <- dir_dat %>% filter(fips == st, !leaid %in% districts_only_1_school) %>% pull(leaid)

# District to choose 
district <- sample(dist_id_list, 1)

abbrs <- paste(c("Isd", "Sd", "Cusd", "Esd", "Ccsd", "Cud"), collapse = "|")
d_nm <- dir_dat %>% 
  filter(year == yr,
         leaid == district) %>%
  pull(lea_name) %>% 
  unique() %>% 
  {gsub(abbrs, "\\U\\1", perl = T, .)}


sch_dat <- tbl(ccd, "ccd_school_enroll") %>% 
  mutate(leaid = as.numeric(leaid),
         ncessch = as.numeric(ncessch)) %>% 
  filter(year == yr,
         leaid == district,
         !is.na(enrollment),
         !race %in% c("Total", "Two or more races", "Unknown"),
         ncessch %in% st_dir) %>% 
  select(year:ncessch, fips, leaid, race, enrollment) %>% 
  collect()

pov_dat <- pov_dat %>%
  select(leaid, est_population_5_17_poverty_pct) %>% 
  filter(!leaid %in% districts_only_1_school)

elem_geo <- try(school_districts(st, type = "elementary", year = yr, class = "sf"))
geo_dat <- school_districts(st, year = yr, class = "sf")

# Some states don't have elementary districts
elem_test <- ifelse(is.null(nrow(elem_geo)), FALSE, nrow(elem_geo) != 0)

if (elem_test & nrow(geo_dat) != 0) {
  geo_dat <- rbind_tigris(geo_dat, elem_geo)
} else if (elem_test & nrow(geo_dat) == 0) {
  geo_dat <- elem_geo
}

if (str_detect(d_nm, "New York City")) {
  geo_dat <- geo_dat %>%
    mutate(GEOID = as.numeric(GEOID)) %>% 
    filter(NAME == "New York City Department Of Education") 
} else {
   geo_dat <- geo_dat %>% 
     mutate(GEOID = as.numeric(GEOID)) %>% 
     filter(GEOID == district)
  }

mapview(geo_dat, label = d_nm,
        legend = FALSE, homebutton = FALSE, 
        color = "midnightblue", col.regions = "cornflowerblue")

seg_dist <- st_dat %>% 
  filter(!leaid %in% districts_only_1_school) %>% 
  group_nest(leaid) %>% 
  mutate(seg_idx = map_dbl(data, ~ segregation::mutual_total(.x, "ncessch", "race", weight = "enrollment") %>% slice(2) %>% pull(est))) %>% 
  select(-data) %>% 
  mutate(scaled_seg = scale(log(seg_idx + 0.00001))[,1])


# Demographics ----
sch_dat %>% 
  group_by(leaid, race) %>% 
  summarize(total = sum(enrollment, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(prop = total/sum(total)) %>% 
  ggplot(aes(x = prop, y = factor(leaid))) +
  geom_col(aes(fill = race), width = 0.5) +
  labs(x = "", y = "",
       title = glue("Racial demographics of {d_nm}")) +
  scale_fill_viridis_d(name = "Race/Ethnicity") +
  theme_minima() +
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Racial segregation distribution graph ----
seg_ind <- seg_dist$scaled_seg[which(seg_dist$leaid == district)]
d_mn_seg <- mean(seg_dist$scaled_seg, na.rm = TRUE)

ttl <- glue("{d_nm} has {case_when(seg_ind < d_mn_seg - 0.01 ~ \"<span style=color:#377EB8;'>below average</span>\", seg_ind > d_mn_seg + 0.01 ~ \"<span style=color:#E41A1C;'>above average</span>\", TRUE ~ \"<span style=color:#4DAF4A;'>about average</span>\")} racial segregation for {st}")

ggplot() + 
  geom_density(data = seg_dist, aes(x = scaled_seg),
               fill = "cornflowerblue", alpha = 0.7) +
  geom_vline(xintercept = seg_ind, color = "midnightblue", linetype = 2) +
  geom_vline(xintercept = d_mn_seg, color = "tomato") +
  labs(x = "", y = "", 
       title = ttl) +
  theme_minima() +
  theme(plot.title = element_markdown(lineheight = 1.1))

# Math achievement distribution graph ----
mach_ind <- st_mach$mn_all[which(st_mach$leaidC == district)]
d_mn_mach <- mean(st_mach$mn_all, na.rm = TRUE)

ttl <- glue("{d_nm} has {case_when(mach_ind < d_mn_mach - 0.01 ~ \"<span style=color:#377EB8;'>below average</span>\", mach_ind > d_mn_mach + 0.01 ~ \"<span style=color:#E41A1C;'>above average</span>\", TRUE ~ \"<span style=color:#4DAF4A;'>about average</span>\")} 5th grade math achievement for {st}")

if (nrow(st_mach) != 0) {
  ggplot() + 
    geom_density(data = st_mach, aes(x = mn_all),
                 fill = "cornflowerblue", alpha = 0.7) +
    geom_vline(xintercept = mach_ind, color = "midnightblue", linetype = 2) +
    geom_vline(xintercept = d_mn_mach, color = "tomato") +
    labs(x = "", y = "", 
         title = ttl) +
    theme_minima() +
    theme(plot.title = element_markdown(lineheight = 1.1))
} else {
  ggplot() + 
    annotate(geom = "text", x = 1, y = 1, 
             label = glue("There is no math achievement data for {st} for {yr}"),
             color = "tomato") +
    labs(x = "", y = "") +
    theme_minima() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank())
}


# Math w-b achievement gap ----
d_mn_wbg <- mean(st_mach$mn_wbg, na.rm = TRUE)
wbg_ind <- st_mach$mn_wbg[which(st_mach$leaidC == district)]

ttl <- glue("{d_nm} has {case_when(wbg_ind < d_mn_wbg - 0.01 ~ \"<span style=color:#377EB8;'>a below average</span>\", wbg_ind > d_mn_wbg + 0.01 ~ \"<span style=color:#E41A1C;'>an above average</span>\", TRUE ~ \"<span style=color:#4DAF4A;'>an about average</span>\")} white-black achievement gap for {st}")

if (nrow(st_mach) == 0) { 
  ggplot() + 
    annotate(geom = "text", x = 1, y = 1, 
             label = glue("There is no math achievement data for {st} for {yr}"),
             color = "tomato") +
    labs(x = "", y = "") +
    theme_minima() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank()) 
} else if (is.na(wbg_ind)) {
  ggplot() + 
    geom_density(data = st_mach, aes(x = mn_wbg),
                 fill = "cornflowerblue", alpha = 0.7) +
    labs(x = "", y = "", 
         title = glue("There is {\"<span style=color:#E41A1C;'>no data</span>\"} for the white-Black achievement gap in {d_nm}")) +
    theme_minima() +
    theme(plot.title = element_markdown(lineheight = 1.1))
} else {
  ggplot() + 
    geom_density(data = st_mach, aes(x = mn_wbg),
                 fill = "cornflowerblue", alpha = 0.7) +
    geom_vline(xintercept = wbg_ind, color = "midnightblue", linetype = 2) +
    geom_vline(xintercept = d_mn_wbg, color = "tomato") +
    labs(x = "", y = "", 
         title = ttl) +
    theme_minima() +
    theme(plot.title = element_markdown(lineheight = 1.1))
}

# Math w-h achievement gap ----
d_mn_whg <- mean(st_mach$mn_whg, na.rm = TRUE)
whg_ind <- st_mach$mn_whg[which(st_mach$leaidC == district)]

ttl <- glue("{d_nm} has {case_when(whg_ind < d_mn_whg - 0.01 ~ \"<span style=color:#377EB8;'>a below average</span>\", whg_ind > d_mn_whg + 0.01 ~ \"<span style=color:#E41A1C;'>an above average</span>\", TRUE ~ \"<span style=color:#4DAF4A;'>an about average</span>\")} white-Hispanic achievement gap for {st}")

if (nrow(st_mach) == 0) { 
  ggplot() + 
    annotate(geom = "text", x = 1, y = 1, 
             label = glue("There is no math achievement data for {st} for {yr}"),
             color = "tomato") +
    labs(x = "", y = "") +
    theme_minima() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank()) 
} else if (is.na(whg_ind)) {
  ggplot() + 
    geom_density(data = st_mach, aes(x = mn_whg),
                 fill = "cornflowerblue", alpha = 0.7) +
    labs(x = "", y = "", 
         title = glue("There is {\"<span style=color:#E41A1C;'>no data</span>\"} for the white-Hispanic achievement gap in {d_nm}")) +
    theme_minima() +
    theme(plot.title = element_markdown(lineheight = 1.1))
} else {
  ggplot() + 
    geom_density(data = st_mach, aes(x = mn_whg),
                 fill = "cornflowerblue", alpha = 0.7) +
    geom_vline(xintercept = whg_ind, color = "midnightblue", linetype = 2) +
    geom_vline(xintercept = d_mn_whg, color = "tomato") +
    labs(x = "", y = "", 
         title = ttl) +
    theme_minima() +
    theme(plot.title = element_markdown(lineheight = 1.1))
}
  


# Segregation by poverty ----
halign <- ifelse(seg_ind > d_mn_seg,  1.05,  -0.1)

pov_ind <- pov_dat$est_population_5_17_poverty_pct[which(pov_dat$leaid == district)]


tmp <- seg_dist %>% 
  left_join(pov_dat) %>%
  mutate(focal = ifelse(leaid == district, "yes", "no")) %>% 
  mutate(d_name = d_nm)

tmp %>% 
  ggplot() +
  geom_point(aes(x= scaled_seg, y = est_population_5_17_poverty_pct, 
                 color = focal, size = focal, alpha = focal)) +
  geom_label(data = tmp %>% filter(focal == "yes"), 
             aes(x= scaled_seg, y = est_population_5_17_poverty_pct, label = d_name),
             hjust = halign) +
  scale_color_manual(values = c("grey50", "cornflowerblue"), guide = FALSE) +
  scale_size_manual(values = c(1, 3), guide = FALSE) +
  scale_alpha_manual(values = c(0.7, 1), guide = FALSE) +
  labs(x = "Segregation Index", y = "% of children in poverty") +
  theme_minima()

# Segregation by achievement ----

if (nrow(st_mach) != 0) {
  tmp <- seg_dist %>% 
    left_join(st_mach, 
              by = c("leaid" = "leaidC")) %>%
    mutate(focal = ifelse(leaid == district, "yes", "no")) %>% 
    mutate(d_name = d_nm)
  
  tmp %>% 
    ggplot() +
    geom_point(aes(x= scaled_seg, y = mn_all, 
                   color = focal, size = focal, alpha = focal)) +
    geom_label(data = tmp %>% filter(focal == "yes"), 
               aes(x= scaled_seg, y = mn_all, label = d_name),
               hjust = halign) +
    scale_color_manual(values = c("grey50", "cornflowerblue"), guide = FALSE) +
    scale_size_manual(values = c(1, 3), guide = FALSE) +
    scale_alpha_manual(values = c(0.7, 1), guide = FALSE) +
    labs(x = "Segregation Index", y = "Mean 5th grade math acievement scores") +
    theme_minima()
} else {
  ggplot() + 
    annotate(geom = "text", x = 1, y = 1, 
             label = glue("There is no math achievement data for {st} for {yr}"),
             color = "tomato") +
    labs(x = "", y = "") +
    theme_minima() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank())
}


         
         
         
  