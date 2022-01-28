if (!require(tidyverse)) install.packages("tidyverse")
if (!require(rvest)) install.packages("rvest")
if (!require(janitor)) install.packages("janitor")
library(tidyverse)
library(rvest)
library(janitor)

fout <- "holocaust_tables.rds"

if (!file.exists(fout)) {
  holocaust <- read_html("http://70.auschwitz.org/index.php?option=com_content&view=article&id=89&Itemid=173&lang=en")
  
  holocaust_tables <- holocaust %>% 
    html_elements("table") %>% 
    html_table()
  
  holocaust_tables <- map(
    seq_along(holocaust_tables),
    function(t) {
      colnames(holocaust_tables[[t]]) <- make_clean_names(as.character(holocaust_tables[[t]][1,]))
      holocaust_tables[[t]] <- holocaust_tables[[t]] %>% 
        filter(row_number() > 1)
    }
  )
  
  # tidy deportees/victims ----
  
  holocaust_tables[[1]] <- holocaust_tables[[1]] %>% 
    select(-matches("perc")) %>% 
    filter(nationality_category != "Total") %>% 
    separate(number_of_deportees, c("number_of_deportees", "number_of_deportees_unit"),
             sep = " ") %>% 
    separate(number_of_victims, c("number_of_victims", "number_of_victims_unit"),
             sep = " ") %>% 
    mutate(
      number_of_deportees = case_when(
        number_of_deportees_unit == "million" ~ as.numeric(number_of_deportees) * 10^6,
        number_of_deportees_unit == "thousand" ~ as.numeric(number_of_deportees) * 10^3
      ),
      
      number_of_victims = gsub("[^0-9]", "", number_of_victims),
      number_of_victims = case_when(
        number_of_victims_unit == "million" ~ as.numeric(number_of_victims) * 10^6,
        number_of_victims_unit == "thousand" ~ as.numeric(number_of_victims) * 10^3,
        # the table contains 70-thousand for Poles
        TRUE ~ as.numeric(number_of_victims) * 10^3
      )
    ) %>% 
    select(-matches("unit"))
  
  holocaust_tables[[2]] <- holocaust_tables[[2]] %>% 
    filter(country_of_origin != "Total") %>% 
    separate(number, c("number", "number_unit"),
             sep = " ") %>% 
    mutate(
      number = as.numeric(number),
      number = case_when(
        number_unit == "thousand" ~ number * 10^3,
        # Norway = 690 deported
        TRUE ~ number
      )
    ) %>% 
    select(-matches("unit"))
  
  holocaust_tables[[3]] <- holocaust_tables[[3]] %>% 
    filter(nationality != "Total") %>% 
    separate(number, c("number", "number_unit"),
             sep = " ") %>% 
    mutate(
      number = as.numeric(number),
      number = case_when(
        number_unit == "thousand" ~ number * 10^3,
        # Yugoslavia, Ukraine, Other have no units
        TRUE ~ number
      )
    ) %>% 
    select(-matches("unit"))
  
  saveRDS(holocaust_tables, fout)
} else {
  holocaust_tables <- read_rds(fout)
}

# create plots ----

# Total number of deportees and murdered in Auschwitz

holocaust_tables[[1]] <- holocaust_tables[[1]] %>% 
  mutate(
    pct_number_of_deportees = round(number_of_deportees / sum(number_of_deportees), 2),
    pct_number_of_victims = round(number_of_victims / sum(number_of_victims), 2),
    pct_number_of_victims_same = round(number_of_victims / number_of_deportees, 2)
  )

# taken from https://commons.wikimedia.org/wiki/File:Auschwitz_outerwear_distinguish_yellow_Star_of_David.jpg
cols <- c("#54746f", "#efa52d")

holocaust_tables[[1]] %>% 
  select(nationality_category, starts_with("number")) %>% 
  gather(group, value, -nationality_category) %>% 
  mutate(group =  str_to_title(str_replace_all(group, "_", " "))) %>% 
  arrange(-value) %>% 
  ggplot() +
  geom_col(aes(x = nationality_category, y = value, fill = group),
           position = "dodge2") +
  theme_minimal(base_size = 13, base_family = "Roboto Condensed") +
  theme(legend.position = "bottom",
        legend.title = element_blank()
    ) +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Nationality/Category",
    y = "No. of people",
    title = "Total number of deportees and murdered in Auschwitz"
  )

# Percentage of the total number of deportees

holocaust_tables[[1]] %>% 
  select(nationality_category, starts_with("pct")) %>% 
  select(-ends_with("same")) %>% 
  gather(group, value, -nationality_category) %>% 
  mutate(
    group =  str_to_title(str_replace_all(group, "_", " ")),
    group = str_replace(group, "Pct Number", "Percentage")
  ) %>% 
  arrange(-value) %>% 
  ggplot() +
  geom_col(aes(x = nationality_category, y = value, fill = group),
           position = "dodge2") +
  theme_minimal(base_size = 13, base_family = "Roboto Condensed") +
  theme(legend.position = "bottom",
        legend.title = element_blank()
  ) +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Nationality/Category",
    y = "Share of people",
    title = "Percentage of deportees and murdered in Auschwitz"
  )

# Percentage of murdered within the category/nationality

holocaust_tables[[1]] %>% 
  select(nationality_category, ends_with("same")) %>% 
  gather(group, value, -nationality_category) %>% 
  arrange(-value) %>% 
  ggplot() +
  geom_col(aes(x = nationality_category, y = value, fill = group),
           position = "dodge2") +
  theme_minimal(base_size = 13, base_family = "Roboto Condensed") +
  theme(legend.position = "none",
        legend.title = element_blank()
  ) +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Nationality/Category",
    y = "Share of people",
    title = "Percentage of murdered within the category/nationality in Auschwitz"
  )

# Jews deported to Auschwitz according to their category/nationality of origin

holocaust_tables[[2]] %>% 
  mutate(
    country_of_origin = str_replace(country_of_origin, "\\s+\\(.*", ""),
    country_of_origin = case_when(
      country_of_origin == "The Protectorate of Bohemia and Moravia" ~ "Bohemia and Moravia",
      country_of_origin == "Concentration camps and other centers" ~ "Concentration camps",
      TRUE ~ country_of_origin
    )
  ) %>% 
  ggplot() +
  geom_col(aes(x = country_of_origin, y = number, fill = "group"),
           position = "dodge2") +
  theme_minimal(base_size = 13, base_family = "Roboto Condensed") +
  theme(legend.position = "none",
        legend.title = element_blank()
  ) +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Nationality/Category",
    y = "No. of people",
    title = "Jews deported to Auschwitz according to their category/nationality"
  ) +
  coord_flip()

# Prisoners of Auschwitz concentration camp

holocaust_tables[[3]] %>% 
  ggplot() +
  geom_col(aes(x = nationality, y = number, fill = "group"),
           position = "dodge2") +
  theme_minimal(base_size = 13, base_family = "Roboto Condensed") +
  theme(legend.position = "none",
        legend.title = element_blank()
  ) +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Nationality/Category",
    y = "No. of people",
    title = "Prisoners of Auschwitz concentration camp according to their\ncategory/nationality"
  ) +
  coord_flip()
