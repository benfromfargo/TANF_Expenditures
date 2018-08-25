# Front Matter #####
library(tidyverse)
library(openxlsx)
library(readxl)
library(zoo)
library(plm)

#Remove scientific notation 
options(scipen = 999)


# Clean Expenditure Data ####
raw_data <- read.xlsx("Input Data/TANF_expenditures.xlsx", sheet = "Raw Values")
raw_data <- gather(raw_data, key = "category", value = "value", -STATE) %>%
  arrange(STATE) %>% 
  separate(category, into = c("category", "year"), sep = "_") %>% 
  group_by(STATE, year) %>% 
  mutate(total_state = sum(value)) %>% 
  ungroup()

props <- raw_data %>% 
  mutate(value = value / total_state) %>% 
  select(-total_state)

avg_props <- props %>%
  mutate(value = rollmean(value, 3, fill = NA)) %>% 
  filter(!(year %in% c("1997", "2014"))) %>% 
  mutate(value = round(value, 10)) %>% 
  mutate(value = ifelse(value > 1 | value < 0, NA, value)) %>% 
  spread(category, value)

props <- props %>%
  filter(!(year %in% c("1997", "2014"))) %>% 
  mutate(value = round(value, 10)) %>% 
  mutate(value = ifelse(value > 1 | value < 0, NA, value)) %>% 
  spread(category, value)

props_avg <- raw_data %>% 
  mutate(value = rollmean(value, 3, fill = NA)) %>% 
  filter(!(year %in% c("1997", "2014"))) %>%
  mutate(value = value/total_state) %>%
  mutate(value = round(value, 10)) %>%
  mutate(value = ifelse(value > 1 | value < 0, NA, value)) %>% 
  select(-total_state) %>% 
  spread(category, value)

# Clean Independent Variables #####
ind_data <- read_excel("Input Data/TANF_ind-variables.xlsx", sheet = "Ind. Variables - FINAL", na = "NA")
ind_data <- gather(ind_data, key = category, value = value, -STATE) %>% 
  separate(category, into = c("category", "year"), sep = " ") 

# Increase all independent variable years by 1
ind_data <- mutate(ind_data, year = as.numeric(year) + 1) %>% 
  filter(!year == 2014 & !year == 2015) %>% 
  mutate(year = as.character(year))

ind_data <- spread(ind_data, key = category, value = value)

# Bind expenditure data to independent variables
to_percent <- function(x) {
  x * 100
}

join_data <- function(x, y) {
  left_join(as.tibble(x), as.tibble(y), by = c("STATE", "year"))
}

# Final data with all variables lagged one year forward 
props_pdata <- join_data(props, ind_data)
props_pdata[, 3:12] <- sapply(props_pdata[, 3:12], to_percent)
props_pdata <- pdata.frame(props_pdata, index = c("STATE", "year"))

avg_props_pdata <- join_data(avg_props, ind_data)
avg_props_pdata[, 3:12] <- sapply(avg_props_pdata[, 3:12], to_percent)
avg_props_pdata <- pdata.frame(avg_props_pdata, index = c("STATE", "year"))

props_avg_pdata <- join_data(props_avg, ind_data)
props_avg_pdata[, 3:12] <- sapply(props_avg_pdata[, 3:12], to_percent)
props_avg_pdata <- pdata.frame(props_avg_pdata, index = c("STATE", "year"))

