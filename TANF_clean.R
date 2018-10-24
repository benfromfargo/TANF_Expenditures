# Front Matter #####
library(tidyverse)
library(openxlsx)
library(readxl)
library(zoo)
library(readstata13)

#Remove scientific notation 
options(scipen = 999)


# Clean Expenditure Data ####
raw_data <- read.xlsx("Input Data/TANF_expenditures.xlsx", sheet = "Raw Values")
raw_data <- gather(raw_data, key = "category", value = "value", -STATE) %>%
  arrange(STATE) %>% 
  separate(category, into = c("category", "year"), sep = "_") %>% 
  filter(category != "total") %>% 
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

avg_props_pdata <- join_data(avg_props, ind_data)
avg_props_pdata[, 3:12] <- sapply(avg_props_pdata[, 3:12], to_percent)

props_avg_pdata <- join_data(props_avg, ind_data)
props_avg_pdata[, 3:12] <- sapply(props_avg_pdata[, 3:12], to_percent)

# New addition (10/20/18) 

panel_dat <- raw_data %>% 
  mutate(value = rollmean(value, 3, fill = NA)) %>% 
  filter(!(year %in% c("1997", "2014"))) %>%
  mutate(value = round(value, 10)) %>%
  mutate(value = ifelse(value < 0, NA,
                        ifelse(value/total_state > 1, NA, value))) %>% 
  select(-total_state) %>% 
  spread(category, value)

panel_dat <- left_join(panel_dat, ind_data, by = c("STATE", "year"))
anti_join(panel_dat, ind_data, by = c("STATE", "year"))

suppressWarnings(csp <- read_csv("Input Data/correlatesofstatepolicyprojectv2_1.csv", 
                col_types = cols_only(state = col_character(),
                                      year = col_integer(),
                                      ranney4_control = col_double())))
csp <- csp %>% 
  select(state, year, ranney4_control) %>% 
  filter(year > 1996) %>% 
  mutate(year = year +1) %>% 
  filter(year < 2014) %>% 
  mutate(year = as.character(year)) %>% 
  filter(state != "District of Columbia") %>% 
  arrange(year) %>% 
  mutate(state_id = rep(state.abb, 16))

csp_dc <- tibble(state = rep("District of Columbia", 16),
                 year = c(1998:2013),
                 ranney4_control = NA,
                 state_id = "DC")
csp <- rbind(csp, csp_dc)

test <- read.dta13("Input Data/caughey_warshaw_summary.dta") %>% 
  as.tibble() %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  mutate(stpo = as.character(stpo)) %>% 
  rename(state_id = stpo) %>% 
  filter(year > 1996) %>% 
  mutate(year = year + 1) %>% #IV t - 1 == DV t
  filter(year < 2014) %>% 
  select(1:8) %>% 
  mutate(year = as.character(year))

abbrvs <- append(state.abb, "DC", 8)

panel_dat <- panel_dat %>% 
  ungroup() %>% 
  arrange(year) %>% 
  mutate(state_id = rep_len(abbrvs, 816))

avg_props_pdata <- left_join(panel_dat, csp, by = c("year", "state_id"))
anti_join(csp, panel_dat, by = c("year", "state_id"))


           








