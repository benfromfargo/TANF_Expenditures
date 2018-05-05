library(tidyverse)
library(readxl)
library(extrafont)

files <- list.files("Caseloads/")
files <- str_c("Caseloads/", files)

readR <- function(file) {
  read_xls(file)
}

case_raw <- as.data.frame(map(files, readR)) %>% 
  select(-contains("State.")) %>% 
  gather(key = "category", value = "value", -State) %>% 
  mutate(category = str_replace(category, "X", "")) %>% 
  mutate(value = floor(value)) %>% 
  separate(category, c("year", "category"), "_")

case_totals <- case_raw %>% 
  filter(category == "families") %>% 
  select(State, year, total = value)

case_raw <- left_join(case_raw, case_totals, c("State", "year"))
anti_join(case_raw, case_totals, c("State", "year"))

case_raw <- case_raw %>% 
  filter(category != "families") %>% 
  mutate(prop = value/total) %>% 
  group_by(year, category) %>% 
  mutate(avg_prop = mean(prop)) %>% 
  mutate(avg_raw = mean(value))

case_raw %>% 
  filter(State == "Alabama") %>% 
  ggplot(aes(year, avg_raw, fill = category)) +
  geom_bar(stat = "identity")

  
  
  
