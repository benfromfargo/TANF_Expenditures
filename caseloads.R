library(tidyverse)
library(readxl)

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
  mutate(avg_raw = mean(value)) %>% 

# Average Monthly TANF Families by Family Type
case_raw %>% 
  filter(State == "Alabama") %>% 
  ggplot(aes(year, avg_raw, fill = category)) +
  geom_bar(stat = "identity") +
  labs(y = "Families", title = "Average Monthly TANF Families by Family Type", 
       fill = "Family Type") +
  scale_fill_discrete(labels = c("Child-only", "One-parent", "Two-parents")) +
  theme_classic() +
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(expand = c(0,0))

# Percentage of Child-Only Families in the Caseload in the Average Month
case_raw %>% 
  filter(category == "0.families") %>% 
  filter(State == "Alabama") %>% 
  ggplot(aes(year, avg_prop)) +
  geom_line(group = 1) +
  labs(y = "Percentage of child-only families in the caseload", 
       title = "Percentage of Child-Only Families in the Caseload in the Average Month") +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(axis.title.x = element_blank())
  
  

  
  
  
