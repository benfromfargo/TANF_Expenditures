# Cleaning ####
library(tidyverse)
library(readxl)
library(ggrepel)
library(stargazer)

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
  separate(category, c("year", "category"), "_") %>% 
  group_by(year, category) %>% 
  mutate(us_total = sum(value)) %>% 
  ungroup()

case_raw %>% 
  filter(State == "Alabama") %>% 
  filter(year != "2014") %>% 
  filter(category == "families" | category == "0.families") %>% 
  ggplot(aes(year, us_total, group = category, color = category)) +
  geom_line() +
  labs(y = "Number of Families", title = "Figure 1 - Number of TANF Families in the Average Month (CY 1998-2013)") +
  scale_colour_manual(labels = c("Child-only families", "All families"), name = element_blank(), 
                        guide = guide_legend(reverse = TRUE), 
                      values = c("grey", "black")) +  
  scale_x_discrete(breaks = c("2000", "2005", "2010")) + 
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 3100000)) 


case_totals <- case_raw %>% 
  filter(category == "families") %>% 
  select(State, year, total = value)

case_raw <- left_join(case_raw, case_totals, c("State", "year"))
anti_join(case_raw, case_totals, c("State", "year"))

case_raw <- case_raw %>% 
  mutate(prop = value/total) %>% 
  group_by(year, category) %>% 
  mutate(avg_prop = mean(prop)) %>% 
  mutate(avg_raw = mean(value)) 

# Total and child only caseloads 

case_raw %>% 
  filter(State == "Alabama") %>% 
  filter(category == "0.families") %>% 
  ggplot() +
  geom_line(aes(year, avg_raw)) +
  geom_line(aes(year, total))


# Average Monthly TANF Families by Family Type ####
case_raw %>% 
  filter(State == "Alabama") %>% 
  ggplot(aes(year, avg_raw, fill = category)) +
  geom_bar(stat = "identity") +
  labs(y = "Families", title = "Average Monthly TANF Families by Family Type", 
       fill = "Family Type") +
  scale_fill_discrete(labels = c("Child-only", "One-parent", "Two-parents")) +
  scale_x_discrete(breaks = c("2000", "2005", "2010")) +
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(expand = c(0,0))
ggsave("Figures and Tables/Caseload_1.pdf", height = 5, width = 6.5, units = "in")


# Percentage of Child-Only Families in the Caseload in the Average Month ####
case_raw %>% 
  filter(category == "0.families") %>% 
  filter(State == "Alabama") %>% 
  ggplot(aes(year, avg_prop)) +
  geom_line(group = 1) +
  labs(y = "Percentage of child-only families in the U.S. caseload", 
       title = "Percentage of Child-Only Families in the U.S. Caseload\nin the Average Month") +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(axis.title.x = element_blank())
ggsave("Figures and Tables/Caseload_2.pdf", height = 5, width = 6.5, units = "in")

# Percentage of Child-Only Families in a State's Caseload in the Average Month ####
s_abbvs <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", 
             "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
             "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", 
             "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
             "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
             "WY")
case_raw %>% 
  filter(category =="0.families") %>%
  arrange(desc(year)) %>% 
  mutate(state_id = rep_len(s_abbvs, length.out = 51)) %>% 
  mutate(lql= quantile(prop, probs = .25, na.rm = TRUE)) %>% 
  mutate(hql = quantile(prop, probs = .75, na.rm = TRUE)) %>% 
  mutate(iqr = hql - lql) %>% 
  mutate(outlier = ifelse((prop < (lql - (iqr * 1.5))) | (prop > (hql + (iqr * 1.5))), state_id, NA)) %>%
  mutate(outlier2 = ifelse((prop < (lql - (iqr * 1.5))) | (prop > (hql + (iqr * 1.5))), prop, NA)) %>% 
  ggplot() +
    geom_boxplot(aes(year, prop, group = year), outlier.shape = NA) +
    scale_x_discrete(name = "", breaks = c("2000", "2005", "2010")) +
    theme(axis.title = element_blank(), legend.position = "none") +
    geom_point(aes(year, outlier2)) +
    geom_text_repel(aes(year, outlier2, label = outlier), size = 2, nudge_x = .15) +
    scale_y_continuous(labels = scales::percent) +
  labs(title = "Percentage of Child-Only Families in States' Caseloads\nin the Average Month")
ggsave("Figures and Tables/Caseload_3.pdf", height = 5, width = 6.5, units = "in")

# Standard Deviations of the Proportion of Child Only Families in\nStates' Caseloads in the Average Month ####
sd_child_only <- case_raw %>% 
  ungroup() %>% 
  filter(category == "0.families") %>% 
  select(State, year, prop, category) %>% 
  spread(year, prop) %>%
  select(-category, -State) %>% 
  map_df(sd, na.rm = TRUE) %>% 
  gather(year, value)

sd_child_only %>% 
  ggplot(aes(year, value)) +
  geom_line(group = 1) +
  theme(axis.title.x = element_blank()) +
  labs(y = "standard deviation", title = "Standard Deviations of the Proportion of Child Only Families in\nStates' Caseloads in the Average Month")
ggsave("Figures and Tables/Caseload_4.pdf", height = 5, width = 6.5, units = "in")

  
# Linear regression ####
child_only <- case_raw %>%
  ungroup() %>% 
  filter(category == "0.families") %>% 
  select(State, year, prop, category) %>% 
  spread(year, prop) %>% 
  select(State, prop_98 = '1998', prop_2014 = '2014')

stargazer(lm(prop_2014 ~ prop_98, data = child_only),
          out = "Figures and Tables/Caseload_5.html")
# Top ten and bottom ten ####
top_ten_case <- case_raw %>% 
  filter(category == "0.families") %>% 
  filter(year == 1998) %>%
  arrange(desc(prop)) %>% 
  top_n(10, prop)

bottom_ten_case <- case_raw %>% 
  filter(category == "0.families") %>%  
  filter(year == 1998) %>%
  arrange(desc(prop)) %>% 
  top_n(-10, prop)

case_raw %>% 
  ungroup() %>% 
  filter(category == "0.families") %>% 
  filter(year == 1998 | year == 2013) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate(rank = as.factor(ifelse(State %in% top_ten_case$State, 1, 
                                 ifelse(State %in% bottom_ten_case$State, 2, 0)))) %>%
  ggplot(aes(year, prop, group = State, color = rank)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent, name = element_blank()) +
  scale_x_discrete(name = element_blank()) +
  scale_color_manual(values = c("#cccccc", "#666666", "#000000"), name = element_blank(), breaks = c(1, 2),  
                     labels = c("Ten states with the highest percentage\nof child only TANF units in 1998", "Ten states with the lowest percentage\nof child only TANF units in 1998")) +
  labs(title = "Child only TANF Units as a Percentage\nof Total TANF Caseloads in 1998 and 2013")
