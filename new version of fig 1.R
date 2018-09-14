files <- list.files("Caseloads/")
files <- str_c("Caseloads/", files)

files_work <- list.files("Workers/")
files_work <- str_c("Workers/", files_work)


case_raw <- as.data.frame(map(files, ReadR)) %>% 
  select(-starts_with("State.")) %>% 
  gather(key = "category", value = "value", -State) %>% 
  mutate(category = str_replace(category, "X", "")) %>% 
  mutate(value = floor(value)) %>% 
  separate(category, c("year", "category"), "_") %>% 
  filter(State != "us_total") %>% 
  group_by(year) %>% 
  mutate(ann_value = sum(value)) %>% 
  ungroup() %>% 
  filter(category == "families") %>% 
  rename(state = State)

workers_raw <- as.data.frame(map(files_work, ReadR)) %>% 
  select(-starts_with("state.")) %>% 
  gather("category", "value", -state) %>% 
  separate(category, c("category", "year"), "_") %>%
  group_by(year) %>% 
  mutate(ann_value = sum(value)) %>% 
  ungroup()

case_raw <- rbind(case_raw, workers_raw)

case_raw %>% 
  ggplot(aes(year, ann_value, color = category, group = category)) +
  geom_line() +
  labs(title = "Figure 1: Families Receiving TANF Assistance in an Average Month",
       subtitle = "1998 - 2017",
       caption = "In millions of families", 
       x = NULL, 
       y = NULL) +
  scale_colour_manual(guide = FALSE, 
                      values = c("#000000", "#000000")) +  
  scale_x_discrete(breaks = c("1998", "2003", "2008", "2013", "2017")) + 
  scale_y_continuous(breaks = seq(1000000, 6000000, 1000000),
                     labels = c("1", "2", "3", "4", "5", "6")) +
  theme(text = element_text(family = "Times New Roman")) +
  annotate("text", x = "2010", y = 4100000, 
           label = "All families", size = 3, 
           family = "Times New Roman",
           hjust = 0) +
  annotate("text", x = "2010", y = 1400000, 
           label = "Families with a work-eligible individual",
           size = 3, 
           family = "Times New Roman",
           hjust = 0)
