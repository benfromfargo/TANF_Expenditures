raw_data <- read.xlsx("Input Data/TANF_expenditures.xlsx", sheet = "Raw Values")

raw_data <- gather(raw_data, key = "category", value = "value", -STATE) %>% 
  arrange(STATE) %>% 
  separate(category, into = c("category", "year"), sep = "_")

raw_totals <- raw_data %>% 
  filter(category == "total") %>% 
  select(STATE, year, total = value)

raw_data <- raw_data %>% 
  filter(category != "total")

raw_data <- raw_data %>% 
  left_join(raw_totals, raw_data, by = c("STATE", "year"))
anti_join(raw_totals, raw_data, by = c("STATE", "year"))

props <- raw_data %>% 
  mutate(value = value / total) %>% 
  select(-total)

avg_props <- data_frame(STATE = props$STATE, category = props$category, 
                           year = props$year, value = rollmean(props[, 4], 3, fill = NA)) %>% 
  filter(!(year %in% c("1997", "2014"))) %>% 
  mutate(value = round(value, 10)) %>% 
  mutate(value = ifelse(value > 1 | value < 0, NA, value)) %>% 
  spread(category, value)

props <- props %>%
  filter(!(year %in% c("1997", "2014"))) %>% 
  mutate(value = round(value, 10)) %>% 
  mutate(value = ifelse(value > 1 | value < 0, NA, value)) %>% 
  spread(category, value)

props_avg <- data_frame(STATE = raw_data$STATE, category = raw_data$category, 
                        year = raw_data$year, value = rollmean(raw_data[, 4], 3, fill = NA),
                        total = raw_data$total)
props_avg <- props_avg %>% 
  filter(!(year %in% c("1997", "2014"))) %>%
  mutate(value = value/total) %>%
  mutate(value = ifelse(value > 1 | value < 0, NA, value)) %>% 
  select(-total) %>% 
  spread(category, value)



props <- spread(props, "category", value = "value")
bom <- props %>% 
  mutate(check = admin + ba + cc + other 
         + pregnancy + prior + shortben + ssbg + tax + work) %>% 
  filter(check != near(1))



