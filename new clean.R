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
  mutate(value = round(value, 10)) %>%
  mutate(value = ifelse(value > 1 | value < 0, NA, value)) %>% 
  select(-total) %>% 
  spread(category, value)

# NA count checks
count_na <- function(data) {
na_count <- aggregate(data, list(data$year), function(y) sum(is.na(y))) %>% 
    select(-STATE, -year) %>% 
    rename(year = `Group.1`)
}


na_count_props <- count_na(props)
na_count_avg_props <- count_na(avg_props)
na_count_props_avg <- count_na(props_avg)

wb2 <- createWorkbook()
addWorksheet(wb2, "props")
addWorksheet(wb2, "avg_props")
addWorksheet(wb2, "props_avg")

writeData(wb2, "props", na_count_props )
writeData(wb2, "avg_props", na_count_avg_props )
writeData(wb2, "props_avg", na_count_props_avg )

saveWorkbook(wb2, "Checks/TANF_na_check2.xlsx")


