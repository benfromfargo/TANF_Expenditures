
### Code for checking R data against excel tables ###





check_data <- function(data) {
  data %>% 
    gather(key = "category", value = "value", -STATE, -year) %>% 
    select(STATE, category, year, value) %>%
    unite(category, category, year, sep = "_") %>% 
    spread(key = "category", value = "value")
}

props_excel <- check_data(props_vis) %>% 
  write_csv("Checks/props_excel.csv")
avg_props_excel <- check_data(avg_props_vis) %>% 
  write_csv("Checks/avg_props_excel.csv")
props_avg_excel <- check_data(props_avg_vis) %>% 
  write_csv("Checks/props_avg_excel.csv")


### NA count checks ###

count_na <- function(data) {
  na_count <-sapply(data[, 3:12], function(y) sum(is.na(y)))
  na_count <- data.frame(na_count)
  
  na_count <- aggregate(data, list(data[, 2]), function(y) sum(is.na(y))) %>% 
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

saveWorkbook(wb2, "Checks/TANF_na_check.xlsx")

# Boxplots are dropping a couple values due to being non-finite, but these checks show 
# no infinite values. 
check_finite <- function (data) {
  sapply(data, function (y) sum(is.infinite(y)))
}
check_finite(props)
check_finite(avg_props)
check_finite(props_avg)
