# Front Matter #####
library(tidyr)
library(tidyverse)
library(ggrepel)
library(pBrackets)
library(gtable)
library(zoo)
library(plm)
library(stargazer)
library(openxlsx)
library(readxl)
library(stats)
library(grid)

#Remove scientific notation 
options(scipen = 999)

# Clean Expenditure Data #####
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

# Figure 1 - Number of TANF Families in the Average Month ####

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
  filter(State == "us_total") %>% 
  filter(year != "2014")

case_raw %>% 
  filter(category == "families" | category == "0.families") %>% 
  ggplot(aes(year, value, group = category, color = category)) +
  geom_line() +
  labs(title = "Figure 1 - Families Receiving TANF Assistance in an Average Month",
       subtitle = "CY 1998 - 2013",
       caption = "In millions of families") +
  scale_colour_manual(labels = c("Child-only families", "All families"), 
                      name = element_blank(), 
                      guide = guide_legend(reverse = TRUE), 
                      values = c("#666666", "#000000")) +  
  scale_x_discrete(breaks = c("2000", "2005", "2010")) + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  scale_y_continuous(breaks = c(1000000, 1500000, 2000000, 2500000, 3000000),
                     labels = c("1", "1.5", "2", "2.5", "3"))
ggsave("Figures and Tables/Figure1.pdf", height = 5, width = 6.5, units = "in")  

# Figure 2 - Aggregate Reported TANF Expenditures on Basic Assistance ####
raw_data %>%
  group_by(category, year) %>% 
  filter(!(year %in% c("1997", "2014"))) %>% 
  mutate(category_total = sum(value)) %>% 
  ungroup() %>% 
  filter(category == "ba") %>% 
  ggplot(aes(year, category_total, group = category)) +
  geom_line() +
  labs(title = "Figure 2 - Aggregate Reported TANF Expenditures on Basic Assistance", 
       subtitle = "FY 1998 - 2013",
       caption = "In billions of dollars, not adjusted for inflation") +
  scale_x_discrete(breaks = c("2000", "2005", "2010")) + 
  scale_y_continuous(breaks = seq(8000000000, 14000000000, 2000000000), 
                     limits = c(8000000000, 14000000000),
                     labels = c("$8", "$10", "$12", "$14")) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank())
ggsave("Figures and Tables/Figure2.pdf", height = 5, width = 6.5, units = "in")  

# Figure 3 Annual Mean Expenditures ####

ann_means_vis <- spread(ann_means, key = "category", value = "value")

ann_means_vis <- ann_means_vis %>%
  mutate(service = (ann_means_vis$cc + ann_means_vis$pregnancy +
                      ann_means_vis$shortben + ann_means_vis$tax +
                      ann_means_vis$work)) %>% 
  mutate(other2 = (ann_means_vis$admin + ann_means_vis$other +
                     ann_means_vis$prior + ann_means_vis$ssbg)) %>% 
  select(year, ba, service, other = other2)

ann_means_vis <- gather(ann_means_vis, key = "category", value = "value", -year)

ann_means_vis <- ann_means_vis %>% 
  mutate(category = factor(ann_means_vis$category, levels = c("other", "service", "ba")))

ggplot(ann_means_vis, aes(year, value, fill = category)) +
  geom_col() +
  scale_x_discrete(name = "", 
                   breaks = c("2000", "2005", "2010")) +
  scale_y_continuous(name = "", 
                     labels = scales::percent,
                     expand = c(0,.02)) +
  scale_fill_manual(values = c("#cccccc", "#666666", "#000000"),
                    labels = c("Other", 
                               "Aid that is not basic assistance", 
                               "Basic assistance"),
                    name = "Type of Spending") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.caption = element_text(hjust = 0,
                                    size = 6)) +
  labs(title = "Figure 3 - Mean TANF Spending as Percentage of Total Expenditures",
       subtitle = "FY 1998 - 2013",
       caption = "\"Other\" includes administration and systems, expenditures under prior law, other non-assistance, and social services block grant categories;
\"Aid that is not basic assistance\" includes child care, marriage and pregnancy programs, diversion benefits, refundable tax credits, 
and work-related activities and supports categories. The percentages in Figure 1 may not add up to 100% in a given fiscal year due to the removal of 
outlier values (i.e., proportional expenditure values that remained above 100% or below 0% after calculating moving averages). 
See Table A.2 in the Appendix for a complete list of annual mean expenditures by year and category.")
ggsave("Figures and Tables/Figure3.pdf", height = 5, width = 6.6, units = "in")  

# Figure 4 - Proportional Expenditures of Aid that is not Basic Assistance ####
x <- ann_means %>% 
  filter(category != "ba" & category != "admin" & category != "other" & category != "prior" & category != "ssbg") %>%
  mutate(label = ifelse(year == "2013", category, NA)) %>% 
  mutate(label = case_when(
    label == "cc" ~ "    Child care",
    label == "work" ~ "    Work-related\n    activities and supports", 
    label == "pregnancy" ~ "    Marriage and pregnancy programs",
    label == "tax" ~ "    Refundable tax credits", 
    label == "shortben" ~ "    Diversion benefits")) %>% 
  ggplot(aes(year, value, group = category)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent,
                     name = element_blank()) +
  scale_x_discrete(breaks = c("2000", "2005", "2010")) +
  geom_text(aes(label = label),
            na.rm = TRUE,
            hjust = 0) +
  labs(title = "Figure 4 - Proportional Expenditures of Aid that is not Basic Assistance",
       subtitle = "FY 1998 - 2013") +
  theme(plot.margin = unit(c(2,12,2,2), "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 

gt <- ggplotGrob(x)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)
ggsave("Figures and Tables/Figure4.pdf", gt, height = 5, width = 6.6, units = "in")




# Figure 5 - Basic Assistance Boxplot ####
avg_props_id <- avg_props %>% 
  arrange(desc(year)) %>% 
  mutate(state_id = rep_len(c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", 
                              "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
                              "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", 
                              "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
                              "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
                              "WY"),
                            length.out = 816)) %>%
  select(state_id, STATE, everything())

avg_props_id %>% 
  select(state_id:year, ba) %>%
  group_by(year) %>% 
  mutate(lql= quantile(ba, probs = .25, na.rm = TRUE)) %>% 
  mutate(hql = quantile(ba, probs = .75, na.rm = TRUE)) %>% 
  mutate(iqr = hql - lql) %>% 
  mutate(outlier = ifelse((ba < (lql - (iqr * 1.5))) | (ba > (hql + (iqr * 1.5))), state_id, NA)) %>% 
  mutate(outlier2 = ifelse((ba < (lql - (iqr * 1.5))) | (ba > (hql + (iqr * 1.5))), ba, NA)) %>% 
  ggplot() +
  geom_boxplot(aes(year, ba, group = year)) +
  scale_x_discrete(name = "", 
                   breaks = c("2000", "2005", "2010")) +
  theme(axis.title = element_blank(), 
        legend.position = "none") +
  geom_point(aes(year, outlier2)) +
  geom_text_repel(aes(year, outlier2, 
                      label = outlier), 
                  size = 2, 
                  nudge_x = .15) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Figure 5 - Basic Assistance Spending as a Percentage of Total Expenditures",
       subtitle = "FY 1998 - 2013")
ggsave("Figures and Tables/Figure5.pdf", height = 5, width = 6.6, units = "in")

# Note: The "missing value" warnings in the boxplot code stem from outlier labelling.

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

# Table 1 - Regression output ####

# Model 1 : Without caseload, pcpi_regional, and unemployment - no time effects
p1 <- plm(ba ~ african_americans + hispanics + fiscal_stability + liberalism + wpr,
          data = avg_props_pdata,
          model = "within", 
          effect = "individual")

# Model 2 : Without pcpi_regional and unemployment - no time effects
p2 <- plm(ba ~ african_americans + hispanics + fiscal_stability + caseload + liberalism + wpr,
          data = avg_props_pdata,
          model = "within", 
          effect = "individual")

# Model 3 : All variables - no time effects 
p3 <- plm(ba ~ african_americans + hispanics + fiscal_stability + caseload + liberalism + wpr + 
            unemployment + pcpi_regional,
          data = avg_props_pdata, 
          model = "within", 
          effect = "individual")

# Model 4 : All variables - time effects 
p4 <- plm(ba ~ factor(year) + african_americans + hispanics + fiscal_stability + caseload + liberalism + wpr + 
            unemployment + pcpi_regional,
          data = avg_props_pdata, 
          model = "within", 
          effect = "individual")

stargazer(p1, p2, p3, p4,
          title = "Table 1 - Regression Output",
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          covariate.labels = c(NA, NA, NA, NA, NA, NA, NA, "pcpi regional (thousands)"),
          dep.var.labels = "Basic Assistance Expenditures as a Percentage of Total TANF Expenditures",
          omit = "year",
          omit.labels = c("Time Fixed Effects"),
          notes.align = "r",
          model.numbers = FALSE,
          initial.zero = FALSE,
          out = "Figures and Tables/Table1.html")

# Figure 7 - Coefficients of Time Fixed Effects from Model 4 ####
time_effects <- data.frame(summary(p4)["coefficients"])
time_effects <- rownames_to_column(time_effects, "year") 

time_effects <- time_effects %>% 
  filter((str_detect(time_effects$year, "factor"))) %>% 
  mutate(year = 1999:2013)

time_effects %>% 
  ggplot() +
  geom_point(aes(x = year, y = coefficients.Estimate)) +
  geom_errorbar(aes(x = year,
                    ymin = coefficients.Estimate - 1.96*coefficients.Std..Error,
                    ymax = coefficients.Estimate + 1.96*coefficients.Std..Error)) +
  labs(title = "Figure 7 - Coefficients of Time Fixed Effects from Model 4",
       subtitle = "FY 1999 - 2013",
       caption = "Error bars represent 95% confidence intervals") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
ggsave("Figures and Tables/Figure7.pdf", height = 5, width = 6.5, units = "in")


# Tables A.2 and A.3 - Annual Mean and Median Tables ####
aggregate(avg_props[, 3:12], list(avg_props$year), mean, na.rm = TRUE) %>% 
  rename(year = `Group.1`) %>% 
  write_csv("Appendix Tables/TableA.2.csv")

aggregate(avg_props[, 3:12], list(avg_props$year), median, na.rm = TRUE) %>%
  rename(year = `Group.1`) %>%
  write_csv("Appendix Tables/TableA.3.csv")
# Table A.4 - Regression output of three cleaning methods ####

p_regress <- function(data) {
  plm(ba ~ factor(year) + african_americans + hispanics + fiscal_stability + caseload + 
        liberalism + wpr + unemployment + pcpi_regional,
      data = data, 
      model = "within", 
      effect = "individual")
}
fixed_props <- p_regress(props_pdata)
fixed_avg_props <- p_regress(avg_props_pdata)
fixed_props_avg <- p_regress(props_avg_pdata)

stargazer(fixed_props, fixed_avg_props, fixed_props_avg,
          column.labels = c("Raw Proportions", "Moving Averages of Proportions", "Proportions of Moving Averages"),
          title = "Table A.4 - Regression Output of Three Data Cleaning Methods", 
          covariate.labels = c(NA, NA, NA, NA, NA, NA, NA, "pcpi regional (thousands)"),
          omit = "year",
          omit.labels = c("Time Fixed Effects"),
          notes.align = "r",
          initial.zero = FALSE,
          dep.var.labels = "Basic Assistance Expenditures as a Percentage of Total Expenditures",
          out = "Appendix Tables/TableA.4.html")












# Figure 6 - Top and bottom ten ####
top_ten_98 <- avg_props_id %>% 
  filter(year == 1998) %>%
  filter(!is.na(ba)) %>%
  top_n(10, ba)

bottom_ten_98 <- avg_props_id %>% 
  filter(year == 1998) %>%
  filter(!is.na(ba)) %>% 
  top_n(-10, ba)

top_ten_13 <- avg_props_id %>% 
  filter(year == 2013) %>%
  filter(!is.na(ba)) %>%
  top_n(10, ba)

bottom_ten_13 <- avg_props_id %>% 
  filter(year == 2013) %>%
  filter(!is.na(ba)) %>%
  top_n(-10, ba)

avg_props_id %>% 
  filter(year == 1998 | year == 2013) %>%
  filter(!is.na(ba)) %>% 
  mutate(year = as.factor(year)) %>%
  mutate(rank = as.factor(ifelse(state_id %in% top_ten_98$state_id, 1, 
                                 ifelse(state_id %in% bottom_ten_98$state_id, 2, 0)))) %>%
  ggplot(aes(year, ba, group = state_id, color = rank, alpha = rank)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent, 
                     name = element_blank()) +
  scale_x_discrete(name = element_blank(),
                   expand = c(.2,.2)) +
  scale_color_manual(values = c("#cccccc", "#666666", "#000000"), 
                     name = element_blank(), 
                     breaks = c(1, 2),
                     labels = c("Ten highest spending\nstates in FY 1998", "Ten lowest spending\nstates in FY 1998")) +
  scale_alpha_manual(values = c(.4, .8, .8),
                     guide = "none") +
  labs(title = "Figure 6 - Basic Assistance Spending as a Percentage of Total Spending",
       subtitle = "FY 1998 and 2013",
       caption = "South Carolina and Tennessee removed due to negative reported basic assistance expenditures in FY 1998. See appendix.") +
  theme(plot.caption=element_text(size=6))
ggsave("Figures and Tables/Figure6.pdf", height = 5, width = 6.6, units = "in")


# States in top ten in 13 and 98
ifelse(top_ten_13$STATE %in% top_ten_98$STATE, top_ten_13$STATE, NA) 

# States in bottom ten in 13 and 98
ifelse(bottom_ten_13$STATE %in% bottom_ten_98$STATE, bottom_ten_13$STATE, NA) 

# States in top ten in 98 and bottom ten in 13
ifelse(bottom_ten_13$STATE %in% top_ten_98$STATE, bottom_ten_13$STATE, NA)

# States in bottom ten in 98 and top ten in 13 
ifelse(top_ten_13$STATE %in% bottom_ten_98$STATE, top_ten_13$STATE, NA)


# Checks ####
check_data <- function(data) {
  data %>% 
    gather(key = "category", value = "value", -STATE, -year) %>% 
    select(STATE, category, year, value) %>%
    unite(category, category, year, sep = "_") %>% 
    spread(key = "category", value = "value")
}

check_data(props) %>% 
  write_csv("Checks/props.csv")
check_data(avg_props) %>% 
  write_csv("Checks/avg_props.csv")
check_data(props_avg) %>% 
  write_csv("Checks/props_avg.csv")

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

writeData(wb2, "props", na_count_props)
writeData(wb2, "avg_props", na_count_avg_props)
writeData(wb2, "props_avg", na_count_props_avg)

saveWorkbook(wb2, "Checks/TANF_na_check.xlsx")











  


avg_props_full <- left_join(avg_props_id, ind_data, by = c("STATE", "year"))

avg_props_full %>% 
  ggplot(aes(african_americans, liberalism, size = ba)) +
  geom_point() +
  geom_text(aes(african_americans, liberalism,
                      label = ifelse(state_id %in% top_ten_13$state_id | state_id %in% bottom_ten_13$state_id,
                                     state_id,
                                     NA)),
                  color = ifelse(filter(avg_props_full, year == 2013)$state_id %in% top_ten_13$state_id,"black",
                                 ifelse(filter(avg_props_full, year == 2013)$state_id %in% bottom_ten_13$state_id, "gray",
                                 "black")),
                  size = 3,
                  nudge_x = 2,
                  nudge_y = 2)







