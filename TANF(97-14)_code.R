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
  labs(y = "Number of Families", title = "Figure 1 - Number of TANF Families in the Average Month (CY 1998 - 2013)") +
  scale_colour_manual(labels = c("Child-only families", "All families"), name = element_blank(), 
                      guide = guide_legend(reverse = TRUE), 
                      values = c("#666666", "#000000")) +  
  scale_x_discrete(breaks = c("2000", "2005", "2010")) + 
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 3100000)) 
ggsave("Figures and Tables/Figure1.pdf", height = 5, width = 6.5, units = "in")  

# Figure 2 - Annual Mean Expenditures ####
ann_means <- aggregate(avg_props[, 3:12], list(avg_props$year), mean, na.rm = TRUE) %>% 
  rename(year = `Group.1`) 
ann_means <- gather(ann_means, key = "category", value = "value", -year)

ann_means %>%
  filter(category == "admin" | category == "ba" | category == "cc" | category == "shortben"
         | category == "prior") %>%
  mutate(category = case_when(
    category == "admin" ~ "Administration\nand Systems",
    category == "ba" ~ "Basic Assistance", 
    category == "cc" ~ "Child Care",
    category == "shortben" ~ "Diversion Benefits", 
    category == "prior" ~ "Expenditures Under\nPrior Law")) %>% 
  ggplot(aes(year, value)) +
  geom_col() +
  facet_grid(category ~.) +
  scale_x_discrete(name = "", breaks = c("2000", "2005", "2010")) +
  scale_y_continuous(name = "", labels = scales::percent, limits = c(0, .6)) +
  theme(strip.text.y = element_text(angle = 0)) +
  ggtitle("Figure 2 - Mean TANF Expenditures as a Percentage of Total\nExpenditures by Category (FY 1998 - 2013)")
ggsave("Figures and Tables/Figure2.pdf", height = 5, width = 6.5, units = "in")  

ann_means %>% 
    filter(category == "pregnancy" | category == "other" | category == "tax" 
           | category == "ssbg" | category == "work") %>%
  mutate(category = case_when(
                              category == "pregnancy" ~ "Marriage and Pregnancy\nPrograms",
                              category == "other" ~ "Other Non-Assistance", 
                              category == "tax" ~ "Refundable\nTax Credits",
                              category == "ssbg" ~ "Social Services\nBlock Grant", 
                              category == "work" ~ "Work Related Activities\nand Supports")) %>% 
  ggplot(aes(year, value)) +
  geom_col() +
  facet_grid(category ~.) +
  scale_x_discrete(name = "", breaks = c("2000", "2005", "2010")) +
  scale_y_continuous(name = "", labels = scales::percent, limits = c(0, .6)) +
  theme(strip.text.y = element_text(angle = 0)) +
  ggtitle("Figure 2 (continued) - Mean TANF Expenditures as a Percentage of Total\nExpenditures by Category (FY 1998 - 2013)")
ggsave("Figures and Tables/Figure2_continued.pdf", height = 5, width = 6.5, units = "in")

# Figure 3 - Marriage and Pregnancy Prevention Boxplot ####

# New dataframe with state abbreivations for boxplots
s_abbvs <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", 
             "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
             "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", 
             "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
             "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
             "WY")

avg_props_id <- avg_props %>% 
  arrange(desc(year)) %>% 
  mutate(state_id = rep_len(s_abbvs, length.out = 816)) %>%
  select(state_id, STATE, everything())

avg_props_id %>% 
  select(state_id:year, pregnancy) %>%
  group_by(year) %>% 
  mutate(lql= quantile(pregnancy, probs = .25, na.rm = TRUE)) %>% 
  mutate(hql = quantile(pregnancy, probs = .75, na.rm = TRUE)) %>% 
  mutate(iqr = hql - lql) %>% 
  mutate(outlier = ifelse((pregnancy < (lql - (iqr * 1.5))) | (pregnancy > (hql + (iqr * 1.5))), state_id, NA)) %>%
  mutate(outlier2 = ifelse((pregnancy < (lql - (iqr * 1.5))) | (pregnancy > (hql + (iqr * 1.5))), pregnancy, NA)) %>% 
  mutate(outlier_vis = ifelse((outlier != "AR") & (outlier != "LA") & (outlier != "NJ"), NA, state_id)) %>% 
  mutate(outlier_vis2 = ifelse((is.na(outlier_vis)), 0, 1)) %>% 
  ggplot() +
      geom_boxplot(aes(year, pregnancy, group = year), outlier.shape = NA) +
      scale_x_discrete(name = "", breaks = c("2000", "2005", "2010")) +
      theme(axis.title = element_blank(), legend.position = "none") +
      geom_point(aes(year, outlier2, colour = factor(outlier_vis2))) +
      scale_colour_manual(values = c("grey", "black")) +
      geom_text_repel(aes(year, outlier2, label = outlier_vis), size = 2, nudge_x = .15) +
      scale_y_continuous(labels = scales::percent) +
      ggtitle("Figure 3 - Marriage and Pregnancy Prevention Expenditures as a\nPercentage of Total TANF Expenditures (FY 1998 - 2013)")
ggsave("Figures and Tables/Figure3.pdf", height = 5, width = 6.5, units = "in")

# standard deviations for marriage and pregnancy programs
sd_pregnancy <- avg_props_id %>% 
  select(STATE, year, pregnancy) %>% 
  spread(year, pregnancy) %>%
  select(-STATE) %>% 
  sapply(sd, na.rm = TRUE)

# Figure 4 - Refundable Tax Credits Boxplot #### 
avg_props_id %>% 
  select(state_id:year, tax) %>%
  group_by(year) %>% 
  mutate(lql= quantile(tax, probs = .25, na.rm = TRUE)) %>% 
  mutate(hql = quantile(tax, probs = .75, na.rm = TRUE)) %>% 
  mutate(iqr = hql - lql) %>% 
  mutate(outlier = ifelse((tax < (lql - (iqr * 1.5))) | (tax > (hql + (iqr * 1.5))), state_id, NA)) %>% 
  mutate(outlier2 = ifelse((tax < (lql - (iqr * 1.5))) | (tax > (hql + (iqr * 1.5))), tax, NA)) %>% 
  mutate(outlier_vis = ifelse((outlier != "NY") & (outlier != "NE") & (outlier != "MN") & (outlier != "KS"), NA, state_id)) %>% 
  mutate(outlier_vis2 = ifelse((is.na(outlier_vis)), 0, 1)) %>% 
  ggplot() +
    geom_boxplot(aes(year, tax, group = year), outlier.shape = NA) +
    scale_x_discrete(name = "", breaks = c("2000", "2005", "2010")) +
    theme(axis.title = element_blank(), 
          legend.position = "none") +
    geom_point(aes(year, outlier2, colour = factor(outlier_vis2))) +
    geom_text_repel(aes(year, outlier2, label = outlier_vis), size = 2, nudge_x = .15) +
    scale_colour_manual(values = c("grey", "black")) +
    scale_y_continuous(labels = scales::percent) +
    ggtitle("Figure 4 - Refundable Tax Credit Expenditures as a Percentage of\nTotal TANF Expenditures (FY 1998 - 2013)")
ggsave("Figures and Tables/Figure4.pdf", height = 5, width = 6.5, units = "in")

# standard deviations for refundable tax credit programs
sd_tax <- avg_props_id %>%
  select(STATE, year, tax) %>% 
  spread(year, tax) %>% 
  select(-STATE) %>% 
  sapply(sd, na.rm = TRUE)

# Figure 5 - Other Non-Assistance Boxplot ####
avg_props_id %>% 
  select(state_id:year, other) %>%
  group_by(year) %>% 
  mutate(lql= quantile(other, probs = .25, na.rm = TRUE)) %>% 
  mutate(hql = quantile(other, probs = .75, na.rm = TRUE)) %>% 
  mutate(iqr = hql - lql) %>% 
  mutate(outlier = ifelse((other < (lql - (iqr * 1.5))) | (other > (hql + (iqr * 1.5))), state_id, NA)) %>% 
  mutate(outlier2 = ifelse((other < (lql - (iqr * 1.5))) | (other > (hql + (iqr * 1.5))), other, NA)) %>% 
  mutate(outlier_vis = ifelse((outlier != "GA") & (outlier != "CO") & (outlier != "SC"), NA, state_id)) %>% 
  mutate(outlier_vis2 = ifelse((is.na(outlier_vis)), 0, 1)) %>% 
  ggplot() +
    geom_boxplot(aes(year, other, group = year), outlier.shape = NA) +
    scale_x_discrete(name = "", breaks = c("2000", "2005", "2010")) +
    theme(axis.title = element_blank(), 
          legend.position = "none") +
    geom_point(aes(year, outlier2, colour = factor(outlier_vis2))) +
    scale_colour_manual(values = c("grey", "black")) +
    geom_text_repel(aes(year, outlier2, label = outlier_vis), size = 2, nudge_x = .15) +
    scale_y_continuous(labels = scales::percent) +
    ggtitle("Figure 5 - Other Non-Assistance Expenditures as a Percentage of\nTotal TANF Expenditures (FY 1998 - 2013)")
ggsave("Figures and Tables/Figure5.pdf", height = 5, width = 6.5, units = "in")

# standard deviations 
sd_other <- avg_props_id %>%
  select(STATE, year, other) %>% 
  spread(year, other) %>% 
  select(-STATE) %>% 
  sapply(sd, na.rm = TRUE)

# Figure 6 - Basic Assistance Boxplot ####
avg_props_id %>% 
  select(state_id:year, ba) %>%
  group_by(year) %>% 
  mutate(lql= quantile(ba, probs = .25, na.rm = TRUE)) %>% 
  mutate(hql = quantile(ba, probs = .75, na.rm = TRUE)) %>% 
  mutate(iqr = hql - lql) %>% 
  mutate(outlier = ifelse((ba < (lql - (iqr * 1.5))) | (ba > (hql + (iqr * 1.5))), state_id, NA)) %>% 
  mutate(outlier2 = ifelse((ba < (lql - (iqr * 1.5))) | (ba > (hql + (iqr * 1.5))), ba, NA)) %>% 
  mutate(outlier_vis = ifelse((outlier != "ME") & (outlier != "CA") & (outlier != "SD") &
                              (outlier != "AK") & (outlier != "HI") & (outlier != "NM") &
                              (outlier != "ID"), NA, state_id)) %>% 
  mutate(outlier_vis2 = ifelse((is.na(outlier_vis)), 0, 1)) %>% 
  ggplot() +
    geom_boxplot(aes(year, ba, group = year)) +
    scale_x_discrete(name = "", breaks = c("2000", "2005", "2010")) +
    theme(axis.title = element_blank(), 
          legend.position = "none") +
    geom_point(aes(year, outlier2, colour = factor(outlier_vis2))) +
    scale_colour_manual(values = c("grey", "black")) +
    geom_text_repel(aes(year, outlier2, label = outlier_vis), size = 2, nudge_x = .15) +
    scale_y_continuous(labels = scales::percent) +
    ggtitle("Figure 6 - Basic Assistance Expenditures as a Percentage of Total\nTANF Expenditures (FY 1998 - 2013)")
ggsave("Figures and Tables/Figure6.pdf", height = 5, width = 6.5, units = "in")

# standard deviations for basic assistance
sd_ba <- avg_props_id %>%
  select(STATE, year, ba) %>% 
  spread(year, ba) %>% 
  select(-STATE) %>% 
  sapply(sd, na.rm = TRUE)

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

# Table 2 - Regression output ####

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
          title = "Table 2 - Regression Output",
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          covariate.labels = c(NA, NA, NA, NA, NA, NA, NA, "pcpi regional (thousands)"),
          dep.var.labels = "Basic Assistance Expenditures as a Percentage of Total TANF Expenditures",
          omit = "year",
          omit.labels = c("Time Fixed Effects"),
          notes.align = "r",
          model.numbers = FALSE,
          initial.zero = FALSE,
          out = "Figures and Tables/Table1.html")

# Tables A.2 and A.3 - Annual Mean and Median Tables ####
aggregate(avg_props[, 3:12], list(avg_props$year), mean, na.rm = TRUE) %>% 
  rename(year = `Group.1`) %>% 
  write_csv("Appendix Tables/TableA.2.csv")

aggregate(avg_props[, 3:12], list(avg_props$year), median, na.rm = TRUE) %>%
  rename(year = `Group.1`) %>%
  write_csv("Appendix Tables/TableA.3.csv")
# Table A.4 - Regression output of three cleaning methods ####

# Time fixed effects 
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









               

  
# Figure 7 - Top and bottom ten ####
top_ten <- avg_props_id %>% 
  filter(year == 1998) %>%
  arrange(desc(ba)) %>% 
  top_n(10, ba)

bottom_ten <- avg_props_id %>% 
  filter(year == 1998) %>%
  arrange(desc(ba)) %>% 
  top_n(-10, ba)

avg_props_id %>% 
  filter(year == 1998 | year == 2013) %>%
  mutate(year = as.factor(year)) %>%
  mutate(rank = as.factor(ifelse(state_id %in% top_ten$state_id, 1, 
                                 ifelse(state_id %in% bottom_ten$state_id, 2, 0)))) %>%
  ggplot(aes(year, ba, group = state_id, color = rank)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent, name = element_blank()) +
  scale_x_discrete(name = element_blank()) +
  scale_color_manual(values = c("#cccccc", "#666666", "#000000"), name = element_blank(), breaks = c(1, 2),  
  labels = c("Ten highest spending\nstates in 1998", "Ten lowest spending\nstates in 1998")) +
  labs(title = "Figure 7 - Basic Assistance Expenditures as a Proportion\nof Total TANF Expenditures in 1998 and 2013")
ggsave("Figures and Tables/Figure7.pdf", height = 5, width = 6.5, units = "in")





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