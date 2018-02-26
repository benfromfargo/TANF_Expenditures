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

# Checks ####
check_data <- function(data) {
  data %>% 
    gather(key = "category", value = "value", -STATE, -year) %>% 
    select(STATE, category, year, value) %>%
    unite(category, category, year, sep = "_") %>% 
    spread(key = "category", value = "value")
}

check_data(props_vis) %>% 
  write_csv("Checks/props.csv")
check_data(avg_props_vis) %>% 
  write_csv("Checks/avg_props.csv")
check_data(props_avg_vis) %>% 
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

# Figure 1 - Annual Mean Expenditures ####
ann_means <- aggregate(avg_props[, 3:12], list(avg_props$year), mean, na.rm = TRUE) %>% 
  rename(year = `Group.1`) 
ann_means <- gather(ann_means, key = "category", value = "value", -year)

ann_means_lab <- ann_means %>% 
  mutate(category = ifelse(category == "admin", "Administration 
and Systems",
                          ifelse(category == "ba", "Basic Assistance",
                          ifelse(category == "cc", "Child Care",
                          ifelse(category == "other", "Other Non-Assistance", 
                          ifelse(category == "pregnancy", "Marriage and Pregnancy 
Programs",
                          ifelse(category == "prior", "Expenditures Under 
Prior Law",
                          ifelse(category == "shortben", "Diversion Benefits", 
                          ifelse(category == "ssbg", "Social Services 
Block Grant",
                          ifelse(category == "tax", "Refundable Tax Credits",
                          ifelse(category == "work", "Work Related Activities 
and Supports", NA)))))))))))

ann_means_lab1 <- ann_means_lab %>% 
  filter(category == "Administration 
and Systems" | category == "Basic Assistance"
         | category == "Child Care" | category == "Diversion Benefits"
         | category == "Expenditures Under 
Prior Law")
ann_means_lab2 <- ann_means_lab %>% 
  filter(category == "Marriage and Pregnancy 
Programs" | category == "Other Non-Assistance"
         | category == "Refundable Tax Credits" | category == "Social Services 
Block Grant"
         | category == "Work Related Activities 
and Supports")

ggplot(ann_means_lab1, aes(year, value)) +
  geom_col() +
  facet_grid(category ~.) +
  scale_x_discrete(name = "", breaks = c("2000", "2005", "2010")) +
  scale_y_continuous(name = "", labels = scales::percent, limits = c(0, .6)) +
  theme(strip.text.y = element_text(angle = 0)) +
  ggtitle("Figure 1 - Mean TANF Expenditures as a Percentage of Total 
Expenditures by Category (FY 1998 - 2013)")
ggsave("Figures and Tables/Figure1.pdf", height = 5, width = 6.5, units = "in")

ggplot(ann_means_lab2, aes(year, value)) +
  geom_col() +
  facet_grid(category ~.) +
  scale_x_discrete(name = "", breaks = c("2000", "2005", "2010")) +
  scale_y_continuous(name = "", labels = scales::percent, limits = c(0, .6)) +
  theme(strip.text.y = element_text(angle = 0)) +
  ggtitle("Figure 1 (continued) - Mean TANF Expenditures as a Percentage of Total 
Expenditures by Category (FY 1998 - 2013)")
ggsave("Figures and Tables/Figure1_continued.pdf", height = 5, width = 6.5, units = "in")

# Figure 2 - Marriage and Pregnancy Prevention Boxplot ####

# New dataframe with state abbreivations for boxplots
s_abbvs <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", 
             "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
             "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", 
             "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
             "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
             "WY")
data_id <- avg_props %>% 
  as.tibble() %>%
  arrange(desc(year)) %>% 
  mutate(state_id = rep_len(s_abbvs, length.out = 816)) %>%
  select(state_id, STATE, everything())
data_pregnancy <- select(data_id, state_id:year, pregnancy) %>%
  group_by(year) %>% 
  mutate(lql= quantile(pregnancy, probs = .25, na.rm = TRUE)) %>% 
  mutate(hql = quantile(pregnancy, probs = .75, na.rm = TRUE)) %>% 
  mutate(iqr = hql - lql) %>% 
  mutate(outlier = ifelse((pregnancy < (lql - (iqr * 1.5))) | (pregnancy > (hql + (iqr * 1.5))), state_id, NA)) %>%
  mutate(outlier2 = ifelse((pregnancy < (lql - (iqr * 1.5))) | (pregnancy > (hql + (iqr * 1.5))), pregnancy, NA)) %>% 
  mutate(outlier_vis = ifelse((outlier != "AR") & (outlier != "LA") & (outlier != "NJ"), NA, state_id)) %>% 
  mutate(outlier_vis2 = ifelse((is.na(outlier_vis)), 0, 1))

ggplot(data_pregnancy) +
  geom_boxplot(aes(year, pregnancy, group = year), outlier.shape = NA) +
  scale_x_discrete(name = "", breaks = c("2000", "2005", "2010")) +
  theme(axis.title = element_blank(), 
        legend.position = "none") +
  geom_point(aes(year, outlier2, colour = factor(outlier_vis2))) +
  scale_colour_manual(values = c("grey", "black")) +
  geom_text_repel(aes(year, outlier2, label = outlier_vis), size = 2, nudge_x = .15) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Figure 2 - Marriage and Pregnancy Prevention Expenditures as a 
Percentage of Total TANF Expenditures (FY 1998 - 2013)")
ggsave("Figures and Tables/Figure2.pdf", height = 5, width = 6.5, units = "in")

# standard deviations 
data_pregnancy <- data_pregnancy %>%
  select(STATE, year, pregnancy) 
data_pregnancy <- spread(data_pregnancy, year, pregnancy)
sd_pregnancy <- data_frame(year = c("1998", "1999", "2000", "2001", "2002", "2003", "2004",
                  "2005", "2006", "2007", "2008", "2009", "2010", 
                  "2011", "2012", "2013"), sd = sapply(data_pregnancy[, 2:17], sd, na.rm = TRUE))

# Figure 3 - Refundable Tax Credits Boxplot #### 
data_tax <- select(data_id, state_id:year, tax) %>%
  group_by(year) %>% 
  mutate(lql= quantile(tax, probs = .25, na.rm = TRUE)) %>% 
  mutate(hql = quantile(tax, probs = .75, na.rm = TRUE)) %>% 
  mutate(iqr = hql - lql) %>% 
  mutate(outlier = ifelse((tax < (lql - (iqr * 1.5))) | (tax > (hql + (iqr * 1.5))), state_id, NA)) %>% 
  mutate(outlier2 = ifelse((tax < (lql - (iqr * 1.5))) | (tax > (hql + (iqr * 1.5))), tax, NA)) %>% 
  mutate(outlier_vis = ifelse((outlier != "NY") & (outlier != "NE") & (outlier != "MN") & (outlier != "KS"), NA, state_id)) %>% 
  mutate(outlier_vis2 = ifelse((is.na(outlier_vis)), 0, 1))

ggplot(data_tax) +
  geom_boxplot(aes(year, tax, group = year), outlier.shape = NA) +
  scale_x_discrete(name = "", breaks = c("2000", "2005", "2010")) +
  theme(axis.title = element_blank(), 
        legend.position = "none") +
  geom_point(aes(year, outlier2, colour = factor(outlier_vis2))) +
  geom_text_repel(aes(year, outlier2, label = outlier_vis), size = 2, nudge_x = .15) +
  scale_colour_manual(values = c("grey", "black")) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Figure 3 - Refundable Tax Credit Expenditures as a Percentage of 
Total TANF Expenditures (FY 1998 - 2013)")
ggsave("Figures and Tables/Figure3.pdf", height = 5, width = 6.5, units = "in")

# standard deviations 
data_tax <- data_tax %>%
  select(STATE, year, tax) 
data_tax <- spread(data_tax, year, tax)
sd_tax <- data_frame(year = c("1998", "1999", "2000", "2001", "2002", "2003", "2004",
                                    "2005", "2006", "2007", "2008", "2009", "2010", 
                                    "2011", "2012", "2013"), sd = sapply(data_tax[, 2:17], sd, na.rm = TRUE))

# Figure 4 - Other Non-Assistance Boxplot ####
data_other <- select(data_id, state_id:year, other) %>%
  group_by(year) %>% 
  mutate(lql= quantile(other, probs = .25, na.rm = TRUE)) %>% 
  mutate(hql = quantile(other, probs = .75, na.rm = TRUE)) %>% 
  mutate(iqr = hql - lql) %>% 
  mutate(outlier = ifelse((other < (lql - (iqr * 1.5))) | (other > (hql + (iqr * 1.5))), state_id, NA)) %>% 
  mutate(outlier2 = ifelse((other < (lql - (iqr * 1.5))) | (other > (hql + (iqr * 1.5))), other, NA)) %>% 
  mutate(outlier_vis = ifelse((outlier != "GA") & (outlier != "CO") & (outlier != "SC"), NA, state_id)) %>% 
  mutate(outlier_vis2 = ifelse((is.na(outlier_vis)), 0, 1))

ggplot(data_other) +
  geom_boxplot(aes(year, other, group = year), outlier.shape = NA) +
  scale_x_discrete(name = "", breaks = c("2000", "2005", "2010")) +
  theme(axis.title = element_blank(), 
        legend.position = "none") +
  geom_point(aes(year, outlier2, colour = factor(outlier_vis2))) +
  scale_colour_manual(values = c("grey", "black")) +
  geom_text_repel(aes(year, outlier2, label = outlier_vis), size = 2, nudge_x = .15) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Figure 4 - Other Non-Assistance Expenditures as a Percentage of 
Total TANF Expenditures (FY 1998 - 2013)")
ggsave("Figures and Tables/Figure4.pdf", height = 5, width = 6.5, units = "in")

# standard deviations 
data_other <- data_other %>%
  select(STATE, year, other) 
data_other <- spread(data_other, year, other)
sd_other <- data_frame(year = c("1998", "1999", "2000", "2001", "2002", "2003", "2004",
                              "2005", "2006", "2007", "2008", "2009", "2010", 
                              "2011", "2012", "2013"), sd = sapply(data_other[, 2:17], sd, na.rm = TRUE))

# Figure 5 - Basic Assistance Boxplot ####
data_ba <- select(data_id, state_id:year, ba) %>%
  group_by(year) %>% 
  mutate(lql= quantile(ba, probs = .25, na.rm = TRUE)) %>% 
  mutate(hql = quantile(ba, probs = .75, na.rm = TRUE)) %>% 
  mutate(iqr = hql - lql) %>% 
  mutate(outlier = ifelse((ba < (lql - (iqr * 1.5))) | (ba > (hql + (iqr * 1.5))), state_id, NA)) %>% 
  mutate(outlier2 = ifelse((ba < (lql - (iqr * 1.5))) | (ba > (hql + (iqr * 1.5))), ba, NA)) %>% 
  mutate(outlier_vis = ifelse((outlier != "ME") & (outlier != "CA") & (outlier != "SD") &
                              (outlier != "AK") & (outlier != "HI") & (outlier != "NM") &
                              (outlier != "ID"), NA, state_id)) %>% 
  mutate(outlier_vis2 = ifelse((is.na(outlier_vis)), 0, 1))

ggplot(data_ba) +
  geom_boxplot(aes(year, ba, group = year)) +
  scale_x_discrete(name = "", breaks = c("2000", "2005", "2010")) +
  theme(axis.title = element_blank(), 
        legend.position = "none") +
  geom_point(aes(year, outlier2, colour = factor(outlier_vis2))) +
  scale_colour_manual(values = c("grey", "black")) +
  geom_text_repel(aes(year, outlier2, label = outlier_vis), size = 2, nudge_x = .15) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Figure 5 - Basic Assistance Expenditures as a Percentage of Total 
TANF Expenditures (FY 1998 - 2013)")
ggsave("Figures and Tables/Figure5.pdf", height = 5, width = 6.5, units = "in")

data_ba <- data_ba %>%
  select(STATE, year, ba) 
data_ba <- spread(data_ba, year, ba)
sd_ba <- data_frame(year = c("1998", "1999", "2000", "2001", "2002", "2003", "2004",
                                "2005", "2006", "2007", "2008", "2009", "2010", 
                                "2011", "2012", "2013"), sd = sapply(data_ba[, 2:17], sd, na.rm = TRUE))

# Note: The "missing value" warnings in the boxplot code stem from outlier labelling.

# Figure 6 - TANF Caseload Line Plot ####
case_data <- read_xlsx("Input Data/TANF_Figure6_caseloads.xlsx")

ggplot(case_data) +
  geom_line(aes(Year, Caseload)) +
  ggtitle("Figure 6 - Average Monthly TANF recipients (1998 - 2013)") +
  theme(axis.title.x = element_blank()) +
  ylab("TANF recipients (millions)") +
  scale_y_continuous(limits = c(0,9), breaks = seq(0,9,2))
ggsave("Figures and Tables/Figure6.pdf", height = 4, width = 6, units = "in")

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









# Code for claim about liberalism in section VI ####                  
fargo <- select(ind_data, STATE, year, liberalism) %>% 
  group_by(STATE) %>% 
  summarise(sd = sd(liberalism, na.rm = TRUE)) %>%
  ungroup()
mean(fargo$sd, na.rm = TRUE)

  