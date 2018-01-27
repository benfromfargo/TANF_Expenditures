# Check

# Front Matter #####

# Load required packages
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

#Remove scientific notation 
options(scipen = 999)

# Clean Expenditure Data #####
        
### Load raw data ###
raw_data <- read.xlsx("Input Data/TANF_expenditures.xlsx", sheet = "Raw Values (for R)")

### Moving averages of proportions ###

# Proportions of raw data 
raw_data <- gather(raw_data, key = "category", value = "value", -STATE) %>% 
  arrange(STATE)

create_prop <- function(x, data) {
  data[seq(x, nrow(data), 11), 3] / data[seq(11, nrow(data), 11), 3]
}

value <- c(
  create_prop(1, raw_data),
  create_prop(2, raw_data),
  create_prop(3, raw_data),
  create_prop(4, raw_data),
  create_prop(5, raw_data),
  create_prop(6, raw_data),
  create_prop(7, raw_data),
  create_prop(8, raw_data),
  create_prop(9, raw_data),
  create_prop(10, raw_data), 
  create_prop(11, raw_data)
  )

# Rearrange colomns to match formula output and bind together
ordered_cols <- arrange(raw_data, category, STATE) %>% 
  separate(category, into = c("category", "year"), sep = "_") %>% 
  arrange(category, STATE, year)

props <- cbind(ordered_cols[, 1:3], value)

# Moving averages of proportions 
props <- props %>%
  filter(!grepl("ztotal", props$category)) %>% 
  arrange(STATE, category)

avg_props <- as.data.frame(rollmean(props[, 4], 3, fill = NA))
avg_props <- cbind(props[, 1:3], avg_props)

# Remove meaningless values
avg_props <- avg_props %>% 
  filter(!(year %in% c("1997", "2014"))) %>% 
  spread(key = category, value = "rollmean(props[, 4], 3, fill = NA)")

props <- props %>% 
  filter(!(year %in% c("1997", "2014"))) %>% 
  spread(key = category, value = value)

# Replace outliers with NAs and control for numerical errors
re_outliers <- function(x) {
  ifelse(x < 0 | x > 1, NA, x)
}

avg_props_rnd <- as.data.frame(sapply(avg_props[, 3:12], round, digits = 10))
avg_props <- cbind(avg_props[, 1:2], sapply(avg_props_rnd, re_outliers))
avg_props_vis <- cbind(avg_props[, 1:2], avg_props_rnd)

props_rnd <- as.data.frame(sapply(props[, 3:12], round, digits = 10))
props <- cbind(props[, 1:2], sapply(props_rnd, re_outliers))
props_vis <- cbind(props[, 1:2], props_rnd)


### Proportions of moving averages ### 

# Moving averages of raw data
avg_raw2 <- raw_data %>% 
  arrange(STATE, category)

avg_raw <- as.data.frame(rollmean(avg_raw2[, 3], 3, fill = NA))
avg_raw <- cbind(avg_raw2[, 1:2], avg_raw)

# Proportions of moving averages
avg_raw <- avg_raw %>% 
  separate(category, into = c("category", "year"), sep = "_") %>% 
  arrange(STATE, year) %>% 
  unite("category", c("category", "year"))

value2 <- c(
  create_prop(1, avg_raw),
  create_prop(2, avg_raw),
  create_prop(3, avg_raw),
  create_prop(4, avg_raw),
  create_prop(5, avg_raw),
  create_prop(6, avg_raw),
  create_prop(7, avg_raw),
  create_prop(8, avg_raw),
  create_prop(9, avg_raw),
  create_prop(10, avg_raw),
  create_prop(11, avg_raw)
)

props_avg <- cbind(ordered_cols[, 1:3], value2)

# Remove meaningless values
props_avg <- props_avg %>% 
  filter(!grepl("ztotal", props_avg$category)) %>% 
  filter(!(year %in% c("1997", "2014"))) %>% 
  spread(key = category, value = value2)

# Control for numerical errors
props_avg_rnd <- as.data.frame(sapply(props_avg[, 3:12], round, digits = 10))

props_avg <- cbind(props_avg[, 1:2], sapply(props_avg_rnd, re_outliers))
props_avg_vis <- cbind(props_avg[, 1:2], props_avg_rnd)

# Checks ####
check_data <- function(data) {
  data %>% 
    gather(key = "category", value = "value", -STATE, -year) %>% 
    select(STATE, category, year, value) %>%
    unite(category, category, year, sep = "_") %>% 
    spread(key = "category", value = "value")
}

props_excel <- check_data(props_vis) %>% 
  write_csv("Checks/props.csv")
avg_props_excel <- check_data(avg_props_vis) %>% 
  write_csv("Checks/avg_props.csv")
props_avg_excel <- check_data(props_avg_vis) %>% 
  write_csv("Checks/props_avg.csv")

# NA count checks
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

# Figure 1 - Annual Mean Expenditures Stacked Bar Chart ####

ann_means <- aggregate(avg_props[, 3:12], list(avg_props[, 2]), mean, na.rm = TRUE) %>% 
  rename(year = `Group.1`) 
ann_means <- gather(ann_means, key = "category", value = "value", -year)

vis_vals <- c(0.9778192, 0.8178061, 0.61509645, 0.43785075,
              0.3096736, 0.25362805, 0.21485205,0.1831645,
              0.1427271,0.057719) 

cum_sum <- as.data.frame(vis_vals) %>% 
  mutate(year = rep("2013", length.out = 10)) %>% 
  mutate(category = c("Administration and Systems", "Basic Assistance", "Child Care",
                      "Other Non-Assistance","Marriage and Pregnancy Programs", 
                      "Expenditures Under Prior Law", "Diversion Benefits",
                      "Social Services Block Grant", "Refundable Tax Credits", 
                      "Work Related Activities and Supports"))
 
bracketsGrob <- function(...){
  l <- list(...)
  e <- new.env()
  e$l <- l
  grid:::recordGrob(  {
    do.call(grid.brackets, l)
  }, e)
}

b1 <- bracketsGrob(.99, .955, .99, .885, h=0.01, lwd=1, col="black")
b2 <- bracketsGrob(.99, .885, 0.99, .675, h=0.01,  lwd=1, col="black")
b3 <- bracketsGrob(.99, 0.675, 0.99, 0.52, h=0.01, lwd=1, col="black")
b4 <- bracketsGrob(.99, 0.52, 0.99, 0.355, h=0.01,  lwd=1, col="black")
b5 <- bracketsGrob(.99, 0.355, 0.99, 0.29, h=0.01,  lwd=1, col="black")
b6 <- bracketsGrob(.99, 0.29, 0.99, 0.25, h=0.01,  lwd=1, col="black")
b7 <- bracketsGrob(.99, 0.25, 0.99, 0.23, h=0.01,  lwd=1, col="black")
b8 <- bracketsGrob(.99, 0.23, 0.99, 0.20, h=0.01,  lwd=1, col="black")
b9 <- bracketsGrob(.99, 0.20, 0.99, 0.15, h=0.01,  lwd=1, col="black")
b10 <- bracketsGrob(.99, 0.15, 0.99, .048, h=0.01,  lwd=1, col="black")



bam <- ggplot(ann_means, aes(year, value, fill = category)) +
  geom_col() +
  labs(y = NULL) +
  scale_x_discrete(name = "", breaks = c("2000", "2005", "2010")) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(data = cum_sum, aes(label = category, x = Inf, y = vis_vals), hjust = 0, 
            size = 2.75, check_overlap = FALSE) +
  annotation_custom(b1) +
  annotation_custom(b2) +
  annotation_custom(b3) +
  annotation_custom(b4) +
  annotation_custom(b5) +
  annotation_custom(b6) +
  annotation_custom(b7) +
  annotation_custom(b8) +
  annotation_custom(b9) +
  annotation_custom(b10) +
  theme(panel.background = element_blank(), 
        legend.position = "none", 
        plot.margin = unit(c(.5, 6, .5, .5), "cm"),
        axis.text.x = element_text(margin=margin(-10,0,0,0)),
        axis.ticks.x = element_blank(), 
        axis.text.y.right = element_text(margin = margin(r = 3))) + 
  ggtitle("Figure 1 - Mean TANF Expenditures as a Percentage 
of Total Expenditures (1998 - 2013)")

gt <- ggplot_gtable(ggplot_build(bam))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
pdf("Figures and Tables/Figure1.pdf", width = 6.5, height = 5); plot(gt); dev.off()

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
  theme(axis.title = element_blank()) +
  geom_point(aes(year, outlier2, colour = factor(outlier_vis2))) +
  scale_colour_manual(values = c("grey", "black")) +
  geom_text_repel(aes(year, outlier2, label = outlier_vis), size = 2, nudge_x = .15) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Figure 2 - Marriage and Pregnancy Prevention Expenditures as a 
Percentage of Total TANF Expenditures (1998 - 2013)")
ggsave("Figures and Tables/Figure2.pdf", height = 5, width = 6.5, units = "in")

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
  theme(axis.title = element_blank()) +
  geom_point(aes(year, outlier2, colour = factor(outlier_vis2))) +
  geom_text_repel(aes(year, outlier2, label = outlier_vis), size = 2, nudge_x = .15) +
  scale_colour_manual(values = c("grey", "black")) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Figure 3 - Refundable Tax Credit Expenditures as a Percentage of 
Total TANF Expenditures (1998 - 2013)")
ggsave("Figures and Tables/Figure3.pdf", height = 5, width = 6.5, units = "in")

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
  theme(axis.title = element_blank()) +
  geom_point(aes(year, outlier2, colour = factor(outlier_vis2))) +
  scale_colour_manual(values = c("grey", "black")) +
  geom_text_repel(aes(year, outlier2, label = outlier_vis), size = 2, nudge_x = .15) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Figure 4 - Other Non-Assistance Expenditures as a Percentage of 
Total TANF Expenditures (1998 - 2013)")
ggsave("Figures and Tables/Figure4.pdf", height = 5, width = 6.5, units = "in")

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
  theme(axis.title = element_blank()) +
  geom_point(aes(year, outlier2, colour = factor(outlier_vis2))) +
  scale_colour_manual(values = c("grey", "black")) +
  geom_text_repel(aes(year, outlier2, label = outlier_vis), size = 2, nudge_x = .15) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Figure 5 - Basic Assistance Expenditures as a Percentage of Total 
TANF Expenditures (1998 - 2013)")
ggsave("Figures and Tables/Figure5.pdf", height = 5, width = 6.5, units = "in")

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
  
# Increase all independent variable years by one to reflect that e.g. 1997 ind vars were in place when 1998 expenditures were decided.
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
props_pdata_percent <- sapply(props_pdata[, 3:20], to_percent)
props_pdata <- cbind(props_pdata[, 1:2], props_pdata_percent)
props_pdata <- pdata.frame(props_pdata, index = c("STATE", "year"))

avg_props_pdata <- join_data(avg_props, ind_data)
avg_props_pdata_percent <- sapply(avg_props_pdata[, 3:20], to_percent)
avg_props_pdata <- cbind(avg_props_pdata[, 1:2], avg_props_pdata_percent)
avg_props_pdata <- pdata.frame(avg_props_pdata, index = c("STATE", "year"))

props_avg_pdata <- join_data(props_avg, ind_data)
props_avg_pdata_percent <- sapply(props_avg_pdata[, 3:20], to_percent)
props_avg_pdata <- cbind(props_avg_pdata[, 1:2], props_avg_pdata_percent)
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
          covariate.labels = c(NA, NA, NA, "caseload (thousands)", NA, NA, NA, "pcpi regional (thousands)"),
          dep.var.labels = "Basic Assistance Expenditures as a Percentage of Total TANF Expenditures",
          omit = "year",
          omit.labels = c("Time Fixed Effects"),
          notes.align = "r",
          model.numbers = FALSE,
          initial.zero = FALSE,
          out = "Figures and Tables/Table1.html")




# Tables A.2 and A.3 - Annual Mean and Median Tables ####
ann_means_print <- aggregate(avg_props[, 3:12], list(avg_props[, 2]), mean, na.rm = TRUE) %>% 
  write_csv("Appendix Tables/TableA.2.csv")
ann_medians_print <- aggregate(avg_props[, 3:12], list(avg_props[, 2]), median, na.rm = TRUE) %>% 
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
          covariate.labels = c(NA, NA, NA, "caseload (thousands)", NA, NA, NA, "pcpi regional (thousands)"),
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

  