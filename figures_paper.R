source("TANF_clean.R")

library(ggrepel)
library(gtable)
library(grid)
library(stargazer)
library(extrafont)
library(plm)

# Figure 1 ####
files <- list.files("Caseloads/")
files <- str_c("Caseloads/", files)

ReadR <- function(file) {
  if (str_detect(file, "xlsx")) {
    read_xlsx(file)
  }
  else {
    read_xls(file)
  }
}

case_raw <- as.data.frame(map(files, ReadR)) %>% 
  select(-starts_with("State.")) %>% 
  gather(key = "category", value = "value", -State) %>% 
  mutate(category = str_replace(category, "X", "")) %>% 
  mutate(value = floor(value)) %>% 
  separate(category, c("year", "category"), "_") %>% 
  filter(State != "us_total") %>% 
  group_by(year) %>% 
  mutate(ann_value = sum(year)) %>% 
  ungroup()

files_work <- list.files("Workers/")
files_work <- str_c("Workers/", files_work)

workers_raw <- as.data.frame(map(files_work, ReadR)) %>% 
  select(-starts_with("state."))

case_raw %>% 
  filter(category == "families" | category == "0.families") %>% 
  ggplot(aes(year, ann_value, group = category, color = category)) +
  geom_line() +
  labs(title = "Figure 1: Families Receiving TANF Assistance in an Average Month",
       subtitle = "1998 - 2017",
       caption = "In millions of families") +
  scale_colour_manual(labels = c("Child-only families", "All families"), 
                      name = element_blank(), 
                      guide = guide_legend(reverse = TRUE), 
                      values = c("#666666", "#000000")) +  
  scale_x_discrete(breaks = c("2000", "2005", "2010", "2015")) + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        text = element_text(family = "Times New Roman")) +
  scale_y_continuous(breaks = c(1000000, 1500000, 2000000, 2500000, 3000000),
                     labels = c("1", "1.5", "2", "2.5", "3"))
ggsave("Figures and Tables/Figure1.pdf", height = 5, width = 6.5, units = "in")  

# Figure 2 ####
cpi <- tribble(
  ~year, ~annual_cpi,
  "1997",	169.5,
  "1998",	173.4,
  "1999",	177.0,
  "2000",	181.3,
  "2001",	186.1,
  "2002",	190.5,
  "2003",	193.2,
  "2004",	196.6,
  "2005",	200.9,
  "2006",	205.9,
  "2007",	210.729,
  "2008",	215.572,
  "2009",	219.235,
  "2010",	221.337,
  "2011",	225.008,
  "2012",	229.755,
  "2013",	233.806,
  "2014",	237.897
)

raw_data <- left_join(raw_data, cpi, by = "year")
anti_join(raw_data, cpi, by = "year")

raw_data <- raw_data %>% 
  mutate(dec_form = annual_cpi / 237.897) %>% 
  mutate(real_exp = value / dec_form) %>% 
  group_by(category, year) %>% 
  mutate(category_total = sum(real_exp)) %>% 
  ungroup()

raw_data %>%
  filter(year != "1997") %>% 
  filter(category == "ba") %>% 
  ggplot(aes(year, category_total, group = category)) +
  geom_line() +
  labs(title = "Figure 2: Aggregate Reported TANF Spending on Basic Assistance", 
       subtitle = "FY 1998 - 2014",
       caption = "In billions of 2014 dollars") +
  scale_x_discrete(breaks = c("2000", "2005", "2010")) + 
  scale_y_continuous(breaks = seq(10000000000, 20000000000, by = 2000000000), 
                     labels = c("$10", "$12", "$14", "$16", "$18", "$20")) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        text = element_text(family = "Times New Roman"))
ggsave("Figures and Tables/Figure2.pdf", height = 5, width = 6.5, units = "in")  

# Figure 3 ####

ann_means <- avg_props %>% 
  gather("category", "value", -STATE, -year) %>% 
  group_by(year, category) %>% 
  summarise(value = mean(value, na.rm = TRUE))

ann_means_vis <- spread(ann_means, key = "category", value = "value")

ann_means_vis <- ann_means_vis %>%
  mutate(service = cc + pregnancy + shortben + tax + work) %>% 
  mutate(other2 = admin + other + prior + ssbg) %>% 
  select(year, ba, service, other = other2)

ann_means_vis <- gather(ann_means_vis, key = "category", value = "value", -year)

ann_means_vis <- ann_means_vis %>% 
  mutate(category = factor(category, levels = c("other", "service", "ba")))

ggplot(ann_means_vis, aes(year, value, fill = category)) +
  geom_col() +
  scale_x_discrete(breaks = c("2000", "2005", "2010")) +
  scale_y_continuous(labels = scales::percent,
                     expand = c(0,.02)) +
  scale_fill_manual(values = c("#cccccc", "#666666", "#000000"),
                    labels = c("Other", 
                               "Work-related, in-kind,\nand short-term benefits", 
                               "Basic assistance"),
                    name = "Type of Spending") +
  theme(panel.grid.major = element_blank(), axis.title.x = element_blank(), 
        panel.grid.minor = element_blank(), axis.title.y = element_blank(),
        panel.background = element_blank(), legend.title = element_text(size = 9),
        legend.text = element_text(size = 8), plot.caption = element_text(size = 7, hjust = 0),
        text = element_text(family = "Times New Roman")) +
  labs(title = "Figure 3: Mean Proportional TANF Spending by Type",
       subtitle = "FY 1998 - 2013",
        caption = "Note: See Table A.1 in the appendix for category groups. Percentages may not add up to 100% in a given fiscal year due to the removal of outlier values. 
See appendix for more information.")
ggsave("Figures and Tables/Figure3.pdf", height = 5, width = 6.5, units = "in")  

# Figure 4 ####
x <- ann_means %>% 
  filter(category != "ba" & category != "admin" & category != "other" & category != "prior" & category != "ssbg") %>%
  mutate(label = ifelse(year == "2013", category, NA)) %>% 
  mutate(label = case_when(
    label == "cc" ~ "     Child care",
    label == "work" ~ "     Work-related activities and supports", 
    label == "pregnancy" ~ "     Marriage and pregnancy programs",
    label == "tax" ~ "     Refundable tax credits", 
    label == "shortben" ~ "     Diversion benefits")) %>% 
  ggplot(aes(year, value, group = category)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent,
                     name = element_blank()) +
  scale_x_discrete(breaks = c("2000", "2005", "2010")) +
  geom_text(aes(label = label),
            na.rm = TRUE,
            hjust = 0, 
            family = "Times New Roman",
            size = 3) +
  theme(plot.margin = unit(c(1,12,1,1), "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(family = "Times New Roman")) + 
  labs(title = "Figure 4: Mean Expenditures on Work-Related, In-Kind, and Short-Term Benefits",
       subtitle = "FY 1998 - 2013")

gt <- ggplotGrob(x)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)
ggsave("Figures and Tables/Figure4.pdf", gt, height = 5, width = 6.7, units = "in")  

# Figure 5 ####
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
  scale_x_discrete(name = element_blank(), 
                   breaks = c("2000", "2005", "2010")) +
  theme(axis.title = element_blank(), 
        legend.position = "none",
        text = element_text(family = "Times New Roman")) +
  geom_point(aes(year, outlier2)) +
  geom_text_repel(aes(year, outlier2, 
                      label = outlier), 
                  size = 2, 
                  nudge_x = .15) +
  labs(title = "Figure 5: Boxplots of Proportional Basic Assistance Expenditures",
       subtitle = "FY 1998 - 2013") +
  scale_y_continuous(labels = scales::percent)
ggsave("Figures and Tables/Figure5.pdf", height = 5, width = 6.5, units = "in")  

# Figure 6 ####
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
                     labels = c("Ten states with the greatest\nshare of TANF funds spent on\nbasic assistance in FY 1998",
                                "Ten states with the smallest\nshare of TANF funds spent on\nbasic assistance in FY 1998")) +
  scale_alpha_manual(values = c(.4, .8, .8),
                     guide = "none") +
  labs(title = "Figure 6: Proportional Basic Assistance Spending in FY 1998 and FY 2013",
        caption = "Note: South Carolina and Tennessee removed due to negative reported basic assistance expenditures in FY 1998. See appendix for more information.") +
  theme(plot.caption=element_text(size=7, hjust = 0), legend.text = element_text(size = 8),
        text = element_text(family = "Times New Roman"), legend.key = element_rect(size = 7),
        legend.key.size = unit(2, 'lines'))
ggsave("Figures and Tables/Figure6.pdf", height = 5, width = 6.5, units = "in")  

# Table 1 ####
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
          title = "Table 1: Regression Output",
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          covariate.labels = c("african americans", NA, "fiscal stability", NA, NA, NA, NA, "pcpi regional"),
          dep.var.labels = "Basic Assistance Expenditures as a Percentage of Total TANF Expenditures",
          omit = "year",
          header = FALSE,
          omit.labels = c("Time Fixed Effects"),
          notes.align = "r",
          model.numbers = FALSE,
          initial.zero = FALSE,
          column.sep.width = "1pt",
          font.size = "small",
          type = "latex",
          out = "Figures and Tables/Table1.html")

# Figure 7 ####
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
  labs(title = "Figure 7: Coefficients of Time Fixed Effects from Model 4",
       subtitle = "FY 1999 - 2013",
       caption = "Note: Error bars represent 95% confidence intervals.") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(family = "Times New Roman"),
        plot.caption=element_text(size=7, hjust = 0))
ggsave("Figures and Tables/Figure7.pdf", height = 5, width = 6.5, units = "in")  

# Table A.2 ####
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
          column.labels = c("Raw Percentages", "Moving Averages of Percentages", "Percentages of Moving Averages"),
          title = "Table A.2: Comparing Regression Output Across Three Data Cleaning Methods", 
          covariate.labels = c(NA, NA, "fiscal stability", NA, NA, NA, NA, "pcpi regional"),
          omit = "year",
          omit.labels = c("Time Fixed Effects"),
          notes.align = "l",
          initial.zero = FALSE,
          model.numbers = FALSE,
          header = FALSE,
          dep.var.labels = "Basic Assistance Expenditures as a Percentage of Total Expenditures",
          column.sep.width = "1pt",
          font.size = "small",
          type = "latex",
          out = "Figures and Tables/TableA.2.html")

# Table A.3 ####
ann_means <- ann_means %>% 
  spread(key = "category", value = "value")

write_csv(ann_means, "Figures and Tables/TableA.3.csv")

# Table A.4 ####
ann_medians <- avg_props %>% 
  gather("category", "value", -STATE, -year) %>% 
  group_by(year, category) %>% 
  summarise(value = median(value, na.rm = TRUE))

ann_medians <- ann_medians %>% 
  spread(key = "category", value = "value")

write_csv(ann_medians, "Figures and Tables/TableA.4.csv")























