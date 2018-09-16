# Init ####
source("TANF_clean.R")

library(ggrepel)
library(gtable)
library(grid)
library(stargazer)
library(extrafont)
library(plm)
library(gridExtra)

my_theme <- theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.major.y = element_line(colour = "#dedddd"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold"))
theme_set(my_theme)

# Figure 1 ####
files <- list.files("Caseloads/")
files <- str_c("Caseloads/", files)

files_work <- list.files("Workers/")
files_work <- str_c("Workers/", files_work)

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
  scale_color_manual(guide = FALSE, 
                      values = c("#000000", "#000000")) +  
  scale_x_discrete(breaks = c("1998", "2003", "2008", "2013", "2017")) + 
  scale_y_continuous(breaks = seq(0, 6000000, 1000000),
                     labels = c("0", "1", "2", "3", "4", "5", "6"),
                     limits = c(0,6100000),
                     expand = c(0,0)) +
  annotate("text", x = "2010", y = 4100000, 
           label = "All families", size = 3, 
           family = "Times New Roman",
           hjust = 0) +
  annotate("text", x = "2010", y = 1300000, 
           label = "Families with a work-eligible individual",
           size = 3, 
           family = "Times New Roman",
           hjust = 0)
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
       caption = "In billions of 2014 dollars",
       x = NULL,
       y = NULL) +
  scale_x_discrete(breaks = c("1998", "2003", "2008", "2014")) + 
  scale_y_continuous(breaks = seq(0, 20000000000, by = 5000000000),
                     labels = c("$0", "$5", "$10", "$15", "$20"),
                     limits = c(0, 20000000000),
                     expand = c(0,0))
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

ggplot(ann_means_vis, aes(year, value, color = category, group = category)) +
  geom_line() +
  scale_x_discrete(breaks = c("1998", "2003", "2008", "2013")) +
  scale_y_continuous(labels = scales::percent_format(1),
                     expand = c(0, 0),
                     limits = c(0, .6),
                     breaks = c(0, .2, .4, .6)) +
  scale_color_manual(values = c("#000000", "#000000", "#000000"),
                     guide = FALSE) +
  theme(plot.caption = element_text(size = 7, hjust = 0)) +
  labs(title = "Figure 3: Mean Proportional TANF Spending by Type",
       subtitle = "FY 1998 - 2013",
       x = NULL,
       y = NULL,
        caption = "Note: See Table A.1 in the appendix for category groups. Percentages may not add up to 100% in a given fiscal year due to the removal of outlier values.\nSee appendix for more information.") +
  annotate("text", "2008", .23, 
           label = "Basic assistance", 
           hjust = 0,
           size = 3,
           family = "Times New Roman") +
  annotate("text", "2008", .33, 
           label = "Other", 
           hjust = 0,
           size = 3,
           family = "Times New Roman") +
  annotate("text", "2008", .465, 
           label = "Work-related, in-kind,\nand short-term benefits", 
           hjust = 0,
           size = 3,
           family = "Times New Roman")
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
  scale_y_continuous(labels = scales::percent_format(1),
                     name = element_blank(),
                     expand = c(0,0),
                     breaks = c(0, .05, .1, .15, .2),
                     limits = c(0, .2)) +
  scale_x_discrete(breaks = c("1998", "2003", "2008", "2013")) +
  geom_text(aes(label = label),
            na.rm = TRUE,
            hjust = 0, 
            family = "Times New Roman",
            size = 3) +
  theme(plot.margin = unit(c(1,12,1,1), "lines")) + 
  labs(title = "Figure 4: Mean Expenditures on Work-Related, In-Kind, and Short-Term Benefits",
       subtitle = "FY 1998 - 2013",
       x = NULL,
       y = NULL)

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
                   breaks = c("1998", "2003", "2008", "2013")) +
  theme(legend.position = "none") +
  geom_point(aes(year, outlier2)) +
  geom_text_repel(aes(year, outlier2, 
                      label = outlier), 
                  size = 2, 
                  nudge_x = .05,
                  segment.color = NA) +
  labs(title = "Figure 5: Boxplots of Proportional Basic Assistance Expenditures",
       subtitle = "FY 1998 - 2013",
       x = NULL,
       y = NULL) +
  scale_y_continuous(labels = scales::percent_format(1),
                     expand = c(0,0),
                     breaks = seq(0, 1, .25),
                     limits = c(0,1))
ggsave("Figures and Tables/Figure5.pdf", height = 5, width = 6.5, units = "in")  

# Figure 6 ####
top_ten_98 <- avg_props_id %>% 
  filter(ba, year == 1998) %>%
  filter(!is.na(ba)) %>%
  top_n(10, ba)

bottom_ten_98 <- avg_props_id %>% 
  filter(year == 1998) %>%
  filter(!is.na(ba)) %>% 
  top_n(-10, ba)

## PLOT 1
plot_one <- avg_props_id %>% 
  filter(year == 1998 | year == 2013) %>%
  filter(!is.na(ba)) %>% 
  mutate(year = as.factor(year)) %>%
  mutate(rank = as.factor(ifelse(state_id %in% top_ten_98$state_id, 1, 0))) %>%  
  ggplot(aes(year, ba, group = state_id, color = rank, alpha = rank)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(1)) + 
  theme(text = element_text(family = "Times New Roman")) +
  scale_x_discrete(expand = expand_scale(mult = c(.05,.2))) +
  scale_color_manual(values = c("#cccccc", "#000000"), 
                     guide = FALSE) +
  scale_alpha_manual(values = c(.4, .8),
                     guide = FALSE) +
  geom_text_repel(aes(year, ba, 
                      label = ifelse(year == "2013" & state_id %in% top_ten_98$state_id,
                                     state_id, NA)),
                  family = "Times New Roman",
                  size = 2,
                  segment.colour = NA,
                  nudge_x = .04) +
  labs(x = NULL, 
       y = NULL)

## PLOT 2
plot_two <- avg_props_id %>% 
  filter(year == 1998 | year == 2013) %>%
  filter(!is.na(ba)) %>% 
  mutate(year = as.factor(year)) %>%
  mutate(rank = as.factor(ifelse(state_id %in% bottom_ten_98$state_id, 1, 0))) %>%  
  ggplot(aes(year, ba, group = state_id, color = rank, alpha = rank)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(1)) + 
  scale_x_discrete(expand = expand_scale(mult = c(.05,.2))) +
  theme(text = element_text(family = "Times New Roman")) +
  scale_color_manual(values = c("#cccccc", "#000000"), 
                     guide = FALSE) +
  scale_alpha_manual(values = c(.4, .8),
                     guide = FALSE) +
  geom_text_repel(aes(year, ba, 
                      label = ifelse(year == "2013" & state_id %in% bottom_ten_98$state_id,
                                     state_id, NA)),
                  family = "Times New Roman",
                  size = 2,
                  segment.colour = NA,
                  nudge_x = .04) +
  labs(x = NULL, 
       y = NULL)

gt <- arrangeGrob(plot_one, plot_two, ncol = 2,
                  top = textGrob("Figure 6: Proportional Basic Assistance Spending in FY 1998 and FY 2013",
                                 gp = gpar(fontsize = 11,
                                           fontfamily = "Times New Roman"),
                                 hjust = .62),
                  bottom = textGrob("Note: South Carolina and Tennessee removed due to negative reported basic assistance expenditures in FY 1998. See appendix for more information.",
                                    gp = gpar(fontsize = 7,
                                              fontfamily = "Times New Roman"),
                                    hjust = .45))
ggsave("Figures and Tables/Figure6.pdf", gt, height = 5, width = 6.5, units = "in")  

# Table 1 ####

# Model 1 : All variables - no time effects 
p1 <- plm(ba ~ african_americans + hispanics + fiscal_stability + caseload + liberalism + wpr + 
            unemployment + pcpi_regional,
          data = avg_props_pdata, 
          model = "within", 
          effect = "individual")

# Model 2 : All variables - time effects 
p2 <- plm(ba ~ factor(year) + african_americans + hispanics + fiscal_stability + caseload + liberalism + wpr + 
            unemployment + pcpi_regional,
          data = avg_props_pdata, 
          model = "within", 
          effect = "individual")

stargazer(p1, p2,
          title = "Table 1: Regression Output",
          column.labels = c("Model 1", "Model 2"),
          covariate.labels = c("Percent of caseload that is Black", 
                               "Percent of caseload that is Hispanic", 
                               "Fiscal balance as a percent of spending",
                               "Percent change in caseload",
                               "Government liberalism",
                               "Work participation rate", 
                               "Unemployment rate", 
                               "Per capita income (in thousands)"),
          dep.var.labels = "Basic Assistance Expenditures as a Percentage of Total TANF Expenditures",
          omit = "year",
          header = FALSE,
          omit.labels = c("Time Fixed Effects"),
          star.cutoffs = c(.05),
          notes = "*p < 0.05",
          notes.append = FALSE,
          notes.align = "r",
          model.numbers = FALSE,
          initial.zero = FALSE,
          column.sep.width = "1pt",
          font.size = "small",
          type = "latex",
          out = "Figures and Tables/Table1.html")

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
          covariate.labels = c("Percent of caseload that is Black", 
                               "Percent of caseload that is Hispanic", 
                               "Fiscal balance as a percent of spending",
                               "Percent change in caseload",
                               "Government liberalism",
                               "Work participation rate", 
                               "Unemployment rate", 
                               "Per capita income (in thousands)"),
          omit = "year",
          omit.labels = c("Time Fixed Effects"),
          notes.align = "l",
          star.cutoffs = c(.05),
          notes = "*p < 0.05",
          notes.append = FALSE,
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























