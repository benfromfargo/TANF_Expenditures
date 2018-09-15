source("TANF_clean.R")

library(ggrepel)
library(gtable)
library(grid)
library(stargazer)
library(extrafont)
library(plm)

# Figure 1 ####
## @knitr Figure.1
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
  scale_y_continuous(labels = scales::percent,
                     expand = c(0,.02),
                     limits = c(0, .65)) +
  scale_color_manual(values = c("#000000", "#000000", "#000000"),
                     guide = FALSE) +
  theme(plot.caption = element_text(size = 7, hjust = 0),
        text = element_text(family = "Times New Roman")) +
  labs(caption = "Note: See Table 3 in the appendix for category groups. Percentages may not add up to 100% in a given fiscal year due to the removal of outlier values. 
See appendix for more information.",
       x = NULL,
       y = NULL) +
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

# Figure 2 ####
## @knitr Figure.2
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
  scale_x_discrete(breaks = c("1998", "2003", "2008", "2013")) +
  geom_text(aes(label = label),
            na.rm = TRUE,
            hjust = 0, 
            family = "Times New Roman",
            size = 3) +
  theme(plot.margin = unit(c(1,12,1,1), "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(family = "Times New Roman")) 

gt <- ggplotGrob(x)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

# Figure 3 ####
## @knitr Figure.3
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

suppressWarnings(avg_props_id %>% 
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
                   theme(axis.title = element_blank(), 
                         legend.position = "none",
                         text = element_text(family = "Times New Roman")) +
                   geom_point(aes(year, outlier2)) +
                   geom_text_repel(aes(year, outlier2, 
                                       label = outlier), 
                                   size = 2, 
                                   nudge_x = .15) +
                   scale_y_continuous(labels = scales::percent))

# Figure 4 ####
## @knitr Figure.4
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
  labs(caption = "Note: South Carolina and Tennessee removed due to negative reported basic assistance expenditures in FY 1998. See appendix for more information.") +
  theme(plot.caption=element_text(size=7, hjust = 0), legend.text = element_text(size = 8),
        text = element_text(family = "Times New Roman"), legend.key = element_rect(size = 7),
        legend.key.size = unit(2, 'lines'))

# Table 1 ####
## @knitr Table.1
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
          title = "Regression Output",
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
          type = "latex")


# Figure 5 ####
## @knitr Figure.5
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
  scale_x_discrete(breaks = c("1998", "2003", "2008", "2013")) +
  labs(caption = "Note: Error bars represent 95% confidence intervals.") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(family = "Times New Roman"),
        plot.caption=element_text(size=7, hjust = 0))

# Table 2 ####
## @knitr Table A.1
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
          title = "Comparing Regression Output Across Three Data Cleaning Methods", 
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
          type = "latex")










