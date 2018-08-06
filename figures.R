source("TANF_clean.R")

library(ggrepel)
library(gtable)
library(grid)
library(stargazer)
library(extrafont)

# Figure 1 ####
## @knitr Figure.1
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
        text = element_text(family = "Times New Roman"),
        plot.caption = element_text(hjust = 0,
                                    size = 6)) +
  labs(title = "Figure 1 - Mean TANF Spending as Percentage of Total Expenditures",
       subtitle = "FY 1998 - 2013",
       caption = "\"Other\" includes administration and systems, expenditures under prior law, other non-assistance, and social services block grant categories;
\"Aid that is not basic assistance\" includes child care, marriage and pregnancy programs, diversion benefits, refundable tax credits, 
and work-related activities and supports categories. The percentages in Figure 1 may not add up to 100% in a given fiscal year due to the removal of 
outlier values (i.e., proportional expenditure values that remained above 100% or below 0% after calculating moving averages). 
See Table A.2 in the Appendix for a complete list of annual mean expenditures by year and category.")


# Figure 2 ####
## @knitr Figure.2
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
  labs(title = "Figure 2 - Mean Proportional Expenditures of Aid that is not Basic Assistance",
       subtitle = "FY 1998 - 2013") +
  theme(plot.margin = unit(c(2,12,2,2), "lines"),
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
                   scale_x_discrete(name = "", 
                                    breaks = c("2000", "2005", "2010")) +
                   theme(axis.title = element_blank(), 
                         legend.position = "none",
                         text = element_text(family = "Times New Roman")) +
                   geom_point(aes(year, outlier2)) +
                   geom_text_repel(aes(year, outlier2, 
                                       label = outlier), 
                                   size = 2, 
                                   nudge_x = .15) +
                   scale_y_continuous(labels = scales::percent) +
                   labs(title = "Figure 3 - Proportional Basic Assistance Expenditures by State",
                        subtitle = "FY 1998 - 2013"))

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
                     labels = c("Ten highest spending\nstates in FY 1998", "Ten lowest spending\nstates in FY 1998")) +
  scale_alpha_manual(values = c(.4, .8, .8),
                     guide = "none") +
  labs(title = "Figure 4 - Propotional Basic Assistance Expenditures by Spending Level",
       subtitle = "FY 1998 and 2013",
       caption = "South Carolina and Tennessee removed due to negative reported basic assistance expenditures in FY 1998. See appendix.") +
  theme(plot.caption=element_text(size=6),
        text = element_text(family = "Times New Roman"))

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
          covariate.labels = c(NA, NA, NA, NA, NA, NA, NA, "pcpi regional"),
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
  labs(title = "Figure 5 - Coefficients of Time Fixed Effects from Model 4",
       subtitle = "FY 1999 - 2013",
       caption = "Error bars represent 95% confidence intervals") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(family = "Times New Roman"))