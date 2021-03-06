############### Front matter ########################
source("TANF_clean.R")

library(ggrepel)
library(gtable)
library(grid)
library(stargazer)
library(extrafont)
library(plm)
library(gridExtra)
library(readstata13)

my_theme <- theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.major.y = element_line(colour = "#dedddd"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())
theme_set(my_theme)

######################################################
################## Figure 1 ##########################
######################################################
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
  geom_line(aes(linetype = category)) +
  scale_x_discrete(breaks = c("1998", "2003", "2008", "2013")) +
  scale_y_continuous(labels = scales::percent_format(1),
                     expand = c(0, 0),
                     limits = c(0, .6),
                     breaks = seq(0, .6, .1)) +
  scale_color_manual(values = c("#000000", "#000000", "#000000"),
                     guide = FALSE) +
  scale_linetype_manual(values = c(2, 4, 1), 
                          guide = FALSE) +
  theme(plot.caption = element_text(size = 7, hjust = 0)) +
  labs(caption = "Note: See Table 3 in the appendix for a list of the spending categories that compose each spending type. Percentages may not add up to 100% 
in a given fiscal year due to the removal of outlier values. Refer to the appendix for more information.",
       x = NULL,
       y = NULL) +
  annotate("text", "2008", .23, 
           label = "Basic assistance", 
           hjust = 0,
           size = 3,
           family = "Times New Roman") +
  annotate("text", "2008", .335, 
           label = "Other spending", 
           hjust = 0,
           size = 3,
           family = "Times New Roman") +
  annotate("text", "2008", .47, 
           label = "Work-related, in-kind,\nand short-term benefits", 
           hjust = 0,
           size = 3,
           family = "Times New Roman")

######################################################
################## Figure 2 ##########################
######################################################
## @knitr Figure.2

x <- ann_means %>% 
  filter(category != "ba" & category != "admin" & category != "other" & category != "prior" & category != "ssbg") %>%
  mutate(label = ifelse(year == "2013", category, NA)) %>% 
  mutate(label = case_when(
    label == "cc" ~ "     Child care",
    label == "work" ~ "     Work-related activities\n     and supports", 
    label == "pregnancy" ~ "     Marriage and pregnancy\n     programs",
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
  theme(plot.margin = unit(c(1,12,1,1), "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 

gt <- ggplotGrob(x)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

######################################################
################## Figure 3 ##########################
######################################################
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
                         legend.position = "none") +
                   geom_point(aes(year, outlier2)) +
                   geom_text_repel(aes(year, outlier2, 
                                       label = outlier), 
                                   size = 2, 
                                   nudge_x = .05,
                                   segment.color = NA) +
                   scale_y_continuous(labels = scales::percent_format(1),
                                      expand = c(0,0),
                                      breaks = seq(0, 1, .25),
                                      limits = c(0,1)))

######################################################
################## Figure 4 ##########################
######################################################
## @knitr Figure.4

top_ten_98 <- avg_props_id %>% 
  filter(ba, year == 1998) %>%
  filter(!is.na(ba)) %>%
  top_n(10, ba)

bottom_ten_98 <- avg_props_id %>% 
  filter(year == 1998) %>%
  filter(!is.na(ba)) %>% 
  top_n(-10, ba)

#################### PLOT 1 ###########################

plot_one <- avg_props_id %>% 
  filter(year == 1998 | year == 2013) %>%
  filter(!is.na(ba)) %>% 
  mutate(year = as.factor(year)) %>%
  mutate(rank = as.factor(ifelse(state_id %in% top_ten_98$state_id, 1, 0))) %>%  
  ggplot(aes(year, ba, group = state_id, color = rank, alpha = rank)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(1),
                     expand = c(0,0),
                     breaks = seq(0, 1, .25),
                     limits = c(0,1)) + 
  theme(text = element_text(family = "Times New Roman"),
        plot.subtitle = element_text(hjust = .5, size = 10)) +
  scale_x_discrete(expand = expand_scale(mult = c(.05,.2))) +
  scale_color_manual(values = c("#cccccc", "#000000"), 
                     guide = FALSE) +
  scale_alpha_manual(values = c(.5, 1),
                     guide = FALSE) +
  geom_text_repel(aes(year, ba, 
                      label = ifelse(year == "2013" & state_id %in% top_ten_98$state_id,
                                     state_id, NA)),
                  family = "Times New Roman",
                  size = 2,
                  segment.size = .2,
                  direction = "y",
                  nudge_x = .1,
                  hjust = 0) +
  labs(x = NULL, 
       y = NULL,
       subtitle = "Ten highest spending states in FY 1998")

#################### PLOT 2 #############################

plot_two <- avg_props_id %>% 
  filter(year == 1998 | year == 2013) %>%
  filter(!is.na(ba)) %>% 
  mutate(year = as.factor(year)) %>%
  mutate(rank = as.factor(ifelse(state_id %in% bottom_ten_98$state_id, 1, 0))) %>%  
  ggplot(aes(year, ba, group = state_id, color = rank, alpha = rank)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(1),
                     expand = c(0,0),
                     breaks = seq(0, 1, .25),
                     limits = c(0,1)) + 
  scale_x_discrete(expand = expand_scale(mult = c(.05,.2))) +
  theme(text = element_text(family = "Times New Roman"),
        plot.subtitle = element_text(hjust = .5, size = 10)) +
  scale_color_manual(values = c("#cccccc", "#000000"), 
                     guide = FALSE) +
  scale_alpha_manual(values = c(.5, 1),
                     guide = FALSE) +
  geom_text_repel(aes(year, ba, 
                      label = ifelse(year == "2013" & state_id %in% bottom_ten_98$state_id,
                                     state_id, NA)),
                  family = "Times New Roman",
                  size = 2,
                  segment.size = .2,
                  direction = "y",
                  nudge_x = .1,
                  hjust = 0) +
  labs(x = NULL, 
       y = NULL,
       subtitle = "Ten lowest spending states in FY 1998")

grid.arrange(plot_one, plot_two, ncol = 2,
                  bottom = textGrob("Note: South Carolina and Tennessee removed due to negative reported basic assistance expenditures in FY 1998. See appendix for more information.",
                                    gp = gpar(fontsize = 7,
                                              fontfamily = "Times New Roman"),
                                    hjust = .48))

######################################################
################## Table 1 ###########################
######################################################
## @knitr Table.1
panel_99 <- avg_props_pdata %>% 
  filter(year == 1999)
panel_05 <- avg_props_pdata %>% 
  filter(year == 2005)
panel_13 <- avg_props_pdata %>% 
  filter(year == 2013)

l1 <- lm(log(ba) ~ african_americans + hispanics + log(liberalism) + unemployment +
           log(pcpi_regional) + fiscal_stability + log(caseload) + wpr, 
         data = panel_99)
l2 <- lm(log(ba) ~ african_americans + hispanics + log(liberalism) + unemployment +
           log(pcpi_regional) + fiscal_stability + log(caseload) + wpr, 
         data = panel_05)
l3 <- lm(log(ba) ~ african_americans + hispanics + log(liberalism) + unemployment +
           log(pcpi_regional) + fiscal_stability + log(caseload) + wpr, 
         data = panel_13)

table_1 <- capture.output(stargazer(l1, l2, l3, 
          title = "Cross-Sectional Regression Models, FY 1999, 2005, and 2013",
          column.labels = c("1999", "2005", "2013"),
          covariate.labels = c("Percent African American", 
                               "Percent Hispanic",
                               "Liberalism\\textsuperscript{\\textdagger}",
                               "Unemployment rate", 
                               "Real per capita income\\textsuperscript{\\textdagger}",
                               "Fiscal balance",
                               "Caseload size\\textsuperscript{\\textdagger}",
                               "Work participation rate"),
          dep.var.labels = "TANF funds spent on basic assistance\\textsuperscript{\\textdagger}",
          notes = "\\multicolumn{4}{l} {\\parbox[t]{13cm}{ \\textit{Notes:} \\textsuperscript{\\textdagger}variable is logged; observations may be less than 51 due to missing values; *p < 0.05}}",
          notes.append = FALSE,
          notes.align = "l",
          header = FALSE,
          model.numbers = FALSE,
          initial.zero = FALSE,
          star.cutoffs = .05,
          type = "latex",
          font.size = "small",
          table.placement = "H",
          out = "Figures and Tables/Table1.html"))

note.latex <- "\\multicolumn{4}{l} {\\parbox[t]{13cm}{ \\textit{Notes:} \\textsuperscript{\\textdagger}variable is logged; observations may be less than 51 due to missing values; *p < 0.05}} \\\\"
table_1[str_detect(table_1, "Note")] <- note.latex
cat(table_1, sep = "\n")

######################################################
################## Table 2 ###########################
######################################################
## @knitr Table.2
p1 <- plm(log(ba) ~ african_americans + hispanics + log(liberalism) + unemployment +
           log(pcpi_regional) + fiscal_stability + wpr + factor(year), 
          model = "fd", 
          index = c("state", "year"),
         data = avg_props_pdata)

p2 <- plm(log(ba) ~ african_americans + hispanics + log(liberalism) + unemployment +
            log(pcpi_regional) + fiscal_stability + wpr + factor(year), 
          model = "within", 
          index = c("state", "year"),
          data = avg_props_pdata)

p3 <- plm(log(ba) ~ african_americans + hispanics + log(liberalism) + unemployment +
            log(pcpi_regional) + fiscal_stability + wpr + factor(year) + lag(log(ba), 2) 
          + lag(log(ba), 3), 
          model = "fd", 
          index = c("state", "year"),
          data = avg_props_pdata)

p1$vcov <- vcovHC(p1, type="HC0", method = "arellano", cluster = "group")
p2$vcov <- vcovHC(p2, type="HC0", method = "arellano", cluster = "group")
p3$vcov <- vcovHC(p3, type="HC0", method = "arellano", cluster = "group")
# http://www.princeton.edu/~otorres/Panel101R.pdf

table_2 <- capture.output(stargazer(p1, p2, p3, 
          title = "Panel Regression Models",
          column.labels = c("First differences", "Time-demeaned", "Lagged dependent variable"),
          covariate.labels = c("Percent African American", 
                               "Percent Hispanic",
                               "Liberalism\\textsuperscript{\\textdagger}",
                               "Unemployment rate", 
                               "Real per capita income\\textsuperscript{\\textdagger}",
                               "Fiscal balance",
                               "Work participation rate",
                               "Lagged DV (t - 2)\\textsuperscript{\\textdagger}",
                               "Lagged DV (t - 3)\\textsuperscript{\\textdagger}"),
          dep.var.labels = "TANF funds spent on basic assistance\\textsuperscript{\\textdagger}",
          notes = "\\textsuperscript{\\textdagger}variable is logged; observations with missing values are dropped; *p < 0.05; standard errors are clustered by state and are robust to serial correlation and heteroskedasticity",
          add.lines = list(c("Time Fixed Effects", "Yes", "Yes", "Yes"),
                           c("Individual Fixed Effects", "Yes", "Yes", "Yes")),
          notes.append = FALSE,
          omit = "year",
          header = FALSE,
          notes.align = "r",
          model.numbers = FALSE,
          initial.zero = FALSE,
          star.cutoffs = .05,
          column.sep.width = "1pt",
          font.size = "small",
          type = "latex",
          table.placement = "H",
          out = "Figures and Tables/Table2.html"))

note.latex_2 <- "\\multicolumn{4}{l} {\\parbox[t]{13cm}{ \\textit{Notes:} \\textsuperscript{\\textdagger}variable is logged; observations with missing values are dropped; *p < 0.05; standard errors are clustered by state and are robust to serial correlation and heteroskedasticity}} \\\\"
table_2[str_detect(table_2, "Note")] <- note.latex_2
cat(table_2, sep = "\n")

#avg_props_pdata %>% 
#  group_by(state) %>% 
#  summarise(sd = sd(hispanics, na.rm = TRUE)) %>% 
#  ungroup() %>% 
#  summarise(mean_sd = mean(sd))


#test <- plm(log(pcpi_regional) ~ unemployment + factor(year), 
#          model = "within", 
#          index = c("state", "year"),
#          data = avg_props_pdata)
#stargazer(test, 
#          omit = "year",
#          type = "text")



