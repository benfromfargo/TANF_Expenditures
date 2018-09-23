########### TEST ###########
library(lfe)
library(stargazer)
library(readstata13)

source("TANF_clean.r")

test <- read.dta13("caughey_warshaw_summary.dta") %>% 
  as.tibble() %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  mutate(stpo = as.character(stpo)) %>% 
  rename(state_id = stpo) %>% 
  filter(year > 1996) %>% 
  mutate(year = year + 1) %>% #IV t - 1 == DV t
  filter(year < 2014) %>% 
  select(1:8) %>% 
  mutate(year = as.character(year))

avg_props_pdata <- avg_props_pdata %>% 
  filter(STATE != "DIST.OF COLUMBIA") %>%
  ungroup() %>% 
  arrange(year) %>% 
  mutate(state_id = rep(state.abb, 16)) %>% 
  mutate(service = cc + pregnancy + shortben + tax + work) %>% 
  mutate(ba_dif = ba - service) %>% 
  arrange(STATE) %>% 
  group_by(STATE) %>% 
  mutate(ba_dif_before = lag(ba_dif)) %>% 
  ungroup()

avg_props_pdata <- left_join(test, avg_props_pdata, by = c("year", "state_id"))
anti_join(test, avg_props_pdata, by = c("year", "state_id"))

p1 <- felm(ba_dif ~ african_americans + hispanics + policyeconlib_est + unemployment + 
             pcpi_regional + fiscal_stability + caseload + wpr
           | STATE + year | 0 | STATE, 
           data = avg_props_pdata)

p2 <- felm(ba_dif ~ african_americans + hispanics + policyeconlib_est + unemployment + 
            pcpi_regional + fiscal_stability + caseload + wpr + ba_dif_before
           | STATE + year | 0 | STATE, 
           data = avg_props_pdata)

stargazer(p1, p2,
          title = "Regression Output",
          column.labels = c("Model 1", "Model 2"),
          covariate.labels = c("Percent of caseload that is African American", 
                               "Percent of caseload that is Hispanic",
                               "Policy liberalism",
                               "Unemployment rate", 
                               "Per capita income (in thousands)",
                               "Fiscal balance as a percent of spending",
                               "Percent change in caseload",
                               "Work participation rate",
                               "Difference in prior year"),
          dep.var.labels = "Basic assistance minus Work-related supports, services, and in-kind benefits",
          add.lines = list(c("Time FEs", "Yes", "Yes"),
                           c("State FEs", "Yes", "Yes")),
          header = FALSE,
          star.cutoffs = c(.05),
          notes = "*p < 0.05; SEs clustered by state",
          notes.append = FALSE,
          notes.align = "r",
          model.numbers = FALSE,
          initial.zero = FALSE,
          column.sep.width = "1pt",
          font.size = "small",
          type = "latex",
          out = "Figures and Tables/Table1_test.html")

boom <- spread(ann_means_vis, category, value) %>% 
  mutate(dif = ba - service)

boom %>% 
  ggplot(aes(year, dif, group = 1)) +
  geom_line() +
  labs(title = "Basic assistance minus WORSSI",
       subtitle = "FY 1998 - 2013",
       x = NULL, 
       y = NULL) +
  scale_y_continuous(labels = scales::percent_format(1),
                     limits = c(-.20,.40),
                     breaks = seq(-.20, .40, .20),
                     expand = c(0,0)) +
  scale_x_discrete(breaks = c("1998", "2003", "2008", "2013"),
                   labels = c("1998", "2003", "2008", "2013"))





