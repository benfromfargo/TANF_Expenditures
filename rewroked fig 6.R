library(gghighlight)
library(ggrepel)
library(gridExtra)
library(grid)

top_ten_98 <- avg_props_id %>% 
  filter(ba, year == 1998) %>%
  filter(!is.na(ba)) %>%
  top_n(10, ba,)

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

