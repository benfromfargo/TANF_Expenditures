test <- avg_props %>% 
  mutate(not_ba = cc + pregnancy +
           shortben + tax +
           work) %>% 
  mutate(other = admin + other + prior + ssbg) %>% 
  select(STATE, year, not_ba, ba, other)

test <- gather(test, key = "category", value = "value", -c("year","STATE"))

test %>%
  ggplot(aes(ba, not_ba)) +
  geom_point()


test <- data.frame(avg_props_pdata) %>% 
  mutate(not_ba = cc + pregnancy +
           shortben + tax +
           work) %>% 
  mutate(other = admin + other + prior + ssbg) %>% 
  select(STATE, year, not_ba, ba, other, liberalism, african_americans) %>% 
  filter(year == 2013)

test %>%
  ggplot(aes(ba, not_ba, color = liberalism)) +
  geom_point()

