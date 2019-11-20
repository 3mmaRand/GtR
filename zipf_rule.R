
# nc3rs

total_words <- words_by_years %>% 
  group_by(start_year) %>% 
  summarise(total = n())

words_by_years <- left_join(words_by_years,total_words)

lm()

freq_by_rank <- words_by_years %>% 
  group_by() %>% 
  mutate(rank = row_number(),
         term_freq = n/total)
ggplot(data = freq_by_rank, 
       aes(x = rank, y = term_freq, colour = factor(start_year))) +
  geom_point() + 
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm")
