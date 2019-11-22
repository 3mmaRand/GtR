
# BBSRC

words_by_year_bbsrc <- words_by_year_rc %>% 
  filter(i_funding_org_name == "BBSRC") 


total_words <- words_by_year_bbsrc %>% 
  group_by(start_year) %>% 
  summarise(total = n())

words_by_year_bbsrc <- left_join(words_by_year_bbsrc,
                                 total_words)



freq_by_rank <- words_by_year_bbsrc %>% 
  group_by() %>% 
  mutate(rank = row_number(),
         term_freq = n/total)
ggplot(data = freq_by_rank, 
       aes(x = rank,
           y = term_freq)) +
  geom_point() + 
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm")
