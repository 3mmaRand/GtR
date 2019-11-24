
## Packages
library(tidytext)
library(tidyverse)

## Data Description
# these data were generated from the filtered set of
# 43761 grants using tidytext::unnest_tokens() and
# stop_words(). 


# import word frequencies by year and rc
file <- "tidy_texts/words_by_year_rc.txt"
words_by_year_rc <- read.table(file,
                    header = TRUE,
                    stringsAsFactors = FALSE)

# filter for BBSRC only
words_by_year_rc <- words_by_year_rc %>% 
  filter(i_funding_org_name == "BBRSC") %>% 
  select(-i_funding_org_name)

# summarise across years
# n_all_years is the number of times the 
# word has been used in total
words_by_year_rc <- words_by_year_rc %>% 
  group_by(word) %>% 
  mutate(n_all_years = sum(n))

# total_words_per_year is ..
words_by_year_rc <- words_by_year_rc %>% 
  group_by(start_year) %>% 
  mutate(total_words_per_year = sum(n))

# frequency of a word in a year relative to it's frequency across
# all years
# high numbers means it is common this year relative to how common it was
# in other years
# 1 means it has only come up in 1 year
words_by_year_rc <- words_by_year_rc %>% 
  mutate(yr_freq_relative_overall_freq = n / n_all_years)

# frequency this year relative to number of words in the year
# high numbers means it is common this year compared to other
# words
words_by_year_rc <- words_by_year_rc %>% 
  mutate(freq_relative_other_words_this_year = n / total_words_per_year)

words_by_year_rc <- words_by_year_rc %>% 
  mutate(relative_freq = n / sum(n))


ggplot(words_by_year_rc, 
       aes(x = yr_freq_relative_overall_freq,
           y = freq_relative_other_words_this_year)) +
  geom_point(alpha = 0.1) +
  xlab("Frequency of a word in a year relative to it's frequency across all years") +
  ylab("Frequency of a word in years relative to number of words in that year") +
  facet_wrap(.~ start_year, nrow = 3) +
  scale_x_log10() +
  scale_y_log10()


ggplot(words_by_year_rc, 
       aes(x = yr_freq_relative_overall_freq,
           y = freq_relative_other_words_this_year)) +
  geom_point(alpha = 0.1) +
  xlab("Frequency of a word in a year relative to it's frequency across all years") +
  ylab("Frequency of a word in years relative to number of words in that year") +
  facet_wrap(.~ start_year, nrow = 3) +
  scale_x_log10() +
  scale_y_log10()