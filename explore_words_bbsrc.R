
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
  filter(i_funding_org_name == "BBSRC") %>% 
  select(-i_funding_org_name)

# drop words that are just numbers
words_by_year_rc <- words_by_year_rc %>%
  mutate(number_word = as.numeric(word))

words_by_year_rc <- words_by_year_rc %>%
  filter(is.na(number_word)) %>% 
  select(-number_word)



# summarise across years
# n_all_years is the number of times the 
# word has been used in total
words_by_year_rc <- words_by_year_rc %>% 
  group_by(word) %>% 
  mutate(n_all_years = sum(n))

words_by_year_rc <- words_by_year_rc %>% 
  group_by(start_year) %>% 
  mutate(words_this_year = sum(n))

words_by_year_rc <- words_by_year_rc %>% 
  mutate(prev_this_yr_relative_overall_prev = n / n_all_years,
         prev_this_yr_relative_words_this_yr = n / words_this_year)


ggplot(words_by_year_rc, aes(x = prev_this_yr_relative_overall_prev)) +
  geom_density() +
  facet_wrap(~start_year)

ggplot(words_by_year_rc, aes(x = prev_this_yr_relative_words_this_yr)) +
  geom_density() +
  facet_wrap(~start_year)
  
ggplot(words_by_year_rc, 
       aes(x = prev_this_yr_relative_overall_prev,
           y = prev_this_yr_relative_words_this_yr)) +
  geom_point(alpha = 0.1) +
  facet_wrap(.~ start_year) 


