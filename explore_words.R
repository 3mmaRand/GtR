###################################
# Exploring word freq table#
###################################

## Packages
library(tidytext)
library(tidyverse)

## Data Description
# these data were generated from the filtered set of
# 43761 grants using tidytext::unnest_tokens() and
# stop_words(). The word frequencies are given by
# start year, funding body and research organisation
# there will be many zero many ro, rc and year 
# combinations will have words with zero frequencies

# import
file <- "tidy_texts/words_by_year_rc_ro.txt"
words <- read.table(file,
                    header = TRUE,
                    stringsAsFactors = FALSE)

# create set that summs over the ro
words_no_ro <- words %>% 
  group_by(i_funding_org_name, start_year, word) %>% 
  summarise(n2 = sum(n))

ggplot(words, aes(n)) +
  geom_histogram() +
  facet_wrap(. ~i_funding_org_name)
# some words are very common

words_by_year %>% 
  filter(n > 300) %>%
ggplot(aes(x = word, y = n)) +
  geom_col() +
  theme(axis.text.y = element_text(size = 5)) +
  coord_flip() +
  facet_wrap(. ~ start_year, nrow = 2)
