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
file <- "tidy_texts/words_by_year_rc.txt"
words_by_year_rc <- read.table(file,
                    header = TRUE,
                    stringsAsFactors = FALSE)



ggplot(words_by_year_rc, aes(n)) +
  geom_histogram() +
  facet_wrap(. ~i_funding_org_name,scales = "free")
# some words are very common, many words are not ubiquitous (many zeros)



words_by_year_rc %>% 
  filter(i_funding_org_name == "BBSRC") %>%
  ggplot(aes(n)) +
  geom_histogram()

words_by_year_rc %>% 
  filter(i_funding_org_name == "BBSRC") %>%
  ggplot(aes(n)) +
  geom_histogram() +
  facet_wrap(. ~start_year,scales = "free")

words_by_year_rc %>% 
  filter(i_funding_org_name == "BBSRC") %>%
  filter(n > 300) %>%
  ggplot(aes(n)) +
  geom_histogram()


words_by_year_rc %>% 
  filter(i_funding_org_name == "BBSRC") %>%
  filter(n > 400) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  theme(axis.text.y = element_text(size = 6)) +
  coord_flip()

words_by_year_rc %>% 
  filter(i_funding_org_name == "BBSRC") %>%
  filter(n > 400) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  theme(axis.text.y = element_text(size = 6)) +
  coord_flip() +
  facet_wrap(. ~ start_year)

words_by_year_rc %>% 
  filter(i_funding_org_name == "BBSRC") %>%
  filter(n > 400) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  theme(axis.text.y = element_text(size = 6)) +
  coord_flip()





