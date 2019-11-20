library(tidytext)
library(tidyverse)

# should also make a version the has a words on each line
# and retains the grant it appears in

# import
words_by_year <- read.table("processed/words_by_year.txt",
                            header = TRUE,
                            stringsAsFactors = FALSE)

# how many years are there
table(words_by_year$start_year) %>% length()

# what is the distribution of frequencies?

ggplot(words_by_year, aes(n)) +
  geom_density()
# some words are very common

words_by_year %>% 
  filter(n > 300) %>%
ggplot(aes(x = word, y = n)) +
  geom_col() +
  theme(axis.text.y = element_text(size = 5)) +
  coord_flip() +
  facet_wrap(. ~ start_year, nrow = 2)
