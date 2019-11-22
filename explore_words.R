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


# Frequency of words relative to 2006
# subset the BBSRC and remove 2020
frequency <- words_by_year_rc %>% 
  filter(i_funding_org_name == "BBSRC") %>% 
  filter(start_year != "2020") %>% 
  count(start_year, word) %>%
  group_by(start_year) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(start_year, proportion) %>% 
  gather(start_year, proportion, `2007`:`2019`)


library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, 
                      y = `2006`,
                      color = abs(`2006` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~start_year, nrow = 2) +
  theme(legend.position = "none") +
  labs(y = "2006", x = NULL)

# Frequency of words 2015 and 2018 relative to 2006
# subset the BBSRC and remove 2020
frequency <- words_by_year_rc %>% 
  filter(i_funding_org_name == "BBSRC") %>% 
  filter(start_year %in% c("2006", "2010", "2018")) %>% 
  count(start_year, word) %>%
  group_by(start_year) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(start_year, proportion) %>% 
  gather(start_year, proportion, c(`2010`,`2018`))


library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, 
                      y = `2006`,
                      color = abs(`2006` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_point(alpha = 0.1, size = 1) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  # scale_color_gradient(limits = c(0, 0.001), 
  #                      low = "darkslategray4", high = "gray75") +
  facet_wrap(~start_year, nrow = 1) +
  theme(legend.position = "none") +
  labs(y = "2006", x = NULL)








