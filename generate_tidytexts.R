library(tidyverse)
library(tidytext)
# import
file <- "processed/grants.txt"
grants <- read.table(file, header = T, 
                     sep = " ", 
                     stringsAsFactors = FALSE)
# number of grants per RC
grants %>% 
  group_by(i_funding_org_name) %>% 
  summarise(n = length(i_funding_org_name))


# creating word frequency tables
# grouping by year, research council, and reasearch organisation
# only took about 2 minutes on this set up
words <- grants %>%
  group_by(start_year, 
           i_funding_org_name,
           lead_ro_name) %>% 
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) 
# write to file
file <- "tidy_texts/words_by_year_rc_ro.txt"
write.table(words, file)

# zip processed data files for uploading to github
files2zip <- dir("tidy_texts/", full.names = TRUE)
zip(zipfile = "tidy_texts_data.zip", files = files2zip)

