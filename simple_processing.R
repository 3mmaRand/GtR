library(tidyverse)
library(lubridate)

# import the data
file <- "data/grants_3-11-19.csv"
grants <- read.csv(file, stringsAsFactors = FALSE)


################################
# Creates derived variables
################################

# add a colum which creates class Dates variables from
# the class character formated dates 
# "start_date_2" "end_date_2"
grants$start_date_2 <- dmy(grants$start_date)
grants$end_date_2 <- dmy(grants$end_date)

# add a column for the start year
# "start_year"
grants$start_year <- year(grants$start_date_2)

# calulate the grant duration
#  "duration_days"
#  first check whether there are any missing start of end dates
summary(grants$start_date_2)
summary(grants$end_date_2)
# there are not
# now calculate difference
grants$duration_days <- difftime(grants$end_date_2,
                                 grants$start_date_2,
                                 units = "days") %>%
  as.numeric()

# calcuate award money per day
# "award_per_day"
grants$award_per_day <- grants$award_pounds / grants$duration_days
summary(grants$award_per_day)

# Determine which grants have zero length
summary(grants$duration_days)
# no grants have NA length
zerolength <- grants %>% 
  filter(duration_days == 0)
# 8 grants had zero length
# these arise where start and end are the same

# write zero length grants to file
file <- "processed/zerolength.txt"
write.table(zerolength, file)


# Determine which grants have zero money
summary(grants$award_pounds)
# some awards have NA pounds 19, some have 0 pounds

zeromoney <- grants %>% 
  filter(award_pounds == 0)
# 8 grants have zero money
file <- "processed/zeromoney.txt"
write.table(zeromoney, file)

namoney <- grants %>% 
  filter(is.na(award_pounds))
# NA money grants have 'expenditure' (19 grants)
# write na money grants to file
file <- "processed/namoney.txt"
write.table(namoney, file)

# determine which grants have missing abstracts
missing <- grants$abstract[1658]
# number choice an arbitrary row which has the value:
# "Abstracts are not currently available in GtR for all funded research. This is normally because the abstract was not required at the time of proposal submission, but may be because it included sensitive information such as personal details."
# in the abstracts column

grant_missing <- grants %>% 
  filter(abstract == missing)
# there are 4271 cases with the placeholder abstract
# write the missing abstract cases to file
file <- "processed/grant_missing.txt"
write.table(grant_missing, file)


################################
# Creates filtered dataset
################################

# filtering as of 20-11-19:
# zero money, na money, zero duration, no abstract

# filter out the 8 zero length and 8 zero money grants
# and the 19 NA money grants
grants <- grants %>% 
  filter(duration_days > 0) %>% 
  filter(award_pounds > 0) %>% 
  filter(!is.na(award_pounds)) %>% 
  filter(abstract != missing)
# grants remain 43761

# write results to file
file <- "processed/grants.txt"
write.table(grants, file)

# TO DO: get rid of non-words like &quot
# 
# 
# TO DO: replacement acronyms with full words
# 
#  
# TO DO: deal with cases which have the same abstract 
# but different grant numbers


# zip processed data files for uploading to github
files2zip <- dir("processed/", full.names = TRUE)
zip(zipfile = "processed_data.zip", files = files2zip)

# origncal date, should not need redoing.
# files2zip <- dir("data", full.names = TRUE)
# zip(zipfile = "data.zip", files = files2zip)
