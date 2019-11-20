library(janitor)
library(tidyverse)
library(httr)



# projectsearch-1572783843289.csv was download on 3.11.2019
# it was a complete search filter only on grant category which is 
# "Research Grant"
file <- "data/projectsearch-1572783843289.csv"
grants <- read.csv(file, 
                   stringsAsFactors = FALSE) %>% 
  clean_names()



# getting the abstracts for each of the project URLs
# n.b. time consuming - ~ about 1 hour for this example
# I don't recommend running this
gtr_project_url <- grants$gtr_project_url

for (i in 1:length(gtr_project_url)) {
  gtr_request <- GET(gtr_project_url[i])
  results <- content(gtr_request)
  grants$abstract[i] <- results[["projectOverview"]][["projectComposition"]][["project"]][["abstractText"]]
}


file <- "data/grants_3-11-19.csv"
write.csv(grants, file)