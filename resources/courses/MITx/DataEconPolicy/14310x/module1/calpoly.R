library(magrittr)
library(tidyverse)
library(rvest)

## The original website in the calpoly URL no longer includes the table in the lecture.

## We use an archived version of the website from the Wayback Machine (visit the URL yourself in your browser)
## to reproduce the exercise. 

CPadmission <- read_html("https://web.archive.org/web/20170503151107/http://admissions.calpoly.edu/prospective/profile.html")

CPadmission_tables <- CPadmission %>%
        html_nodes("table") %>% html_table()

## Print CPadmission_tables in your console (or use str(CPAdmission_tables))

## It is a list with the 2 tables of interest parsed on indices 2 and 3. 
## It may be easier to analyze/or manipulate each table as a data frame 
## Hence we access and save them separately

admission_1<- CPadmission_tables[[2]]

admission_2<- CPadmission_tables[[3]]
