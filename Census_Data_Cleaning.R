library("tidyverse")
library("stringr")
setwd("~/CMU/Fall_2019/ML_Pipeline/Final Project")
citizenship <- read.csv("CitizenshipStatus.csv") %>% select(c(1,3,8)) %>% rename(cit.total = 2, foreign.pop = 3) %>% 
            mutate(foreign.rate = foreign.pop/cit.total)
education <- read.csv("EducationAttainment.csv") %>% mutate(noDiploma.pop = .[,5] + .[,6] + .[,7] + .[,8] + .[,9] + .[,10] + .[,11] + .[,12] + .[,22] + .[,23] + .[,24] + .[,25] + .[,26] + .[,27] + .[,28] + .[,29],
                                                            highSchool.pop = .[,13] + .[,30],
                                                            someCollege.pop = .[,14] + .[,15] + .[,31] + .[,32],
                                                            assoc.pop = .[,16] + .[,33],
                                                            bach.pop = .[,17] + .[,34],
                                                            graduateDegree.pop = .[,18] + .[,19] + .[,20] + .[,35] + .[,36] + .[,37]) %>%
            select(c(1,3,38:43)) %>%
            rename(edu.total = 2) %>%
            mutate(noDiploma.rate = noDiploma.pop/edu.total, highSchool.rate = highSchool.pop/edu.total, someCollege.rate = someCollege.pop/edu.total, assoc.rate = assoc.pop/edu.total, bach.rate = bach.pop/edu.total, graduateDegree.rate = graduateDegree.pop/edu.total)
workedHrs <- read.csv("MeanHrsWorked.csv") %>% select(c(1,3)) %>% rename(hrs.worked = 2)
age <- read.csv("MedianAge.csv") %>% select(c(1,3)) %>% rename(median.age = 2)
income <- read.csv("MedianIncome.csv") %>% select(c(1,3)) %>% rename(median.income = 2)
race <- read.csv("Race.csv") %>% mutate(other.pop = .[,9] + .[,10] + .[,11] + .[,12]) %>% select(c(1,3:8,13)) %>% 
            rename(race.total = 2, white.pop = 3, black.pop = 4, amIndian.pop = 5, asian.pop = 6, pacific.pop = 7) %>%
            mutate(white.rate = white.pop/race.total, black.rate = black.pop/race.total, amIndian.rate = amIndian.pop/race.total, asian.rate = asian.pop/race.total, pacific.rate = pacific.pop/race.total, other.rate = other.pop/race.total)
veteran <- read.csv("VeteranStatus.csv") %>% select(c(1,3,4)) %>% rename(vet.total = 2, veteran.pop = 3) %>% mutate(veteran.rate = veteran.pop/vet.total)

map <- read.csv("Tract to Community Area Equivalency File.csv") %>% select(c(7,8))

combine <-  inner_join(age, citizenship, by = "Id") %>% 
            inner_join(education, by = "Id") %>% 
            inner_join(income, by = "Id") %>% 
            inner_join(race, by = "Id") %>%
            inner_join(veteran, by = "Id") %>%
            left_join(., workedHrs, by = "Id") %>%
            mutate(TRACT = str_sub(Id, -6) %>% str_remove("^0+") %>% as.integer()) %>%
            inner_join(map, by = "TRACT")