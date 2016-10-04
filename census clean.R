
library(dplyr)
library(reshape2)
library(stringr)
library(car)

setwd("~/Dropbox/tmc/polls/160106/Supporting\ materials/")
id <- read.csv("ACS_14_5YR_B15001/ACS_14_5YR_B15001_metadata.csv", header = TRUE, stringsAsFactors = FALSE)
counties <- read.csv("cheseapeake_counties_final.csv", header = TRUE, stringsAsFactors = FALSE)
census <- read.csv("ACS_14_5YR_B15001/ACS_14_5YR_B15001_with_ann.csv", header = TRUE, stringsAsFactors = FALSE)
census <- census %>%
  select(-GEO.id)

removes <- grep("^Margin",census[1,])
census <- census[ , -(removes)]

names(census)[grep("GEO.id2", names(census))] <- "FIPS"
census$FIPS <- as.numeric(census$FIPS)

### merge and rename the columns -------------------------------------------------------
counties_merged <- left_join(counties, census, by = "FIPS")

column_fixer <- id$Id
names(column_fixer) <- id$GEO.id

counties_fixed <- counties_merged
old_names <- colnames(counties_fixed)
colnames(counties_fixed) <- c(old_names[1:6], column_fixer[old_names[7:length(old_names)]])

#### Gender -----------------------------------------------------------------------------
gender <- counties_fixed %>% 
  select(ends_with('Male:'), ends_with("Female:")) %>%
  mutate_each(funs(as.numeric)) %>% 
  summarise_each(funs(sum)) %>% 
  {./rowSums(.)} %>%
  melt

names(gender)[grep("value", names(gender))] <- "Population_Proportion"
names(gender)[grep("variable", names(gender))] <- "Label"
gender$Item <- "demGender"
gender$Value <- 1:nrow(gender)

#gender$Label <- str_match_all(gender$Label, "^Estimate; (\\w+):$")[[1]][,2]

gender$Label <- gsub("^.*; ", "", gender$Label)
gender$Label <- gsub(":", "", gender$Label)


### AGE ---------------------------------------------------------------------------------

age <- counties_fixed %>% 
  select(ends_with("years:"), ends_with("over:")) %>%
   mutate_each(funs(as.numeric)) %>% 
  summarise_each(funs(sum)) %>% 
  {./rowSums(.)} %>%
  melt %>% 
  mutate(
    variable = as.character(variable),
    grouper = gsub('^.* - ', '', variable)
  ) %>% 
  group_by(grouper) %>% 
  summarise(
    AgeGroupSum = sum(value)
  )

age$grouper <- gsub(":", "", age$grouper)

age$grouper <- recode(age$grouper, "'18 to 24 years' = '18-34';
                      '25 to 34 years' = '18-34';
                      '35 to 44 years' = '35-64';
                      '45 to 64 years' = '35-64';
                      '65 years and over'='65+'")

age <- age %>%
  group_by(grouper) %>%
  summarise(group = sum(AgeGroupSum))


age$Value <- 1:nrow(age)
age$Item <- "age3"

names(age)[grep("^group$", names(age))] <- "Population_Proportion"
names(age)[grep("^grouper$", names(age))] <- "Label"

### Education ---------------------------------------------------------------------------------
edu <- counties_fixed %>% 
  select(contains("years: -")) %>%
  mutate_each(funs(as.numeric)) %>% 
  summarise_each(funs(sum)) %>% 
  {./rowSums(.)} %>%
  melt %>% 
  mutate(
    variable = as.character(variable),
    grouper = gsub('^.* - ', '', variable)
  ) %>% 
  group_by(grouper) %>% 
  summarise(
    AgeGroupSum = sum(value)
  )

edu$grouper <- gsub("'", "", edu$grouper)

edu$grouper <- recode(edu$grouper, 
                      "'9th to 12th grade, no diploma' = 'Less than college';
                      'Associates degree' = 'Less than college';
                      'High school graduate (includes equivalency)' = 'Less than college';
                      'Some college, no degree' = 'Less than college';
                      'Less than 9th grade' = 'Less than college';
                      'Bachelors degree' = 'College';
                      'Graduate or professional degree' = 'Post-grad'; else=NA")

edu <- edu %>%
  group_by(grouper) %>%
  summarise(group = sum(AgeGroupSum))

edu$Value <- 1:nrow(edu)
edu$Item <- "demEduc3"
names(edu)[grep("^group$", names(edu))] <- "Population_Proportion"
names(edu)[grep("^grouper$", names(edu))] <- "Label"

### Race ---------------------------------------------------------------------------------

id <- read.csv("ACS_14_5YR_B02001/ACS_14_5YR_B02001_metadata.csv", header = TRUE, stringsAsFactors = FALSE)
counties <- read.csv("cheseapeake_counties_final.csv", header = TRUE, stringsAsFactors = FALSE)
census <- read.csv("ACS_14_5YR_B02001/ACS_14_5YR_B02001_with_ann.csv", header = TRUE, stringsAsFactors = FALSE)

removes <- grep("^Margin",census[1,])
census <- census[ , -(removes)]

names(census)[grep("GEO.id2", names(census))] <- "FIPS"
census$FIPS <- as.numeric(census$FIPS)

counties_merged <- left_join(counties, census, by = "FIPS")

column_fixer <- id$Id
names(column_fixer) <- id$GEO.id

counties_fixed <- counties_merged
old_names <- colnames(counties_fixed)
colnames(counties_fixed) <- c(old_names[1:6], column_fixer[old_names[7:length(old_names)]])


### make race df
race <- counties_fixed %>% 
  select(ends_with('alone')) %>%
  mutate_each(funs(as.numeric)) %>% 
  summarise_each(funs(sum)) %>% 
  {./rowSums(.)} %>%
  melt

race$variable <- gsub("^.*- ", "", race$variable)
race$variable <- gsub("alone", "", race$variable)
race$variable <- gsub("alone", "", race$variable)
#race$variable <- gsub(" ", "", race$variable, fixed = TRUE)

names(race)[grep("value", names(race))] <- "Population_Proportion"
names(race)[grep("variable", names(race))] <- "Label"


race$Label <- recode(race$Label, "'White '='White'; 'Black or African American '='Black'; else='Other'")

race <- race %>%
  group_by(Label) %>%
  summarise(group = sum(Population_Proportion))
  
race$Value <- 1:nrow(race)
race$Item <- "demRace"
names(race)[grep("group", names(race))] <- "Population_Proportion"

### Rbind Data File for Weights --------------------------------------------------------------


weightfile <- rbind(age, gender, edu, race)
weightfile <- weightfile[c("Item", "Label", "Value", "Population_Proportion")]

write.csv(weightfile, "pop_proportions_2_JW.csv")
