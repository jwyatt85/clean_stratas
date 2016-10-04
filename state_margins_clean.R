library(MCsurvey)
library(rvest)
library(readr)
library(dplyr)

data <- readRDS("~/Dropbox/tmc/polls/151002/Data/dataFinal.rds")
get_names("^dem", df=data)
get_names("^Inc", df=data)

#### wikipedia union membership by state -----------------------------------------------------------------------------------------------------------------
my_table <- "https://en.wikipedia.org/wiki/Union_affiliation_by_U.S._state"

union_margins <- my_table %>% 
  read_html() %>% 
  html_table(fill = TRUE) %>% 
  .[[1]]

final_union_margins <- union_margins %>% 
  transmute(
    state = State,
    percent_union = .[,3]
  ) %>% 
  mutate(
    percent_non_union = 100 - percent_union
  )

remove_white <- function (x)  sub("^\\s+", "", x)
final_union_margins$state <- remove_white(final_union_margins$state)

final_union_margins <- final_union_margins %>% 
  filter(state != "United States") %>% 
  transmute(
    stname = c(state.abb[1:8], 'DC', state.abb[9:50])[match(final_union_margins$state, sort(unique(final_union_margins$state)))],
    percent_union = percent_union/100,
    percent_non_union = percent_non_union/100
  )
  

#### ACS 5 year income by state -----------------------------------------------------------------------------------------------------------------

library(MCsurvey)
library(rvest)
library(readr)
library(dplyr)

census <- read_csv("~/Desktop/income_bystate.csv")
names(census) <- census[1,]
census <- census[-1,]

census_clean <- census %>% 
  select(-contains("MARGIN")) %>% 
  select(Geography, contains("INCOME AND BENEFITS")) %>% 
  select(Geography, contains("HOUSEHOLD")) %>% 
  select(Geography, contains("ESTIMATE"))

names(census_clean) <- gsub("Estimate;\\sINCOME\\sAND\\sBENEFITS\\s\\(IN 2014 INFLATION-ADJUSTED DOLLARS\\)\\s-\\s",  "", names(census_clean))
names(census_clean) <- gsub("Total\\shouseholds\\s-\\s",  "Amount:", names(census_clean))
names(census_clean) <- gsub("\\sto\\s",  "-", names(census_clean))
census_clean <- as.data.frame(census_clean)

for(i in 2:length(census_clean)){
  census_clean[,i] <- as.numeric(census_clean[,i])
}

census_clean_final <- census_clean %>% 
  filter(Geography != "Puerto Rico") %>% 
  transmute(
    state = Geography,
    total_households = `Total households`,
    `Less_35k`  = `Amount:Less than $10,000` + `Amount:$10,000-$14,999` + `Amount:$15,000-$24,999` + `Amount:$25,000-$34,999`,
    `<50K`  = `Amount:$35,000-$49,999`,
    `<75k`  = `Amount:$50,000-$74,999`,
    `<100k` = `Amount:$75,000-$99,999`,
    `100k+` = `Amount:$100,000-$149,999` + `Amount:$150,000-$199,999` + `Amount:$200,000 or more`
  )  %>% 
  transmute(
    stname = as.factor(c(state.abb[1:8], 'DC', state.abb[9:50])[match(state, sort(unique(state)))]),
    `less_than_35k`  = Less_35k/total_households,
    `percent_<50K`  = `<50K`/total_households,
    `percent_<75k`  = `<75k`/total_households,
    `percent_<100k` = `<100k`/total_households,
    `percent_100k+` = `100k+`/total_households
  ) 

new_margins <- left_join(census_clean_final, final_union_margins, by="stname")
write_csv(new_margins, "addiontal_marings.csv")



