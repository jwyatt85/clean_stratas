library(dplyr)
library(readr)
library(MCsurvey)
library(rvest)


#### State Ideology ---------------------------------------------------------------------------------------------------
load("state_leg_ideology.RData")

y <- x %>% 
  filter(year > 2008) %>% 
  group_by(st) %>% 
  summarise(mean_house = mean(hou_chamber, na.rm=TRUE),
            mean_senate = mean(sen_chamber, na.rm=TRUE )) %>% 
  mutate(ideology_score = mean_house + mean_senate) %>% 
  arrange(ideology_score) %>% 
  mutate(stname = as.character(st))

dc <- data.frame(st='DC', mean_house = NA, mean_senate = NA, ideology_score = NA, stname = 'DC')
y <- rbind(y, dc)


#### State GDP ---------------------------------------------------------------------------------------------------


state_gdp <- read_csv("quarter_gdp_bystate.csv") 

state.name2 <- c(state.name, "District of Columbia")

state_gdp2 <- state_gdp %>% 
  filter(Description == "All industry total", 
         ComponentName == 'Gross domestic product (GDP) by state', 
         GeoName %in% state.name2) %>% 
  select(GeoName, matches('^(2012|2013|2014|2015)')) %>% 
  mutate(
    state = GeoName,
    stname = c(state.abb[1:8], 'DC', state.abb[9:50])[match(state, sort(unique(state)))],
    percent_gdp_increase = (as.numeric(`2015Q3`) - as.numeric(`2012Q1`)) / as.numeric(`2012Q1`)
  )

#### State Obama Approval ---------------------------------------------------------------------------------------------------
library(MCsurvey)

load("data_TS.Rda")

obama_approval <- as.data.frame.matrix(prop.table(table(tsdat$demState, tsdat$nr2Bin),1))
states <- as.data.frame(cbind(levels(tsdat$demState)))
names(states)[1] <- "state"
obama_approval <- cbind(states, obama_approval)
names(obama_approval)[2] <- "obama_approve"
names(obama_approval)[3] <- "obama_disapprove"

obama_approval_final <- obama_approval %>% 
  mutate(
    stname = c(state.abb[1:8], 'DC', state.abb[9:50])[match(state, sort(unique(state)))]
  )


#### Unemployment by state ---------------------------------------------------------------------------------------------------

my_table <- "http://www.bls.gov/web/laus/lauhsthl.htm"

state_unemployment <- my_table %>% 
  read_html() %>% 
  html_table(fill = TRUE) %>% 
  .[[2]]
names(state_unemployment)[1] <- 'state'
names(state_unemployment)[2] <- 'unemployment_rate'
state_unemployment <- as.data.frame(state_unemployment)
state_unemployment <- state_unemployment[,1:2]

state_unemployment <- state_unemployment %>% 
  slice(-1) %>% 
  mutate(
    stname = c(state.abb[1:8], 'DC', state.abb[9:50])[match(state, sort(unique(state)))]
  )

##### Merge All dataframes ---------------------------------------------------------------------------------------------------



final_updated_state_data <- left_join(state_unemployment, obama_approval_final, by='stname')
final_updated_state_data <- left_join(final_updated_state_data, state_gdp2, by = 'stname')
final_updated_state_data <- left_join(final_updated_state_data, y, by = 'stname')

final_updated_state_data <- final_updated_state_data %>% 
  transmute(
    stname = stname,
    percent_gdp_increase = percent_gdp_increase,
    unemployment_rate = unemployment_rate,
    obama_approve = obama_approve,
    obama_disapprove = obama_disapprove,
    mean_house_ideology = mean_house,
    mean_senate_ideology = mean_senate,
    combined_ideology_score = ideology_score
  )

write_csv(final_updated_state_data, "extra_state_grouping_vars.csv")







