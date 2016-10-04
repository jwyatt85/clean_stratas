
library(dplyr)
library(reshape2)
library(stringr)
library(car)

setwd("~/Desktop/County_MRP/data/ACS_14_5YR_B15001")

#### Gender data ####
census <- readr::read_csv("ACS_14_5YR_B15001_with_ann.csv")

names(census) <- census[1,]
census <- census[-1,]

removes <- grep("Margin",names(census))
census <- census[ , -(removes)]

table(nchar(census$Id2))

county_gender_final <- census %>% 
  transmute(
    region = Id2, 
    Geography = Geography,
    totalpop = `Estimate; Total:`,
    totalmales = `Estimate; Male:`,
    totalfemales = `Estimate; Female:`
  ) %>% 
  mutate_each(
    funs(as.numeric),
    contains("total")
  ) %>% 
  transmute (
    region,
    Geography = Geography,
    percentmales = totalmales / totalpop,
    percentfemales = totalfemales / totalpop
  )

just_mute <-census %>% 
  transmute(
    Geography = Geography, 
    region = Id2
  ) 


### Age #####
removes <- grep("Id2|Geography|^Estimate; Total:$|18 to 24 years:$|25 to 34 years:$|35 to 44 years:$|45 to 64 years:$|65 years and over:$",names(census))
census2 <- census[ , (removes)]

converts <- grep("Estimate", names(census2), value = TRUE)
for(i in seq_along(converts)){
  x1 <- converts[i]
  census2[[x1]] <- as.numeric(census2[[x1]])
}

county_age_final <- census2 %>% 
  transmute(
    region = Id2,
    totalpop = `Estimate; Total:`,
    total18_24 = (`Estimate; Male: - 18 to 24 years:` + `Estimate; Female: - 18 to 24 years:`) / `Estimate; Total:`,
    total25_34 = (`Estimate; Male: - 25 to 34 years:` + `Estimate; Female: - 25 to 34 years:`) / `Estimate; Total:`,
    total35_44 = (`Estimate; Male: - 35 to 44 years:` + `Estimate; Female: - 35 to 44 years:`) / `Estimate; Total:`,
    total45_44 = (`Estimate; Male: - 45 to 64 years:` + `Estimate; Female: - 45 to 64 years:`) / `Estimate; Total:`, 
    total65_older = (`Estimate; Male: - 65 years and over:` + `Estimate; Female: - 65 years and over:`) / `Estimate; Total:`
  )

county_MRP_final <- left_join(county_gender_final, county_age_final, by = "region")

### Education #### - Do not Run #####

# removes <- grep("Geography|^Estimate; Total:$|grade|degree|graduate",names(census))
# census3 <- census[ , (removes)]

df_grid <- do.call(
  expand.grid,
  list(
    c('Male', 'Female'),
    c('18 to 24 years:', '25 to 34 years:', '35 to 44 years:', '45 to 64 years:', '65 years and over:'),
    c('Less than 9th grade', '9th to 12th grade, no diploma', 'High school graduate', 'Some college, no degree',
      "Associate's degree", "Bachelor's degree", "Graduate or professional degree"),
    census$Geography[!grepl('District of Columbia|Puerto Rico', census$Geography)]
  )
) %>% 
  mutate(
    sex = as.character(Var1),
    age = as.character(Var2),
    education = as.character(Var3),
    district = as.character(Var4)
  ) %>% 
  transmute(
    sex,
    age,
    education,
    district,
    state = sapply(
      .$district,
      function(str) {
        gsub('^\\s+|\\s+$', '', strsplit(str, split = ',')[[1]][2])
      }
    ),
    count = 0
  )

match_function <- function(row) {
  index <- grep(row$sex, names(census), value = TRUE) %>% 
    grep(row$education, ., value = TRUE) %>%
    grep(row$age, ., value = TRUE) %>% 
    {which(names(census) == .)}
  
  row_match <- census %>% 
    filter(Geography == row$district) %>% 
    select_(index) %>% 
    as.numeric()
  
  if (length(row_match) > 0) return(row_match)
  else return(NA)
}

temp <- df_grid %>% 
  mutate(
    count = sapply(
      seq_len(NROW(df_grid)),
      function(row_num) {
        df_grid %>% 
          slice(row_num) %>% 
          match_function()
      }
    )
  )

readr::write_csv(temp, '~/Desktop/df_grid_full.csv')
### Figure if this works #### 

test <- readr::read_csv('~/Desktop/df_grid_full.csv') 
ya <- test %>% 
  group_by(district) %>% 
  summarise(county_size = sum(count)) %>% 
  ungroup() %>% 
  left_join(readr::read_csv('~/Desktop/df_grid_full.csv'), by = 'district') %>% 
  group_by(education) %>% 
  mutate(
    percent = count / county_size,
    county_size
  )


ya3 <- test %>% 
  group_by(district) %>% 
  summarise(county_size = sum(count))

ya2 <- ya %>% 
  group_by(district, education) %>% 
  summarise(total = sum(count)) %>% 
  left_join(., ya3, by = 'district') %>% 
  mutate(
    percent = total / county_size
  ) %>% 
  select(
    district,
    education, 
    percent
  )

library(reshape2)
test <- dcast(ya2, district ~ education)


## final df ####
names(test)[1] <- "Geography"

county_MRP_final2 <- left_join(county_MRP_final, test, by = 'Geography')

readr::write_csv(county_MRP_final2, "~/Desktop/county_MRP_final.csv")
### Hispanic ####
library(dplyr)
setwd("~/Desktop/County_MRP/data/ACS_14_5YR_DP05")

census <- readr::read_csv("ACS_14_5YR_DP05_with_ann.csv")

names(census) <- census[1,]
census <- census[-1,]

removes <- grep("Margin",names(census))
census <- census[ , -(removes)]

keeps <- grep("Id2|Geography|Hispanic|^Estimate; SEX AND AGE - Total population$",names(census))
census2 <- census[ , (keeps)]

census2 <- census2 %>% 
  transmute(
    region = Id2,
    district = Geography, 
    percent_hispanic = as.numeric(`Percent; HISPANIC OR LATINO AND RACE - Total population - Hispanic or Latino (of any race)`)/ 100
  )

county_MRP_final3 <- readr::read_csv("~/Desktop/county_MRP_final.csv") %>% 
  left_join(., census2, by = 'region') %>% 
  readr::write_csv(., "~/Desktop/County_MRP/county_MRP_final2.csv")


#### Race ####
library(dplyr)
library(reshape2)
library(stringr)
library(car)

setwd("~/Desktop/County_MRP/data/ACS_14_5YR_DP05")
census <- readr::read_csv("ACS_14_5YR_DP05_with_ann.csv")

names(census) <- census[1,]
census <- census[-1,]

removes <- grep("Margin",names(census))
census <- census[ , -(removes)]

removes <- grep("Id2|Geography|^Percent; RACE - One race - White$|^Percent; RACE - One race - Black or African American$",names(census))
census2 <- census[ , (removes)]
names(census2)[3] <- "percent_white"
names(census2)[4] <- "percent_black"

census2 <- census2 %>% 
  mutate(
    percent_white = as.numeric(percent_white), 
    percent_black = as.numeric(percent_black), 
    percent_white = as.numeric(percent_white / 100), 
    percent_black = as.numeric(percent_black / 100),
    percent_other = 1 - (percent_white + percent_black),
    Id2 = as.numeric(Id2)
  ) %>% 
  select(
    region = Id2,
    percent_white, 
    percent_black, 
    percent_other 
  )

county_df <- readr::read_csv("~/Desktop/County_MRP/data/county_MRP_final2.csv") 
  

county_MRP_final3 <- left_join(county_df, census2, by = 'region') %>% 
  mutate(
    district = NULL
  )

readr::write_csv(county_MRP_final3, "~/Desktop/County_MRP/data/county_MRP_final3.csv")
  
