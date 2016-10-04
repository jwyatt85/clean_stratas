library(readr)
library(dplyr)
library(car)
library(blme)
set.seed(1234)

### Get data sets loaded and merged ####
zip_county_df <- read_csv("~/Dropbox/tmc/polls/16xxxx/Trav_James_Projects/County_MRP/data/zip_county_match.csv") %>% 
  mutate(zip = as.character(zip)) 

obama <- read_csv("~/Dropbox/tmc/polls/16xxxx/Trav_James_Projects/County_MRP/data/obama_12_kd.csv") %>% 
  na.omit %>% 
  select(region, obama, romney) %>% 
  mutate(region = as.character(region))

county_df <- read_csv("~/Dropbox/tmc/polls/16xxxx/Trav_James_Projects/County_MRP/data/county_MRP_final3.csv") %>% 
  na.omit() %>% 
  filter(!is.na(region)) %>% 
  mutate(
    `18-34` = total18_24 + total25_34,
    `35-44` = total35_44 ,
    `45-64` = total45_64, 
    `65+` = total65_older,
    HSorLess = `9th to 12th grade, no diploma` + `Less than 9th grade` + `High school graduate`, 
    SomeCollege = `Associate's degree` + `Some college, no degree`, 
    CollegeGrad = `Bachelor's degree`, 
    Postgraduate = `Graduate or professional degree`,
    white = percent_white,
    black = percent_black, 
    other = percent_other
  ) %>% 
  inner_join(., obama, by = 'region') %>% 
  filter(!duplicated(region)) %>% 
  left_join(readr::read_csv("~/Dropbox/tmc/polls/16xxxx/Trav_James_Projects/County_MRP/data/urban.csv"), by='region') %>% 
  left_join(readr::read_csv("~/Dropbox/tmc/polls/16xxxx/Trav_James_Projects/County_MRP/data/samesex.csv"), by='region')


# Read in survey data and sample county by probability 
survey_df <- read_rds("~/Dropbox/tmc/polls/16xxxx/Trav_James_Projects/candidate_analysis/data/tsdat.rds") %>% 
  mutate(zip = as.character(zip)) %>% 
  left_join(zip_county_df, by = 'zip') %>% 
  filter(!is.na(zcta_county_prob)) %>%
  group_by(pid) %>% 
  sample_n(1, weight = zcta_county_prob) %>% # Here's where we sample the row based on the county probability 
  ungroup() %>% 
  filter(!duplicated(pid)) %>%
  left_join(county_df, by = 'region') %>% 
  filter(!is.na(region)) %>%
  

# grep('', names(survey_df), value = T)
  
### Retrieve population proportions for each county - function ####
get_margins <- function(region){
  county_margins <- lapply(
    region, 
    function(i){
      df <- county_df
      
      df <- df %>% 
        dplyr::filter(
          region == i
        )
      margins <- list()
      
      sex_margins <- list(data.frame(sex = c('Male', 'Female'), Freq = c(df$percentmales, df$percentfemales)))
      margins <- c(margins, sex_margins)
      names(margins) <- c(names(margins), "sex")
      
      age_margins <- list(data.frame(age = c('18-34', '35-44','45-64', '65+'), Freq = c(df$`18-34`, df$`35-44`, df$`45-64`, df$`65+`)))
      margins <- c(margins, age_margins)
      names(margins) <- c(names(margins)[names(margins) > 1], "age")
      
      educ_margins <- list(data.frame(education = c('HSorLess', 'SomeCollege', 'CollegeGrad', 'Postgraduate'), Freq = c(df$HSorLess, df$SomeCollege, df$CollegeGrad, df$Postgraduate)))
      margins <- c(margins, educ_margins)
      names(margins) <- c(names(margins)[names(margins) > 1], "education")
      
      hispanic_margins <- list(data.frame(hispanic = c('Hispanic', 'NotHispanic'), Freq = c(df$percent_hispanic, df$percent_not_hispanic)))
      margins <- c(margins, hispanic_margins)
      names(margins) <- c(names(margins)[names(margins) > 1], "hispanic")
      
      race_margins <- list(data.frame(race = c('white', 'black', 'other'), Freq = c(df$percent_white, df$percent_black, df$percent_other)))
      margins <- c(margins, race_margins)
      names(margins) <- c(names(margins)[names(margins) > 1], "race")
      
      margins
    }
  )
  names(county_margins) <- region
  county_margins
}

x <- get_margins(county_df$region)

#### This nested for loop examines if any marings are 0, ####
# if so we assign it to .0001 so we don't get errors when we create the survey design/post-strat table
  for(i in 1:length(x)){
    print(i)
    for(z in 1:length(x[[i]])){
      for(y in 1:NROW(x[[i]][[z]])){
        x[[i]][[z]][y,]$Freq
        if(x[[i]][[z]][y,]$Freq == 0){
          x[[i]][[z]][y,]$Freq <- .0001
        }
      }
    }
  }

### Make joint probability tables ####

get_joint_probs <- function(data){
  x <- data
  final_grids <- lapply(1:length(x), function(i){
    region_strats <- lapply(
      1:length(x[[i]]), function(j){
        levels(x[[i]][[j]][,1])
      }
    )
    names(region_strats) <- names(x[[i]])
    expand.grid(region_strats)
  })
  
  names(final_grids) <- names(x)
  
  for(i in 1:length(final_grids)){
    final_grids[[i]] <- final_grids[[i]] %>% 
      dplyr::mutate(
        id = seq(1, nrow(.)),
        wts = rep(1, nrow(.)),
        regions = names(final_grids)[i]
      ) 
  }
  survey_designs <- lapply(
    1:length(final_grids), function(i){
      df <- final_grids[[i]]
      census_dsg <- survey::svydesign(id = ~id, weights = ~wts, data = df)
    }
  )
  
  final_county_tables <- lapply(
    1:length(x), function(i){
      census_dsg <- survey_designs[[i]]
      print(paste0("county: ", i))
      iter <- 1
      epsilon <- 1
      sample_margins <- vector('list', length(names(x[[i]])))
      for(z in 1:length(sample_margins)) { 
        sample_margins[[z]] <- as.formula(paste0("~",names(x[[i]])[z]))
      }
      nmar <- length(sample_margins)
      population_margins <- x[[i]]
      design <- census_dsg
      
      ff <- formula(
        paste(
          "~", 
          paste(
            unlist(lapply(sample_margins, all.vars)),
            collapse = "+"), 
          sep = ""
        )
      )
      
      strata <- lapply(
        sample_margins,
        function(margin) {
          if (inherits(margin, "formula")) {
            mf <- model.frame(margin, data = design$variables, na.action = na.fail)
          }
        }
      )
      oldtable <- survey::svytable(ff, design)
      
      while (iter < 100) {
        design$postStrata <- NULL
        for (i in 1:nmar) {
          design <- survey::postStratify(
            design,
            strata[[i]],
            population_margins[[i]], 
            compress = FALSE
          )
        }
        newtable <- survey::svytable(ff, design)
        delta <- max(abs(oldtable - newtable))
        if (delta < epsilon) {
          converged <- TRUE
          break
        }
        
        cat('Running iteration: ', iter, '\n')
        oldtable <- newtable
        iter <- iter + 1
      }
      
      newtable <- as.data.frame(newtable)
      newtable <- newtable %>% 
        mutate(
          id = 1:nrow(newtable)
        )
      
    })
  
  names(final_county_tables) <- names(x)
  final_county_tables
}

### function to make character to formula ####
.myformulatocharacter <- function(formula) {
  string <- strsplit(Reduce(paste, deparse(formula)), split = '')[[1]] %>% 
    paste0(collapse = '+') %>% 
    gsub('^\\s|\\s$', "", .) %>% 
    strsplit(., split = '\\s+\\+\\s+')
  string <- string[[1]] %>% 
    gsub('\\.', "", .)
  string[nzchar(string)]
}

.myformulatocharacter2 <- function(formula) {
  string <- unlist(strsplit(formula, "+", fixed=TRUE)) %>% 
    gsub('\\s', "", .) %>% 
    strsplit(., split = '\\s+\\+\\s+')
  string <- string[nzchar(string)]
}
### Run MRP estimates for county ideology ####

mrmp <- function(survey_data, jointp_list, individualvars, groupingvars, response, weights = NULL){
  
  response <- as.character(response)
  individualvars <- as.character(individualvars)
  groupingvars <- as.character(groupingvars)
  
  individualvars <- as.character(dplyr::setdiff(.myformulatocharacter2(individualvars), response))
  groupingvars <- as.character(dplyr::setdiff(.myformulatocharacter2(groupingvars), response))
  
  #do the recodes
  survey_data_final <- survey_data %>% 
    dplyr::mutate(
      race      = as.character(as.factor(car::recode(demRace4, "2='black';3='white';1='other';4='other';else=NA"))),
      age       = as.character(as.factor(car::recode(age, "1='18-34'; 2='35-44'; 3='45-64'; 4='65+'"))),
      sex       = as.character(as.factor(ifelse(demGender == 1, "Male", "Female"))),
      education = as.character(as.factor(car::recode(educ4, "1='HSorLess'; 2='SomeCollege'; 3='CollegeGrad'; 4='Postgraduate'"))),
      stname    = c(state.abb[1:8], "DC", state.abb[9:50])[demState],
      party     = as.character(as.factor(car::recode(demPidNoLn, "1='Republican'; 2='Democrat'; 3='Independent'; 4='Something else'"))),
      religion  = as.character(as.factor(car::recode(xreligion3, "1='Christian'; 2='Non_Christian'"))), 
      hispanic  = as.character(as.factor(car::recode(hisp_age7, "1='Hispanic'; 2='NotHispanic'; 3='Hispanic'; 4='NotHispanic'; 5='Hispanic';
                                                     6='NotHispanic'; 7='Hispanic'; 8='NotHispanic'; 9='Hispanic'; 10='NotHispanic'; 
                                                     11='Hispanic';12='NotHispanic'; 13='Hispanic'; 14='NotHispanic'")))
    ) %>%
    .[, c(groupingvars, response, 'age', 'education', 'stname', 'sex', 'hispanic','race')] %>% 
    na.omit
  
  if (!all(c(individualvars %in% names(survey_data_final)))) {
    stop("Individualvars not included in data - check names of your covariates", call. = FALSE)
  }
  if (!all(c(groupingvars %in% names(survey_data_final)))) {
    stop("Groupingvars not included in data - check names of your covariates", call. = FALSE)
  }
  
  #reformlate the parameters of the formula to specified blme
  blme_formula <- as.formula(
    paste0(
      paste0(
        response, ' ~ ',
        paste0('(1|', individualvars, ')', collapse = ' + ')),'+',paste0("", groupingvars, "", collapse = ' + ')))
  
  survey_data_final[[response]] <- as.factor(survey_data_final[[response]])
  
  #run model
  if(weights){
    MRmP <- suppressWarnings({blmer(blme_formula, data = survey_data_final, weights = survey_data_final$wts, family = binomial(link="logit"))})
  } else {
    MRmP <- suppressWarnings({blmer(blme_formula, data = survey_data_final, family = binomial(link="logit"))})
  }
  
  county_mrmp <- lapply(
    1:length(jointp_list), 
    function(i){
      df <- county_df %>% 
        filter(region == names(jointp_list[i])) 
      
      if(NROW(df) > 1){
        df <- df %>%
          mutate(
            obama = mean(obama),
            county_same_sex_couples = mean(county_same_sex_couples),
            county_urban = mean(county_urban)
          ) %>% 
          filter(!duplicated(region))
      }
      print(i)
      
      obama_percent <- df$obama
      county_urban <- df$county_urban
      county_same_sex_couples <- df$county_same_sex_couples
      
      jointp_list[[i]] <- jointp_list[[i]] %>% 
        mutate(
          obama = obama_percent,
          county_urban = county_urban,
          county_same_sex_couples = county_same_sex_couples,
          region = names(jointp_list[i])
        )
      
      predicted <- jointp_list[[i]] %>% 
        dplyr::mutate(pred = predict(MRmP, newdata=jointp_list[[i]], type="response"))
      
      predicted <- predicted %>% 
        dplyr::mutate(
          weighted_pred = predicted$pred * predicted$Freq
        )
      
      final_county_df <- data.frame(region = names(jointp_list[i]), county_pred = sum(predicted$weighted_pred))
      final_county_df
    }
  )
}

### Compute MRP ideology at the county level ####
joints <- get_joint_probs(x)

# survey_df$y <- recode(survey_df$demPolIdeo, "1=1;2=1;3=1;4=0;5=0;6=0;7=0;8=0;else=NA")
survey_df$clinton <- recode(survey_df$v16g5, "1=1;2=0;3=0;else=NA")
survey_df$trump <- recode(survey_df$v16g5, "1=0;2=1;3=0;else=NA")

survey_df <- survey_df %>% 
  filter(!is.na(clinton), !is.na(trump))

individualvars <- c("age + hispanic + education + sex + race")
groupingvars <- c("obama + county_urban + county_same_sex_couples")
responsevar <- c("clinton")

clinton <- mrmp(
  survey_data     = survey_df,
  jointp_list     = joints,
  individualvars  = individualvars,
  groupingvars    = groupingvars,
  response        = responsevar,
  weights = FALSE
) %>% 
  bind_rows() 

write_csv(clinton, "~/Desktop/county_MRP/data/clinton_support_final.csv")

responsevar <- c("trump")
trump <- mrmp(
  survey_data     = survey_df,
  jointp_list     = joints,
  individualvars  = individualvars,
  groupingvars    = groupingvars,
  response        = responsevar,
  weights = FALSE
) %>% 
  bind_rows()

write_csv(trump, "~/Desktop/county_MRP/data/trump_support_final.csv")

names(trump)[2] <- 'trump'
names(clinton)[2] <- 'clinton'

z <- county_df %>% 
  select(region, Geography, county_urban, percent_white, percent_black, obama, romney) %>% 
  mutate(region = as.character(region))

trump_clinton2 <- left_join(clinton, trump, by = 'region') %>% 
  mutate(
    trump_clinton_difference = clinton-trump
  ) %>% 
  select(
    region,
    clinton,
    trump,
    trump_clinton_difference
  ) %>% 
  left_join(z, by = 'region') %>% 
  left_join(obama, by = 'region')

write_csv(trump_clinton2, "~/Dropbox/tmc/polls/16xxxx/Trav_James_Projects/County_MRP/data/clinton_trump_difference.csv")

### Transforming the data ####
# x <- readr::read_csv("~/Desktop/County_MRP/data/county_MRP_v16g5.csv")
# hist(x$county_pred, breaks = 50) # shows extream positive skew
# 
# x <- x %>% 
#   mutate(county_pred = log10(county_pred))
# hist(x$county_pred, breaks = 10) 
# 
# write_csv(x, "~/Desktop/county_MRP/data/v16g5_skew_transformed.csv")

trump_clinton2 %>% 
  filter(trump_clinton_difference > -.05 & trump_clinton_difference < .05) %>%
  filter(grepl("Ohio", Geography)) %>% View

cor(trump_clinton2$clinton, trump_clinton2$obama.y, use="complete")


