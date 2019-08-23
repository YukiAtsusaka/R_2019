
###############################################################################################
# SASinR_Day4.R
# Created by Yuki Atsusaka
# Last updated: August 7, 2019
# Since: August 5, 2019
# Aim: to learn basic operation in R (Day 4 of Social Analysis and Simulation in R)
##############################################################################################

###############################################################################################    
# GROUND RULE

# (1) Codes are for other people (including future you) to READ
# Thus, please write your codes as neat as possible
# Please provide a sufficient explanation of what your're doing

# (2) Codes are for other people (including future you) to REPRODUCE
# Thus, please write your codes so that you and others can reproduce your results
# without any adjustment or modification to the original codes
###############################################################################################    

###############################################################################################    
# SETTING UP 
###############################################################################################    

rm(list=ls()) # rm() cleans up your R. list=ls() means everything in the environment
gc();gc()     # gc() cleans up your memory

# Other resources are available from (https://www.rstudio.com/resources/cheatsheets/)

###############################################################################################    
# TIDYVERSE
###############################################################################################    
# So far, we have used so-called base R with some help of other packages which are
# a collection of "functions, data, and documentation that extends the capabilities
# of base R"
# We are now introduced to the mega package called "tidyverse" by Hadley Wickham  and others
# that is developed to make R programming and data science "easier, faster, and more fun".

install.packages("tidyverse")
library(tidyverse) # See 8 packages are loaded

# We will learn the world of tidyverse according to its data science philosophy
# Namely, we will follow this diagram of data science

# Import --> Tidy --> [Transform --> Visualize --> Model --> Trans] --> Communicate


###############################################################################################    
# DATA EXPLORATION: VISUALIZATION
###############################################################################################    
# ggplot2 helps you explore data visually
# ggplot2 is based on a layered grammer of graphics
# readr is much faster, convenient (doesn't transform character vecs to factors),
# and more compatible between differnt operating systems

 rm(list=ls())
 dat <- readr::read_csv("franseze.csv") # This is a tibble

 dat <- haven::read_dta("germanvoting1982_stata13") # Reading dta file
# readr::write_csv(dat, "saved_data.csv")

 p <- ggplot(data = dat) +                          # This specidies data
      geom_point(mapping = aes(x = YEAR, y = INF))  # This creates plots
 p  # This is a list containing all the info
 rm(p)
 
 # Color by country
 ggplot(data = dat) +                          
   geom_point(mapping = aes(x=YEAR, y=INF, color=as.factor(CTRY))) 

 # Color
 ggplot(data = dat) +                          
   geom_point(mapping = aes(x=YEAR, y=INF), color="blue") 


 # Transparency
 ggplot(data = dat) +                          
   geom_point(mapping = aes(x=YEAR, y=INF), alpha=1/10) 
 
 
 # Size by Inflation(t-1)  
 ggplot(data = dat) +                          
   geom_point(mapping = aes(x=YEAR, y=INF, size=`CBI(-1)`)) 
 
 # Transparency by Inflation(t-1)
 ggplot(data = dat) +                          
   geom_point(mapping = aes(x=YEAR, y=INF, alpha=`CBI(-1)`))  
 
 # Transparency by Inflation(t-1)
 ggplot(data = dat) +                          
   geom_point(mapping = aes(x=YEAR, y=INF, shape=as.factor(CTRY))) 

 # By Group
 ggplot(data = dat) +
   geom_point(mapping = aes(x = YEAR, y = INF)) +
   facet_wrap(~ CTRY, nrow=4)
 
 
# DIFFERENT GEOMS
 # Loess smoothing
  ggplot(data = dat) +                          
   geom_smooth(mapping = aes(x = YEAR, y = INF))  
  
  ggplot(data = dat) +                         
    geom_smooth(mapping = aes(x = YEAR, y = INF, color=as.factor(CTRY)))  
  
 # If you have multiple geom objects...
  ggplot(data = dat, mapping = aes(x = YEAR, y = INF)) +                          
    geom_smooth() +
    geom_point(mapping = aes(color=as.factor(CTRY)))
  
  
# BAR CHARTS
  ggplot(data=diamonds)+
    geom_bar(mapping = aes(x=cut))
  
  ggplot(data = diamonds) +
    stat_count(mapping = aes(x = cut))
  
  # Proportion
  ggplot(data = diamonds) +
    geom_bar(
      mapping = aes(x = cut, y = ..prop.., group = 1) )

  
  ggplot(data = diamonds) +
    stat_summary(
      mapping = aes(x = cut, y = depth),
      fun.ymin = min,
      fun.ymax = max,
      fun.y = median )  
  
  # Fill with colors
  ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, fill = clarity))
  

  ggplot(data = diamonds) +
    geom_bar(
      mapping = aes(x = cut, fill = clarity),
      position = "dodge" )
        
  # Jittering
  ggplot(data = mpg) +
    geom_point(
      mapping = aes(x = displ, y = hwy),
      position = "jitter"     )
  
# COORDINATE SYSTEMS
  # Flit coordinate
  ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
    geom_boxplot() +
    coord_flip()
  
  
# The Layered Grammer of Graphics  
# The general format is  
  
  # ggplot(data = <DATA>) +
  #   <GEOM_FUNCTION>(
  #     mapping = aes(<MAPPINGS>),
  #     stat = <STAT>,
  #     position = <POSITION>
  #   ) +
  #   <COORDINATE_FUNCTION> +
  #   <FACET_FUNCTION>
  

###############################################################################################    
# DATA EXPLORATION: TRANSFORMATION
###############################################################################################    
# dplyr helps you organize and transform the original data
# filter(), arrange(), select(), mutate(), summarize() are core functions
# group_by always help you categorize data by group
# pipe is an extremely helpful tool in data transformation and others
  
  rm(list=ls())
  library(tidyverse)
  
  dat <- readr::read_csv("franseze.csv") # This is a tibble
  
  
 # With filter, you can subset the data by directly specifying the condition  
  (CTRY1 <- filter(dat, CTRY==1)) # filter "dat" with the counry variable being 1
                                  # (new name <- function()) saves and shows in Console too
  
  CTRY1_1970 <- filter(dat, CTRY==1 & YEAR < 1980) # Multiple conditions
  
  CTRY123 <- filter(dat, CTRY==1 | CTRY==2 | CTRY==3) # This gets cubersome as conditions increase
  
  CTRY123 <- filter(dat, CTRY %in% c(1,2,3)) # Better way  
  

 # With arrange, you can reorder the data
  arrange(dat, YEAR, CTRY)           # See the data is sorted by Year and then Country
  arrange(dat, desc(YEAR, CTRY))     # Descending order
  

 # With select, you can keep variables you only need
  select(dat, YEAR, CTRY, INF)       # See the column order too
  select(dat, YEAR, CTRY:`CBI(-1)`)  # ":" is like "to"  
  select(dat, -(YEAR:`CBI(-1)`))     # "-" is like "except for"  
  
    # select has several super convenient "helper functions"
    select(dat, starts_with("G"))      # variable whose name starts with G
    select(dat, ends_with("(-1)"))     # variable whose name ends with (-1)
    select(dat, contains("INF"))       # variable whose name contains INF
  
  
    # rename is a variant of select, it renames variables
    rename(dat, country = CTRY)      # new_name = old_name
          
    # everything() is a helper function meaning "everything else"
    select(dat, INF, `INF(-1)`, everything()) # if you particulary care about the first two variables

  
 # mutate adds new variables to the current data
  dat_abb <- select(dat, YEAR, CTRY, INF) # Select only 3 variables
  mutate(dat_abb, INF_sq = INF^2)         # Create Inflation^2 variable
  
  mutate(dat_abb, 
         INF_sq = INF^2,
         INF_sq_round = round(INF_sq))    # New variable can be used as well
    

 # summarize gives you summary statistics
  summarize(dat, inf_mean = mean(INF))         # a single variable
  summarize_all(dat, median)                   # medians for all variables 
  summarize_all(dat, ~quantile(.,probs=0.025)) # 2.5 percentile
  
  
  # group_by specifies a group on which summary statistics are created
  
  within_country <- group_by(dat, CTRY)
  summarize(within_country, inf_within_mean = mean(INF)) # Within country mean
  
  between_country <- group_by(dat, YEAR)
  summarize(between_country, inf_between_mean = mean(INF)) # Between country mean

  
 # pipe ("%>%") helps you perform multiple operations in a sequence
  dat_mean <- dat %>%                  # Input data, then
              group_by(CTRY) %>%       # Make group-structure, then
              summarize(               # Summarize data
              inf_mean = mean(INF))    # using mean
  dat_mean

  # You can also write like this at the expense of readability  
  dat_mean <- dat %>% group_by(CTRY) %>% mutate(inf_mean_ctry = mean(INF)) %>% ungroup %>%
                      group_by(YEAR) %>% mutate(inf_mean_year = mean(INF)) %>% ungroup %>%
                      select(CTRY, YEAR, inf_mean_ctry, inf_mean_year) 
  dat_mean  
  summarize(dat_mean, n=n()) # n() shows the number of obs

  
  
 # Other helpful operations  
  dat %>% summarise_at(c("CTRY", "YEAR"), mean, na.rm=T) # Specifying variables directly
  
  dat %>% summarise_all(~quantile(.,probs=0.025)) # 2.5 percentile
  
  # Grouping by multiple variables
  dat %>%
    group_by(CTRY, YEAR) %>%
    summarize( mean(INF), num = n())    
  
  
  
 # A little advanced, but a pretty important topic (Especially in HW5)  
 # map function
  vec <- 1:10         # Vector of interest (e.g., values of the variable of interest)
  coef <- data.frame(var1=2, var2=1, var3=5)  # Data (e.g., coefficient of covariates)

  map(vec, ~coef %>% mutate(mapped_vec:=.x)) # See that only mapped_vec changes its values
  
  map(vec, ~coef %>% mutate(mapped_vec:=.x)) %>%
    bind_rows() # Turn a list into a data.frame
  
  
 # map with a specific argument position  
  1:10 %>% map(~ rnorm(.x, mean=0,sd=1)) # put 1:10 into ".x" (number of draws from N(0,1))

  
  # map can be used with "split" and applied to multiple datasets
  rm(list=ls())
  dat <- haven::read_dta("germanvoting1982_stata13.dta")
  unique(dat$edulevel)
  
  dat %>%
    split(.$edulevel) %>% # Split data by Education Level (4 unique values)
    map(~ glm(pmvotegrp ~ lrself + male + age,  
          family=binomial(link="logit"), data=.x)) %>% # Run logit for each splitted data
    map(summary) %>%      # Output summary for each model
    map_dbl("aic")        # Only report Akaike Information Criterion (AIC)
  

###############################################################################################    
# DATA WRANGLING: TIBBLE
###############################################################################################    
# tibble is a more flexible form of data.frame in base R
# tibble does NOT (1) change the type of inputs (e.g., strings to factors)
# (2) change the names of variables, and (3) create row names
  
 rm(list=ls())
 library(tidyverse)

 as_tibble(iris)  # This is tibble
 head(iris)       # This is data.frame
  
  
 tib_dat <- tibble(
             x = 1:5,
             y = 1,         # 1 will be recycled until the end of the sequence
             z = x ^ 2 + y,
             o = sample(letters, 5, replace = TRUE)
             )  
 tib_dat


 # Transposed tibble
 tribble(
   ~x, ~y, ~z,       # Variable names
   #--|--|----
   "a", 2, 3.6,      # Direct entry of values
   "b", 1, 8.5
 ) 
 
   
 # tibble is quite different from data.frame regarding printing and subsetting
 
 # Printing: tibble only shows top 10 rows (so kind!)
 tibble(
   a = lubridate::now() + runif(1e3) * 86400,
   b = lubridate::today() + runif(1e3) * 30,
   c = 1:1e3,
   d = runif(1e3),
   e = sample(letters, 1e3, replace = TRUE)
 )
 
 # Subsetting: tibble has easier ways to subset data 
 df <- tibble(
   x = runif(5),
   y = rnorm(5)
 )

 df        # See the whole data
 df$x      # This is how we do in data.frame
 df[["x"]] # This is what tibble can do
 df[[1]]   # Selecting on the column order is also possibe  
 
 # With pipe, you need some magical placeholder "."
 df %>% .$x         
 df %>% .[["x"]]
 
 
###############################################################################################    
# DATA WRANGLING: TIBBLE and TIDYING UP
###############################################################################################    
# tidydata is a data format where each row is one observation and each column is one variable
# Tidying up is important because most real-life (raw) data are not tidy

 # Gather
  table4a 
  
  (tidy4a <- table4a %>% gather(`1999`, `2000`, key="year", value="case")) # gather data
 
  table4b
 
  (tidy4b <- table4b %>% gather(`1999`, `2000`, key="year", value="population")) # gather data
 

 # Merge two datasets
  left_join(table4a, tidy4b, by="country")
  right_join(table4a, tidy4b, by="country")
 
  merge(table4a, tidy4b, all.x = TRUE) # In base R merge command
  merge(table4a, tidy4b, all.y = TRUE) # In base R merge command
 
 # Spread
  table2 # This data has two rows for one observation
  spread(table2, key="type", value="count") # spread the data
   
 
 # Seperate
  table3 # This data contains two variables in one row
  table3 %>% separate(rate, into=c("cases", "pop")) # seperate one column into two
  table3 %>% separate(rate, into = c("cases", "pop"), sep = "/") # specific about "/"
 
   # now cases and pop are character variables
    table3 %>% separate(rate, into=c("cases", "pop"), convert=T) # make them integers

 # Unite
  table5 # This has centry and year variables
  table5 %>% unite(C_YEAR, century, year)
  table5 %>% unite(C_YEAR, century, year, sep="C_") # Specify the seperation word
  

###############################################################################################    
# METAPROGRAMMING: QUOTING <ADVANCED TOPIC>
###############################################################################################    
# For HW 5, quoting will be necessary
# When writing functions, euquoting will be super important sometimes
# For reference, see (https://adv-r.hadley.nz/quasiquotation.html)
  

  rm(list=ls())
  dat <- haven::read_dta("germanvoting1982_stata13.dta")
  
  m_logit <- glm(pmvotegrp ~ lrself + edulevel + male, data=dat, family=binomial(link="logit") )

  
 # Consider this function    
  clarifyR <- function(glm_obj, zvar){
    
    zvar <- enquo(zvar)    # Quote variable name
    zmin <- glm_obj$model %>% select(!!zvar) %>% min() # Minimum value for Z-variable       

  return(zmin)
  }
  
  clarifyR(m_logit, lrself) # This works properly
  
  
 # Next, consider this function  
  clarifyR2 <- function(glm_obj, zvar){
    zmin <- glm_obj$model %>% select(zvar) %>% min() # zvar
    return(zmin)
  }
  clarifyR2(m_logit, lrself)  # Error in .f(.x[[i]], ...) : object 'lrself' not found 
  
  
  # Also, consider this function  
  clarifyR3 <- function(glm_obj, zvar){
    zmin <- glm_obj$model %>% select(!!zvar) %>% min() # !!zvar
    return(zmin)
  }
  clarifyR3(m_logit, lrself)  # Error in quos(...) : object 'lrself' not found 
  
  
 # Why is this happening...? To investigate, see what "zvar" is in each function.
  
 # This is the function that works  
  clarifyR <- function(glm_obj, zvar){
    zvar <- enquo(zvar)    # Quote variable name
    return(zvar)
  }
  clarifyR(m_logit, lrself) # This works properly
  
# We get this.   
#  <quosure>
#  expr: ^lrself
#  env:  global
  
  
  # Next, consider this function  
  clarifyR2 <- function(glm_obj, zvar){
    return(zvar)
  }
  clarifyR2(m_logit, lrself)  

# Error in clarifyR2(m_logit, lrself) : object 'lrself' not found 
  
  
  # Also, consider this function  
  clarifyR3 <- function(glm_obj, zvar){
    return(zvar)
  }
  clarifyR3(m_logit, lrself) 
  
# Error in quos(...) : object 'lrself' not found 

  
# I WILL ADD MORE EXPLANATION FOR DAY 4 or 5 CLASS    
    
##################################################################################################################    
# END OF THIS R SOURCE FILE
##################################################################################################################    