
###############################################################################################
# SASinR_Day5.R
# Created by Yuki Atsusaka
# Last updated: August 15, 2019
# Since: August 15, 2019
# Aim: to learn basic operation in R (Day 5 of Social Analysis and Simulation in R)
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
# WEB SCRAPING (I)
###############################################################################################
# The point of programming is that the computer will automatically do you job you programm
# TO OBTAIN INFORMATION ABOUT html_nodes, WE CAN USE "SELECTOR GADGET" ON GOOGLE CHROME  
# https://chrome.google.com/webstore/detail/selectorgadget/mhjhnkcfbdhnjickkkdbjoemdmbfginb  
# For reference, see http://bradleyboehmke.github.io/2015/12/scraping-html-text.html


rm(list=ls())
# install.packages("rvest")
# install.packages("stringr")
library(rvest) 
library(stringr)  

# PROBLEM: I WANT TO LIST NAMES OF ALL BLACK POLITICIANS IN THE U.S HISTORY FROM A WEBPAGE
#  http://history.house.gov/Exhibitions-and-Publications/BAIC/Historical-Data/Black-American-Representatives-and-Senators-by-Congress/


# READ THE WEBSITE PAGE FROM WHICH I WANT TO SCRAPE INFORMATION
archives <- read_html("http://history.house.gov/Exhibitions-and-Publications/BAIC/Historical-Data/Black-American-Representatives-and-Senators-by-Congress/")

# CODE TO EXTRAcT HTML CODED AS "a" (DATA COLLECTION) 
# TO FIGURE OUT HTML CODE, WE CAN USE "SELECTOR GADHET" ON GOOGLE CHROME.
name <- archives %>% html_nodes("a") %>% html_text() 


# CLEAN UP (DATA CLEANING)  
head(name, n=10) # WE HAVE UNNECCESARY INFORMATION AS WELL
name <- name[153:length(name)]                       # ONLY KEEP NAMES
name <- name[1:953]                                  # ONLY KEEP NAMES 

name <- gsub(pattern="\r\n", replacement="", name)   # DELETE ALL "r\n\" (REPLACE \r\n WITH NOTHING)  
name <- str_trim(name, side="both")                  # DELETE ALL "  " IN BOTH SIDES

# EXTRAcT ONLY SURNAME  
surname <- gsub(pattern=",.*$", replace="", name)    # REPLACE AFTER "," W/ "" (i.e. DELETE)


# EXTRACT ONLY FIRST NAME    
first <- gsub(pattern=", Jr."   , replace="", name)  # REPLACE ", Jr." W/ "" (i.e.DELETE)
first <- gsub(pattern=" Jr."    , replace="", first) # REPLACE " Jr." W/ "" (i.e.DELETE)
first <- gsub(pattern=", Sr."   , replace="", first) # REPLACE ", Sr." W/ "" (i.e.DELETE)
first <- gsub(pattern=" Sr."    , replace="", first) # REPLACE " Sr." W/ "" (i.e.DELETE)
first <- gsub(pattern=", II"    , replace="", first) # REPLACE ", II" W/ "" (i.e.DELETE)
first <- gsub(pattern=" II"     , replace="", first) # REPLACE " II" W/ "" (i.e.DELETE)
first <- gsub(pattern=", III"   , replace="", first) # REPLACE ", III" W/ "" (i.e.DELETE)
first <- gsub(pattern=" III"    , replace="", first) # REPLACE " III" W/ "" (i.e.DELETE)
first <- gsub(pattern=".*,"     , replace="", first) # REPLACE BEFORE "," W/ "" (i.e.DELETE)
first <- gsub(pattern="[0-9\\.]", replace="", first) # REPLACE NUMBERS W/ "" (i.e.DELETE)
first <- str_trim(first, side="both")                # DELETE "  " IN THE BOTH SIDES
first <- gsub(pattern=" .*$"    , replace="", first) # DELETE MIDDLE NAME
                                                     #  X.*$ means from X to the end

# CREATE A DATAfRAME  
blackrep <- data.frame(toupper(first), toupper(surname), 1)  # NAME LIST OF BLACK REPRESENTATIVES (W/ CAPITAL LETTERS)
names(blackrep)[c(1:3)] <- c("First", "Last", "AfAm") # COLUMN NAME
blackrep <- unique(blackrep) # DELETE DUPLICATES (ONLY KEEP UNIQUE NAME)
head(blackrep, n=10)
write.csv(blackrep, "blackrep_name.csv", row.names=F) # IMPORT AS CSV FILE


###############################################################################################    
# WEB SCRAPING (II)
###############################################################################################
  
rm(list=ls())
library(tidyverse)
library(rvest) 
library(stringr)  

# OBTAIN THE URL: LOOK AT HOW THEY ORGANIZE THEIR WEBSITE (2010-2019/2010/statprctreggen10.txt)
# SOMETIMES, YOU CAN ONLY CHANGE A PART OF THE URL TO GET ANOTHER PAGE
url <- "https://elections.sos.state.tx.us/elchist342_state.htm"

sc_text <- read_html(url) %>% html_node("form")  %>% html_text()  # AS TEXT
sc_text  # THIS IS .... NOT GOOD!

sc_table <- read_html(url) %>% html_node("form") %>%
            html_node("table") %>% html_table()  # AS TABLE
sc_table # MUCH BETTER! LOOK AT THIS RAW DATA--

dat <- sc_table[2:3,2:5] # GET WHAT YOU ONLY WANT
dat



# YOU CAN DO THIS REGARDLESS OF THE APPEARENCE OF TABLES
rm(list=ls())
url <- "https://elections.sos.state.tx.us/elchist233_race62.htm"

sc_table <- read_html(url) %>% html_node("table") %>% html_table()
sc_table # HERE YOU GO
  



###############################################################################################    
# POST-ESTIMATION SIMULATION
###############################################################################################

library(tidyverse)
library(dplyr)
library(readstata13)
library(ggplot2)
dat <- read.dta13("germanvoting1982_stata13.dta")
m_logit <- glm(pmvotegrp ~ lrself + edulevel + male + retnat + income + age, data=dat, family=binomial(link="logit") )


clarifyR <- function(glm_obj, zvar, xvar, xstart, xend, zinc, nsims){
  
  # QUOTE ANY VARIABLE NAME
  xvar <- enquo(xvar)
  zvar <- enquo(zvar)
  
  # CREATE A VECTOR OF ZVAR VALUES WITH AN INCREMENT OF zinc      
  zmin <- glm_obj$model %>% select(!!zvar) %>% min()
  zmax <- glm_obj$model %>% select(!!zvar) %>% max()
  zvector <- seq(from=zmin, to=zmax, by=zinc)
  
  # GRAB THE COEFFICIENT
  coefs <- coef(glm_obj)
  vcovs <- vcov(glm_obj)
  
  # GET TYPICAL VALUES
  ## to access the name of independent variables  
  indnames <- attr(glm_obj$terms, "term.labels")  
  
  ## obtain the median matrix of independent variables  
  typical <- glm_obj$model %>% 
    select(indnames) %>%
    summarize_all(median) %>%
    mutate(`(Intercept)`=1) %>% select(`(Intercept)`, everything())
  
  ## Build two datasets of typical values, one for xstart and the other for xend
  ## vary over zvector; xstart or xend; all the others set at their ypical values  
  
  # we're gonna map everyting to zvector
  typical_xstart <- map(zvector, ~typical %>% mutate(!!xvar:=xend, 
                                                     !!zvar:=.x)) %>%
    bind_rows() # length(zvector)xNumber of Independent variables
  
  typical_xend <- zvector %>%
    map(~typical %>% mutate(!!xvar:=xend, 
                            !!zvar:=.x)) %>% 
    bind_rows()
  
  # find xvar and changes to the value of xend
  # find zvar and changes to the original vector (zvector)
  # := means assign inside a function
  
  library(mvtnorm)      
  
  # SIMULATE THE COEFFICIENTS NSIM TIMES
  
  set.seed(20190307)
  coef_sim <- rmvnorm(nsims, mean= coefs, sigma = vcovs)
  
  lin_xstart <- as.matrix(typical_xstart)%*%t(coef_sim) # 1000x7 and 7x1000
  lin_xend <- as.matrix(typical_xend)%*%t(coef_sim)
  
  pred_xstart <- exp(lin_xstart)/(1 + exp(xstart))
  pred_xend <- exp(lin_xend)/(1 + exp(xend))
  pred_diff <- (pred_xend - pred_xstart) %>% t() %>% as.tibble()  # This is still Num-z times nsims matrix
  
  
  # CALCULATE THE CONFIDENCE INTERVAL
  
  ci_low    <- pred_diff %>% summarize_all(~quantile(.,probs=0.025)) %>% t()
  ci_high   <- pred_diff %>% summarize_all(~quantile(.,probs=0.975)) %>% t()
  ci_median <- pred_diff %>% summarize_all(~quantile(.,probs=0.5))   %>% t()
  
  ggdata <- cbind(ci_low, ci_high, ci_median, zvector) %>% as.tibble() %>%
    dplyr::rename(ci_low=V1, ci_high=V2, ci_median=V3)
  
  
  # DRAW GG PLOT
  
  p <-  ggplot(data=ggdata, aes(x=zvector)) +
    geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=0.001, size=2, color="orange") +
    geom_point(aes(y=ci_median), size=4, color="orange")
  
  return(p)                  
}


clarifyR(glm_obj=m_logit, xvar=edulevel, xstart=1,xend=4,
         zvar=income,zinc=0.5, nsim=1000)


##################################################################################################################    
# END OF THIS R SOURCE FILE
##################################################################################################################    