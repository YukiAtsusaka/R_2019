
###############################################################################################
# SASinR_Day3.R
# Created by Yuki Atsusaka
# Last updated: July 29, 2019
# Since: July 29, 2019
# Aim: to learn basic operation in R (Day 3 of Social Analysis and Simulation in R)
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

###############################################################################################    
# FUNCTIONS (II)
###############################################################################################    
# You can write your own function to automate the process

 rm(list=ls())

 doubleup <- function(argument){ # Function must have at least one argument
   2*argument                    # Operation takes place over the argument 
 }
 doubleup(3)                     # To use it, you can simply input a value as an argument
 
 class(doubleup)                 # Check the class
 print(doubleup)                 # Check what's inside

 
 
 doubleup <- function(x){        # Function whose argument is "x"
   print(2*x)                    # You can have multiple operations
   print(  paste("This function multiplied", x, "by 2")  )
 }
x``
 doubleup(10)
 doubleup(c(10,20,30))           # You can also input lists or matrices
 doubleup(matrix(1:4, nrow=2, ncol=2)) # Inputting a matrix



 mix <- function(x, y, z){        # Multiple arguments
   output <- x + y + z            # Save the result locally
   return(output)                 # After "return" nothing will be executed
   print("Hey")
 } 

 mix(1,2,3)
 mix(c(1,1,1), c(2,2,2), c(3,3,3)) # Input vectors

 
###############################################################################################    
# FUNCTIONS (III)
###############################################################################################     
# You can program various operations inside functions
# When you write a function, you must think really hard WHAT YOU NEED TO ACCOMPLISH 
 
 rm(list=ls())
 
 Normal  <- rnorm(1000, mean=0,sd=1)
 Poisson <- rpois(1000, lambda=2)

 
 # Relatively long function 
 plot4me <- function(var1, var2){
   
   # cat("Start plotting data for you!", "\n")
   # cat("Current time is", format(Sys.time(), "%a %b %d %X %Y"), "\n")
   # start <- Sys.time() # Store the time
   # 
   # cat("----------------------------", "\n")
   par(mfrow=c(2,2))   # Specify the graphic parameter
   hist(var1, xlab=deparse(substitute(var1)), 
        main=paste0("Histogram of", sep=" ", substitute(var1)))
   hist(var2, xlab=deparse(substitute(var2)),
        main=paste0("Histogram of", sep=" ", substitute(var2)))
   
   m1 <- lm(var1 ~ var2)  # You can do whatever necessary inside functions
   m2 <- lm(var2 ~ var1)  # Here we run a regression and store the result for the plot
   
   plot(var1, var2)    
   abline(m1, col="red")
   
   plot(var2, var1)
   abline(m2, col="red")
   
   # end <- Sys.time() # Store the time
   # cat("Finished plotting data!", "\n")
   # cat("Current time is", format(Sys.time(), "%a %b %d %X %Y"), "\n")
   # cat("The process took", end - start, "seconds")
   
    }

 # Let's use this function 
 plot4me(Normal, Poisson)
 
 
 # Let's write a function for estimating predicted probabilities
 rm(list=ls())
 library(readstata13)
 
 dat <- read.dta13("germanvoting1982_Stata13.dta")
 
 m <- glm(pmvotegrp ~ lrself + male + income + age  + edulevel, data=dat, family="binomial")  
 summary(m)
  

#****************************************************************************#
PredProb <- function(model, xvar, xstart, xend, xind){  # Five arguments
   
#   xvar <- enquote(xvar)
   coefs <- coef(model)                          # Coefficient list
   indnames <- attr(model$terms, "term.labels")  # Names of variables
   xmat <- model$model[,-1]                      # Matrix for predictors
   
   # Median values for all variables except for xvar
   typical <- apply(xmat[-which(names(xmat)==deparse(substitute(xvar)))], 2, median) 
   
   xseq <- seq(from=xstart, to=xend, by=xind)    # Variable of interest
   intercept <- rep(1, length(xseq))             # Intercept
   typicalmat <- matrix(rep(typical, length(xseq)), ncol=length(typical), byrow=TRUE)
   
   typical_dat <- data.frame(intercept, xseq, typicalmat) # Typical data frame
   colnames(typical_dat) <- c("Intercept", indnames)      # Typical data frame name
   
   typical_dat <- as.matrix(typical_dat)         # Typical data matrix (10 by 6)
   coefs <- as.matrix(coefs)                     # Coefficient matrix (6 by 1)
   linagg <-  typical_dat %*% coefs              # Linear aggregators
  
   pred <- exp(linagg)/(1 + exp(linagg))         # Predicted probabilities

   # Provide a visualization of predicted probabilities
   plot(pred ~ xseq, ylab="Pred probability of voting for PM party", pch=16,
        xlab=substitute(xvar))
   lines(pred ~ xseq, type="l")
   title("Predicted probabilities")
   
   return(pred)
  }
#****************************************************************************# 

 # Let's use our function here
   summary(m)
   
   PredProb(m, lrself, 1, 10, 1)   # Left-right ideology variable
   PredProb(m, edulevel, 1, 4, 1)  # Education level variable
   PredProb(m, age, 15, 93, 1)     # Age variable


###############################################################################################    
# QUANTITATIVELY PREDICTIVE LOGICAL MODEL
###############################################################################################     
# You can provide a simulated result of your quantitatively predicteid logical model
# Here, an example is a model that predicts the emergence of black candidates
   
# (Below is the explanation of the model)
# We model the district level minority candidate emergence as the decision making of 
# the most viable minority politicians whose sole agenda is to win elections where 
#   two racial groups (i.e., minority and majority groups) compete each other. 
# In the model, we theorize that minority candidates decide to run for office when 
#  they see a higher probability of winning. Minority candidates then attempt to calculate
#   the prior win probability in the upcoming elections, but as for any candidate, 
#   they are considered to be bounded rational and thus try to make the most satisfactory choice
#   based on incomplete information. Because it is quite difficult for minority candidates, 
#   as for any candidate, to calculate the prior probability of winning, 
#   they rely on two sources of information, which consist of 
#   (1) electoral performance of co-ethnic candidates in the last elections and
#   (2) district level racial composition as relevant heuristics. 
# Finally, we claim that the value of information from the last elections increases as 
#   districts become more competitive or more racially heterogeneous because 
#   in racially balance districts the racial makeup is not as informative as 
#   in racially homogeneous districts.   
   
# Formally, 
# P_{it} = F (alpha_{it}* delta_{it-1}  + (1 - alpha_{it})*beta_{it} ), where
# alpha = weight parameter (0,1) (how much a candidate relies on past electoral result)
# delta = past electoral result (-50,50)
# beta  = expected vote share based only on racial makeup (-50, 50)   
# F     = some cumulative distribution function
# lambda = proportion of blacks
   
# *** alpha = 4*lambda + (1-lambda)
# *** beta  = 50*(2*lambda - 1)   
   
# Hypothesis 1: Past information matters
# Hypothesis 2: Impact of past information increases as distrcits become more racially balanced   

      
   rm(list=ls())
   
  # Showing the behavior of parameters
   lambda = seq(from=0,to=1,by=0.01)    # Proportion of blacks
   alpha = 4*lambda*(1-lambda)          # Weight parameter
   beta = 50*(2*lambda - 1)             # Naive guess parameter
   
   par(mfrow=c(1,2))
   plot(alpha ~ lambda, type="l", col="firebrick4", lwd=3, 
        xlab=expression(lambda[it]), ylab=expression(alpha[it]),
        main=expression(paste("Weight Parameter ", (alpha[it]), sep=" ")))
   abline(v=0.5, lty=2, col="dimgray")
   plot(beta ~ lambda, type="l",  col="firebrick4", lwd=3,
        xlab=expression(lambda[it]), ylab=expression(beta[it]), ylim=c(-50,50),
        main=expression(paste("Naive Guess Parameter ", (beta[it]), sep=" ")))
   abline(v=0.5, lty=2, col="dimgray"); abline(h=0, lty=2, col="dimgray")
   
   
  # Exploring the functional form of the model
   rm(list=ls())
   par(mfrow=c(1,1))
   
   lambda_val = 0.5   # Tuning parameter (proportion of blacks)
   
   delta = seq(from=-50, to=50, by=0.01)
   alpha = 4*lambda_val*(1-lambda_val)
   beta = 50*(2*lambda_val - 1)
   q = alpha * delta + (1-alpha)*beta
   SD = 17
   
   # Sample plot (We want to create a bunch of these by varying parameters)
   P0 = pnorm(q=q, mean=0, sd=SD)
   plot(P0 ~ delta, type="l", lwd=2, ylab="P", ylim=c(0,1), cex.main=2, xlab=expression(Delta),
        main=substitute(paste(lambda, sep=" = ", v), list(v=lambda_val))) 
   
   
  # Now, we want to create similar plots under different conditions
  # We use a loop for this purpose

  # Run from here all the way down
  rm(list=ls())
  par(mfrow=c(4,3), mar = c(3, 3, 2, 2)) # Default (5,4,4,2) c(bottom, left, top, right)
  
  # Write a loop for this
  for(i in 0:10){                              # We explore (0,1) in lambda

    lambda_val = i * 0.1                       # Parameters to be varied
    SD = 17                                    # Parameters to be varied
    SD_small = 8                               # Parameters to be varied
    SD_large = 25                              # Parameters to be varied
        
    delta = seq(from=-50, to=50, by=0.01)      # Fixed quantities
    alpha = 4*lambda_val*(1-lambda_val)        # Function of lambda_val
    beta = 50*(2*lambda_val - 1)               # Function of lambda_val
    q = alpha * delta + (1-alpha)*beta         # Function of lambda_val
   
    prob  <- pnorm(q=q, mean=0, sd=SD)         # Probability of winning
    prob2 <- pnorm(q=q, mean=0, sd=SD_small)   # Probability of winning
    prob3 <- pnorm(q=q, mean=0, sd=SD_large)   # Probability of winning
    prob4 <- pnorm(q=q, mean=-20, sd=SD_small) # Probability of winning
    
    plot(prob ~ delta, type="l", lwd=2, ylim=c(0,1), ylab="", 
        xlab=expression(Delta), cex.main=1.5,  
        main=substitute(paste(lambda, sep=" = ", v), list(v=lambda_val)))
    lines(prob2 ~ delta, type="l", lty=2, col="firebrick", lwd=2)
    lines(prob3 ~ delta, type="l", lty=3, col="navy", lwd=2)
    lines(prob4 ~ delta, type="l", lty=4, col="forestgreen", lwd=2)
     }         

  # We want to put a legend on this combined graph
  plot(1, type = "n", axes=FALSE, xlab="", ylab="") # No plotting
  legend(x = "topleft", 
         legend = c("Normal(0,17)", "Normal(0,8)", "Normal(0,25)",
                    "Normal(-20,8)"), 
         col=c("black", "firebrick", "navy", "forestgreen"),
         lty=c(1,2,3,4), ncol=2,
         lwd=2, cex=1.1, horiz = FALSE, text.width=0.2, box.col = "white")   

   
 # ---> Hypotheses 1 and 2

  
###############################################################################################    
# APPLY AND LOOP
###############################################################################################     
# This section introduces the argument that "You shouldn't use for loop whenever you can!"  
# For a reference, see here (https://nicercode.github.io/guides/repeating-things/) 
# Some exemplary codes are borrowd from the above page   

# FOR LOOPS must be avoided because they become
# (1) expressive (= lots of lines! Not intuitivel! Hard to read!)    
# (2) vulnerable to various mistakes

# The apply family: laaply, sapply, tapply, aggregate, mapply, apply  as good alternatives
# Advantage: The order of iteration does not matter (iteration is independent here)  

# Here, we need to be introduced to the concept of lists first   
# "List" as a generic vector containing multiple objects   
  
  rm(list=ls())
  
  num = c(2, 3, 5) 
  state = c("Wisconsin", "North Carolina", "Ohio", "Pennsylvania", "Maryland") 
  logic = c(TRUE, FALSE, TRUE, FALSE, FALSE) 
  
  mylist = list(Number=num, State=state, Logic=logic, Scalar=3)  # List with four "components"
  
  # Now look at this carefully
  
  mylist
  
  mylist[[1]]       # First vector *** note that we use [[]] double bracket here
  mylist[[2]][1]    # First entry of second vector
  
  mylist$State      # You can access a specific vector by its name
  mylist[["State"]] # Alternatively  
  
  
#---------------------------------------------------------------------------------------------#    
# LAPPLY, SAPPLY, MAPPLY
#---------------------------------------------------------------------------------------------#     
# Getting to know lapply and sapply functions

# first.act  <- lapply(data, first.function)       --> Using some initial data
# second.act <- lapply(first.act, second.function) --> Using the result from above  
# Third.act ... (We can stack multiple tasks like forever)  

  rm(list=ls())      
  cities <- c("Melbourne", "Sydney", "Brisbane", "Cairns") # Names of cities we explore
  urls <- sprintf("http://nicercode.github.io/guides/repeating-things/data/%s.csv",
                  cities) # Put each element of "cities" in the place of "%s"
  urls                     # We saved multiple urls  
  
  download.maybe <- function(url, refetch=FALSE, path=".") { # Function
    dest <- file.path(path, basename(url)) # This is 
    if (refetch || !file.exists(dest))     # If such file doesn't exist in my folder
      download.file(url, dest)             # download the file
    dest
  }
  
  path <- "data"                           # Name of a new folder
  dir.create(path, showWarnings=FALSE)     # Connect with the new folder "data"
  
  # SAPPLY FUNCTION HERE ****
  # On each element of "urls", use "download" function
  files <- sapply(urls, download.maybe, path=path)# Just 1 line regardless of the number of cities
  
  names(files) <- cities                   # Put names on downloaded files  
  
  # Look at your working directory. Do you see a folder "data"?    
  
  # If you don't use SAPPLY, you need four lines! (Do not run)
  download.maybe(urls[[1]], path=path)
  download.maybe(urls[[2]], path=path)
  download.maybe(urls[[3]], path=path)
  download.maybe(urls[[4]], path=path)
  
  
  
  load.file <- function(filename) {
    d <- read.csv(filename, stringsAsFactors=FALSE)
    d$time <- as.POSIXlt(d$time)
    d
  }
  
  
  dat <- lapply(files, load.file)
  names(dat) <- cities
  
  y <- list(random=rnorm(4, 0,1), another=c("Just", "random", "words"))
  
  # Consequently, we obtain a list
  head(dat$Sydney)      
  
  sapply(dat, nrow) # Using "dat", show the number of rows for all components
  lapply(dat, nrow) # Compare with sapply
  
  sapply(dat, function(x) max(x$temp)) # Using "dat" Show maximum of temp
  autocor <- lapply(dat, function(x) acf(x$temp, lag.max=24)) # Autocorrelation plot
  plot(autocor$Sydney, main="Sydney") # Show one plot (autocor is a list!)
  
  # Do you see the pattern? The code takes the following form
  #  lapply(your_data, function(x) FUNCTION(x)), where x is an indicator of an argument
  
  mapply(function(x,y){x^y},x=c(2,3),y=c(3,4)) # Multivariate sapply with two data sources
  # FUN comes first here
  
  mapply(function(first_list, second_list)     # Another example
    seq_len(first_list) + second_list,    # FUNCTION to apply
    c(a =  1, b = 2, c = 3),              # See that the first of "first_list" remains
    c(A = 10, B = 0, C = -10))
  
  
  # Parallelising: If you deal with BIG DATA, you may want to fully use your cores
  # If you just use this, you only use one of four or more cores you (possibly) have
  normal <- sapply(dat, nrow) # This takes less than a second, but as an example
  normal
  
  library(parallel)   
  quick <- mclapply(dat, nrow)
  quick  
  
#---------------------------------------------------------------------------------------------#    
# TAPPLY, AGGREGATE
#---------------------------------------------------------------------------------------------#     
  rm(list=ls()) 
  dat <- read.csv("franseze.csv", header=T, sep=",") # Read the data
  
  head(dat)  # See that we have Country and Year
  dat <- dat[order(dat$YEAR, dat$CTRY), ] # Sort the data (rows)
  
  head(dat)  
  
 # How to calculate the average inflation rate per year?
  mean(dat$INF[dat$YEAR==1972])
  mean(dat$INF[dat$YEAR==1973])
 #....... cont'd
  mean(dat$INF[dat$YEAR==1990])   # This takes 19 lines (intuitive, but long)
  
 # Do this with for loop    
  year <- sort(unique(dat$YEAR))
  inf  <- numeric(length(year))   # 0 vector of length 19
  for (i in seq_along(year))      # Loop over the sequence of year
    inf[i] <- mean(dat$INF[dat$YEAR == year[i]])
  inf                             # This takes 4-5 lines (less intuitive, but short)
  
  
 # Do this with sapply  
  inf.split <- split(dat$INF, dat$YEAR) # Split the inflation vector by year
  head(inf.split)                       # This is a list with 19 components
  sapply(inf.split, mean)               # This takes 2 line: sapply(LIST, FUNCTION)
  
  
 # Do this with tapply  
  tapply(dat$INF, dat$YEAR, mean) # This takes 1 line: tapply(Data, By_group, FUNCTION)

  
 # Extra tips   
  se <- function(x) sqrt(var(x) / length(x)) # Write a function
  inf.se   <- sapply(inf.split, se)
  inf.mean <- sapply(inf.split, mean)               
  
  plot(inf.mean ~ year, ylim=c(4,15), pch=19) # Plot the mean inflation rate
  arrows(year, inf.mean - inf.se, year, inf.mean + inf.se, # Add 2 standard deviations
         code=0, angle=90, length=0.02)
  

 # tapply is useful, but it's hard to draw the results back from it  
 # Do this with aggregate  
  aggregate(dat$INF, dat["YEAR"], mean) # This takes 1 line 
                                        # The new variable is saved as "x"
  aggregate(cbind(INF, INF..1.) ~ YEAR, dat, mean) # Can handle multiple variables
  
    
#---------------------------------------------------------------------------------------------#    
# REPLICATE
#---------------------------------------------------------------------------------------------#     
  
  rm(list=ls())
  
  coin <- function(n)    # Coin flip function: calculate number of heads
    sum(runif(n) < 0.5)  # inside the function 
  
  
  coin(10)  
  coin(200)
  
  replicate(100, coin(50)) # Replicate the coin flip experiment (50 flips) 100 times
                           # Remember that "coin()" is a function

 # Monte Carlo Simulation  
  experiment1 <- replicate(5, coin(100))
  experiment2 <- replicate(100, coin(100))
  
  par(mfrow=c(1,2))
  plot(table(experiment1), xlim=c(30,70), ylab="")
  abline(v=mean(experiment1), col="navy", lwd=3)    # Sample mean
  abline(v=50, col="red", lwd=2, lty=2)             # Thaoretical mean
  plot(table(experiment2), xlim=c(30,70), ylab="")
  abline(v=mean(experiment2), col="navy", lwd=3)
  abline(v=50, col="red", lwd=2, lty=2)
    
  
##################################################################################################################    
# END OF THIS R SOURCE FILE
##################################################################################################################    