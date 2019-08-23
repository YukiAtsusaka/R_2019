
###############################################################################################
# SASinR_Day2.R
# Created by Yuki Atsusaka
# Last updated: August 20, 2019
# Since: July 28, 2019
# Aim: to learn basic operation in R (Day 2 of Social Analysis and Simulation in R)
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
# SETTING UP & OTHER INFORMATION
###############################################################################################    

rm(list=ls()) # rm() cleans up your R. list=ls() means everything in the environment
gc();gc()     # gc() cleans up your memory


###############################################################################################    
# CONDITIONS
###############################################################################################    
# Boolean (data type) is a form of data that takes TRUE or FALSE

 rm(list=ls())
 country <- c("USA", "Canada", "Mexico", "Canada", "Mexico", NA) # Store a vector

 country
 country == "Canada"  # Make a conditional statement
 country != "Canada"  # Not equal to
 
 country == "Canada" | country == "USA" # Or
 country == "Canada" & country != "USA" # And 
 
 value <- c(1,2,3,4,5,6)
 dat <- data.frame(value, country)
 dat
 
 dat[country=="Canada",]  # Only a subset of data which satisfies the condition
 
 is.na(country)           # Missing value
 dat[is.na(country)==FALSE,]
 

###############################################################################################    
# MATRICES (I)
###############################################################################################    
# You can create matrices and perform various operations on them
# You can also create arrays (multiple matrices saved in one object)
 
 rm(list=ls())
 dat <- c(1,1,1,1,2,2,2,2,3,3,3,3)
 mat <- matrix(dat, nrow=4, ncol=3)
 mat
 
 mat2 <- matrix(c(1,2,3, 4,5,6), nrow=3, ncol=2,
         dimnames=list(c("Rich", "Middle", "poor"),c("Turnout", "Abstain")))
 mat2
 
 mat2[1,]      # First row
 mat2[,2]      # First column
 colSums(mat2) # Sum over all rows by columns
 rowSums(mat2) # Sum over all columns by rows

 # (Matrix, Dimension, Function to apply)
 apply(mat2, 1, mean) # Take means on the first dimension = rows
 apply(mat2, 2, mean) # Take mans on the second dimension = columns

 
 mat_identity <- diag(nrow=3) # Identity matrix
 mat_identity

 t(mat)      # Transpose the matrix
 mat         # Original matrix
 
 vec <- c(1,2,1)        # 3 x 1 Matrix (Vector)
 mat * 2                # Scalar multiplication
 mat %*% vec            # Vector multiplication
 vec %*% mat            # Error....why?
 mat %*% mat_identity   # Matrix multiplication
  

 array(1:24, dim=c(3,4,2)) # Array: you can see two matrices inside
 

###############################################################################################    
# MATRICES (II): ORDINARY LEAST SQUARES
###############################################################################################    
# Illustrate how to perform OLS by hand via matrix calculation
# OLS estimator is XtX.inv Xty 
 
 rm(list=ls())
 library(readstata13)
 dat <- read.dta13("anes_timeseries_2016_Stata13.dta")
 dat <- dat[dat$V161086 >= 0,]                   # Drop obs who have missing values
 dat <- dat[dat$V162171 <= 7 & dat$V162171 >=1,] # Same as above
 dat <- dat[dat$V162336 <= 7 & dat$V162336 >=1,] # Same as above: we have 3003 obs

# Similarly, you can do:
 dat_sub <- subset(dat, dat$V161086 >=0)
  
# With missing values, you could potentially do:
 dat_comp <- dat[complete.cases(dat), ]  # We have 1737 obs (Dropping all cases with at least one NA)
 dat_comp <- dat[is.na(dat$V161086)==F,] # Only keep rows whose values are not missing
   
 
 # Extract several variables
 feeling <- dat$V161086               # Feeling thermometer for Hilary Clinton
 female <- ifelse(dat$V161342==2,1,0) # Female
 cons <- dat$V162171                  # Conservatism
 anxious <- dat$V162336               # How "anxious, easily upset" R is


 # Now, we write these as vector and matrix 
 y <- feeling                         # Store the outcome as a vector
 X <- as.matrix(cbind(1, female, cons, anxious)) # Store predictors as a matrix

 head(y) # See the outcome vector
 head(X) # See the predictor matrix

 Xt <- t(X)                  # X transposed
 XtX.inv <- solve(Xt %*% X)  # (X transpoed X)^(-1) (inverse)
 Xty <- Xt %*% y             # X transposed y
 BETA.hat <- XtX.inv %*% Xty # (X transposed X)^(-1) * (X transpoed y)
 
 print(BETA.hat)             # Show the result
 
 m_ols <- summary(lm(feeling ~ female + cons + anxious))
 m_ols                       # Compare with the pre-written program
 
 library(xtable)             # Review of Day 1
 xtable(m_ols)               # You can copy and paste this to your R Markdown file

###############################################################################################    
# PROBABILITY DISTRIBUTIONS
###############################################################################################    
# You can utilize various probability distributions to
# perform simulation and calculate other quantities
# You need to understand density, value, cumulative probability 
 
 rm(list=ls())
 
 dnorm(x=1.96, mean=0, sd=1) # Draw a density of the value = 1.96 from N(0,1)

 pnorm(q=1.96, mean=0, sd=1) # Show a cumulative probability for x = 1.96
 pnorm(q=1.96, mean=0, sd=1) - pnorm(q=-1.96, mean=0, sd=1) # What is this?
 
 qnorm(p=0.975, mean=0, sd=1) # Show x-value that has 0.975 as a cumulative probability
 
 rnorm(n=10, mean=0, sd=1)  # Draw values from N(0,1): We use this for simulation a lot
 
 # Other probability distributions are available
 rbinom(n=10, size=1, prob=0.5) # Binomial Distribution
 rpois(n=10, lambda=0.2)        # Poisson Distribution
 rexp(n=10, rate=0.2)           # Exponential Distribution
 rt(n=10, df=2)                 # Student's t Distribution
 # and Others. You name it.
 
 
 # You can also draw from multivariate distributions
 library(MASS)
 mu_par <- rep(0,2)
 Sigma_par <- matrix(c(10,3,3,2), nrow=2, ncol=2)
 mu_par         # Mean vector
 Sigma_par      # Variance-covariance matrix
 
 draw <- mvrnorm(n=1000, mu=mu_par, Sigma=Sigma_par) # Bivariate Normal
 head(draw)  
 plot(draw) # Visualize the result
            # Play with the SIGMA matrix to see how the reuslt changes
 
 
###############################################################################################    
# SIMULATION (I): MONTE CARLO SIMULATION
###############################################################################################    
# Monte Carlo Simulation
# Even when you don't know the theoretical property of a distribution,
# if you can draw a bunch of values from the distribution and plot a hitgoram
# you can fully recover the distribution!
 
 rm(list=ls())
 
 # Play with the size of "n" to see if the simulated mean approches 0
 normal <- rnorm(n=10, mean=0, sd=1)
 hist(normal, breaks=40) 
 abline(v=mean(normal), col="red", lwd=2)
 
 
 # Simulation is powerful when you don't know the shape of the distribution
 # Or you forgot it from your intro-statistics class
 
 # Here, simulate Exponential(0.2) distribution
 sim_mean <- NA     # Make a storage of values outside the loop
 for(i in 1:5000){  # Repeat inside for each values in 1:50000
  drawn_values <- rexp(n=i, rate=0.2)
  sim_mean[i] <- mean(drawn_values) 
 }
 
 temp <- seq(from=0, to=40, by=0.01)  # Specify a range for below
 true_density <- dexp(temp, rate=0.2) # Draw a true theoretical density for comparison

 # Compare simulation to theoretical density
 par(mfrow=c(1,2)) 
 hist(drawn_values, breaks=40, xlab="Drawn Values", main="Monte Carlo Simulation of Exp(0.2)")
 plot(true_density ~ temp, type="l", main="Theoretical-True Density of Exp(0.2)") 
 
  
 # To fully recover the density, you need to draw a lot of values
 xrange <- 1:5000
 par(mfrow=c(1,1))
 plot(sim_mean ~ xrange, type="l", xlab="Number of Draws", ylab="Simulated Mean")
 title("Monte Carlo Simulation for Exp(0.2)")
 

 # Setting seed for pseudo-random number generator
 rnorm(n=1, mean=0, sd=1) # We obtain different values
 rnorm(n=1, mean=0, sd=1) # for each simulation

 set.seed(12212012)       # Set right before when you draw values
 rnorm(n=1, mean=0, sd=1) # Now, we get the same value
 set.seed(12212012)
 rnorm(n=1, mean=0, sd=1) # no matter when you do it
 
   
###############################################################################################    
# SIMULATION (II): OBSERVATIONAL DATA
###############################################################################################    
# You can create hypothetical datasets using what we've learned so far 
# Here, we want to create the following type of data of 100 hypothetical precincts
# where we know the number of blacks and whites, and voters who chose DEM and REP candidates
# We start from individual level DGP (Data generating process) and then 
# aggregate them into precinct level data
   
#       precinct black white total vdem vrep 
# 1          1    45   375   420  156  264  
# 2          2    60   318   378  135  243  
# 3          3    40   347   387  136  251  
# 4          4    42   350   392  138  254  
# 5          5    56   353   409  159  250  
# 6          6    47   370   417  175  242
# 7          7    62   344   406  156  250
# 8          8    48   346   394  147  247
# 9          9    34   335   369  135  234  
# 10        10    55   339   394  151  243

 
 rm(list=ls())
 set.seed(11232017) # Set seed for pseudo-random number generator
 
 simN <- 40000      # NUMBER OF TOTAL VOTERS
 
 # (1-1) GENERATE CORRELATED RACE AND PARTY VARIABLES (i.e., BLACK AND DEMOCRAT) 
 black <- rbinom(n=simN, size=1, prob=0.12)                  # 12% = BLACK, 88% = WHITE
 partyid0 <- rep(NA, simN)
 partyid1 <- ifelse(black==1 & runif(simN)<0.9, 1, partyid0) # 90% OF BLACKS = DEMOCRAT
 partyid2 <- ifelse(black==0 & runif(simN)<0.4, 1, partyid1) # 40% OF WHITES = DEMOCRAT
 democrat <- ifelse(is.na(partyid2)==T, 0, 1)                # NON-DEMOCRAT = REPUBLICAN
 
 sim.data <- data.frame(black, democrat)
 head(sim.data)
 sum(democrat)
 sum(black)
 
 counts <- t(table(sim.data$black, sim.data$democrat))
 counts

 par(mfrow=c(1,1))   
 barplot(counts, main="PartyID by Race", xlab="", beside=T,
         col=c("firebrick", "navy"), legend=c("Republican", "Democrat"),
         axisnames=T, names.arg=c("White", "Black"))
 rho <- cor(black, democrat)
 rho   # CORRELATION OF RACE AND PARTYID

 rm(partyid0, partyid1, partyid2, rho, black, democrat)
 
##############################################################################  
 
 
############################################################################## 
# (1-2) GENERATE VOTING BEHAVIOR BASED ON UTILITY FUNCTION FOR DEMOCRATIC VOTING
 error      <- rnorm(n=simN, mean=-1, sd=1)    # HOMOSKEDASTIC ERROR FOR BOTH PARTY
 b.black    <- runif(n=simN, min=0.4, max=0.4) # COEF OF BLACK: START FROM A SIMPLE MODEL
 b.democrat <- runif(n=simN, min=1.2, max=1.2) # COEF OF DEM: START FROM A SIMPLE MODEL
 
 # UTILITY FUNCTION FOR DEMOCRATIC VOTING 
 utility.d <- b.black*black + b.democrat*democrat + error  # UTILITY FUNCTION OF VOTING FOR DEMOCRATIC CANDIDATE

 hist(utility.d, breaks=40, col = "#ff00ff40", border = "#ff00ff", xlim=c(-5,5),
      main="Distribution of Utility Function")
 vote.d <- ifelse(utility.d > 0, 1, 0)        # Vote for DEM if Utility > 0
 sim.data$vote.d <- vote.d
 
 # par(mfrow=c(1,2))   
 # hist(vote.d, xlab="Vote Choice(1=Democrat)", main="Votintg")          # HISTOGRAM OF DEMOCRATIC VOTING
 # hist(sim.data$democrat,xlab="Party ID(1=Democrat)", main="Party Identification") 
 
 # CHECK INDIVIDUAL ASSOCIATION (BLACK AND DEMOCRAT ARE BOTH SIGNIFICANT !) 
 model <-  glm(vote.d ~ black + democrat, family=binomial(link="logit"), data=sim.data)
 summary(model)     # RECOVERED THE DATA GENERATING PROCESS (OK) 
 
 rm(b.black, b.democrat, error, utility.d, counts, vote.d)
##############################################################################   


############################################################################## 
 # (1-3) CREATE PRECINCTS AND ALLOCATE VOTERS 
 
 # RANDOM SAMPLING TO PROBABILITY SAMPLING
 # Here, we randomly sample a value from (1...100) and define it as a precinct number for each observation
 precinct <- sample(1:100, size = nrow(sim.data), replace = TRUE)
 sim.data$precinct <- precinct
# hist(sim.data$precinct, breaks=100)  
 
 # PRECINCT LEVEL VOTES, BLACKS, DEMOCRATS    
 agg.vote<-c()
 agg.black<-c()
 agg.democrat<-c()
 temp<-c()
 agg.total<-c()
 
 for(i in 1:100) {   
   agg.vote[i] <- sum(sim.data$vote.d[precinct==i])
   agg.black[i] <- sum(sim.data$black[precinct==i])
   agg.democrat[i] <- sum(sim.data$democrat[precinct==i])
   agg.total[i] <- length(sim.data$precinct[precinct==i]) # NUMBER OF VOTERS IN EACH PRECINCT
 }
 
# par(mfrow=c(1,3))   
# hist(agg.vote, main="Precinct Level Vote", freq=F)
# lines(density(agg.vote), col="orange")
# hist(agg.black, main="Preinct Level Black", freq=F)
# lines(density(agg.black), col="orange")
# hist(agg.democrat, main="Precinct Level Democrat", freq=F)
# lines(density(agg.democrat), col="orange")
 
 # CREATE PRECINCT LEVEL DATA
 precinct <- 1:100
 
 black <- agg.black
 white <- agg.total - black
 vdem <- agg.vote           # VOTES FOR DEMOCRATS
 vrep <- agg.total - vdem   # VOTES FOR REPUBLICAN
 ndem <- agg.democrat       # NUMBER OF PEOPLE WHOSE PARTY ID == DEMOCRAT
 nrep <- agg.total - ndem   # NUMBER OF PEOPLE WHOSE PARTY ID == REPUBLICAN
 total <- black + white
 agg.data <- data.frame(precinct, black, white, total, vdem, vrep, ndem, nrep)
 
 rm(precinct, agg.vote, agg.black, agg.democrat, agg.total, black, i, white,
    vdem, vrep, total, ndem, nrep, temp, simN)
 
 agg.data
 
 write.table(agg.data, "sim_precinct.csv", sep=",", row.names=F) # Output the data
 
 
 
 # SIMPLE ANALYSES
 summary(lm(total ~ black + ndem, data=agg.data))
 summary(glm(total ~ black + ndem, family=poisson(link="log"), data=agg.data))
 
 summary(model) # Compare with the individual level model
 

 
###############################################################################################    
# SIMULATION (III): NOTE ON HOMOEWORK 3
###############################################################################################    
# When simulating "Y" values, please follow the procedure below
# This is based on the potential outcomes framework
# You may learn more in POLI 506 (Causal Inference) in Fall 2019

rm(list=ls())

# Global parameters
set.seed(20190820)
N = 1000                                   # Population size
N_samp <- 50                               # Sample size

# Population parameters 
Covar_1 <- rbinom(n=N, 1, 0.4)             # Some binary covariate
Covar_2 <- rnorm(n=N, mean=0, sd=2)        # Some continuous covariate  

a = 0.5                                    # Intercept: Ground mean
b = 2                                      # (Fixed=constant) Effects of Covar_1 
b2 = -2                                    # (Fixed=constant) Effects of Covar_2
tau = 4                                    # (Fixed=constant) Treatment effect  
e = rnorm(n=N, mean=0, sd=1)               # N(0,1) error

Y_0 <- a + tau*0 + (b*Covar_1) + (b2*Covar_2) + e  # Potential outcome when not treated
Y_1 <- a + tau*1 + (b*Covar_1) + (b2*Covar_2) + e  # Potential outcome when treated
                                           
pop_dat <- data.frame(Y_0, Y_1, Covar_1, Covar_2) # Population level data
head(pop_dat)
#*** Values are defined at the population level up to this point

# Now, we consider our sample from here
sample_ind <- sample(1:nrow(pop_dat), size=N_samp)     # Sampling index
sample_dat <- pop_dat[sample_ind, ]                    # Only keep obs that match the index
head(sample_dat)

d <- ifelse(runif(N_samp)<=0.5, 1, 0)                        # Treatment assignment indicator (1=Treated, 0=Controlled)
sample_dat$Y_obs <- d*sample_dat$Y_1 + (1-d)*sample_dat$Y_0  # Observed outcomes
                                                             # Y_obs = d*Y_1 + (1-d)*Y_0
sample_dat$Status <- ifelse(d==1, "Treated", "Control")      # Copying the treatment status into sample data
 
head(sample_dat)


# Visualize in your favorite plots
boxplot(sample_dat$Y_obs[sample_dat$Status=="Treated"],
        sample_dat$Y_obs[sample_dat$Status=="Control"],
        names=c("Treated", "Control"))


t <- c(0,1)
y_mean <- c(mean(sample_dat$Y_obs[sample_dat$Status=="Control"]),
            mean(sample_dat$Y_obs[sample_dat$Status=="Treated"]))
y_sdv <- c(sd(sample_dat$Y_obs[sample_dat$Status=="Control"]),
           sd(sample_dat$Y_obs[sample_dat$Status=="Treated"]))

plot(y_mean ~ t, pch=16, ylim=range(c(y_mean-y_sdv, y_mean+y_sdv)),
     xlab="Treatment Status", ylab="Y_obs ± sd",xaxt="n") # Without x-axis lable
axis(1, at = seq(00, 1, by = 1), las=1) # If las=2, numbers will be flipped by 90 degree
arrows(t, y_mean-y_sdv, t, y_mean+y_sdv, length=0, angle=90, lwd=3)
title("Simulated Result")


#-------------------------------------------------------------------------------------------------#
# EXTRA NOTES:::
#-------------------------------------------------------------------------------------------------#
# You can also perform a T-test (difference in means test)
t.test(Y_obs ~ Status, data=sample_dat)


# Estimating the average treatment effect
tau_hat <- mean(sample_dat$Y_obs[sample_dat$Status=="Treated"]) -
           mean(sample_dat$Y_obs[sample_dat$Status=="Control"])
tau_hat

# Population level treatment (fixed=constant) effect
tau_true <- mean(pop_dat$Y_1 - pop_dat$Y_0) # Mean Difference of Individual Potential Outcomes
tau_true
tau_true == tau


##################################################################################################################    
# END OF THIS R SOURCE FILE
##################################################################################################################    