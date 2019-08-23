
###############################################################################################
# SASinR_Day1.R
# Created by Yuki Atsusaka
# Last updated: July 28, 2019
# Since: July 23, 2019
# Aim: to learn basic operation in R (Day 1 of Social Analysis and Simulation in R)
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

# If you encounter + + + + + + problem, click "Esc" (escape) to get out of it
# You can write comments after "#" mark. R will recognize whatever comes after # as comments
# (It's called comment-out)
# Both "<" and "=" can be used for assignment
# If you want to see "Help" on some commands, you can always type "? + command"
# (For example, ?print, ?scatter, ?lm; if you cannot find any, try "?? + command")

###############################################################################################    
# PRINT
###############################################################################################    
# You can print words

 print("Hello world") # String or object
 print(1) # Numeric 
 print( paste("Math prep", 2, sep=": ") ) # Combining two parts

###############################################################################################    
# CALCULATION
###############################################################################################   
# You can perform (scholar) calculation
  
 1 + 2
 3 * 6
 exp(1) # Some function can be used via pre-specified function command
 log(1)
 
###############################################################################################    
# VARIABLES / OBJECTS
###############################################################################################    
# You can store values in variables
# You can then perform calculation or manipulation with those variables
# There are different "classes" of objects in R  
 
  x <- 45
  y <- 90
  y - x
  x_quote <- "45"
  y_quote <- "90"
  y_quote - x_quote # See the error message
  
  word <- "Mission Impossible"
  part <- "Rouge Nation"
  print(word)
  print(part)
  print(paste(word, part, sep=": "))
 
  class(x)
  class(word)  
  class(log)
  
  rm(x) # Remove objects from the environment
  
###############################################################################################    
# LIST / VECTOR
###############################################################################################    
# List is a one-dimensional array of multiple values  
# You can access to any position in lists  
# List as a generic vector containing multiple objects is covered in Day 3 ***  
# Here, I use the term "list" as "vector" (list is technically a vector)

 rm(list=ls()) # Clean the environment  
  
 list.one <- c(1,2,3,4) # c = concatenate
 print(list.one)        # Show the list 
 list.one               # You can also directly type out
 list.one[3]            # Obtain 3rd Item
 list.one[1:3]          # Obtain 1st to 3rd items
 list.one[c(2, 4)]      # Obtain 2nd and 4th items
 list.one[-1]           # Obtain except for 1st item
 list.one / 0.1         # Perform calculation on lists 
 list.one / list.one[4] 
  
 list.one2 <- c(5,6,7,8)
 list.one_comp <- c(list.one, list.one2) # Concatenating two lists into one
 list.one_comp
 list.one_comp[c(7,8)] <- c(99, 99) # Overwrite 7th and 8th positions with new values
 list.one_comp
 
 list.two <- seq(from=-100, to=100, by=0.01) # Make a sequence of numbers
 print(list.two)
 head(list.two)          # Check the first several obs
 head(list.two, n=20)    # Specify the number to show
 rm(list.two)            # Delete this list
 rm(list.one, list.one2) # Delete multiple lists
 
###############################################################################################    
# FUNCTIONS (I)
###############################################################################################     
# You can apply various functions to lists and objects 
# You can also combine functions in the same line 
# You can use pre-programmed functions called "packages" 
# You can also write your own function (We will cover this in FUNCTIONS (II) of Day 2) 
 
 length(list.one_comp) # Length of lists (i.e., Number of values)
 abs(list.one_comp)    # Absolute value operator
 sum(list.one_comp)    # Summation operator
 prod(list.one_comp)   # Product operator
 exp(list.one_comp[1:3])
 log(list.one_comp[c(2,4,6)])

 min(list.one_comp); max(list.one_comp) # You can stack operations by ";"
 range(list.one_comp)
 mean(list.one_comp); median(list.one_comp) # If Mean is way smaller/larger than Median, the distribution is skewed
 
 sum(list.one_comp) / length(list.one_comp) # Arithmetic mean

 
 installed.packages("pracma") # Installing a package = pre-programmed tools with functions
                              # You can also go "Tools" < "Install Packages.." in RStudio
 library(pracma)              # Reading a package
 nthroot(x=prod(list.one_comp), n=length(list.one_comp) ) # Geometric mean

  
 names(list.one_comp)
 names(list.one_comp) <- c("Texas","North Carolina", "Colorado", "Texas",
                           "Alabama", "Oregon", "New York", "Oregon") # Assign names
 list.one_comp
 names(list.one_comp)           # Obtain names only
 unique( names(list.one_comp) ) # Obtain unique names
  
 
###############################################################################################    
# LOOP (I) and IF-ELSE CONDITIONS
###############################################################################################     
# You can create for, while, repeat loops in R
# You can specify conditions on which operations take place by if-else

 rm(list=ls()) 

 # For loop
 for(i in 1:10){   # Loop the following operation over the list of number i=1,....10
   cat("Current number is:", i, "\n") # "\n" creates another index
 }
 
 # While loop
 num <- 1          # Initial value for an index
 while(num <= 7){  # While loop: repeat while the condition holds
   cat("Current number is less than 8:", num, "\n") 
   num = num + 1   # Update the index
    }
 
 # Repeat loop
 val <- 1
 repeat {          # Repeat the following until the break happens
   cat("We keep doin' this:", val, "\n") 
   val = val+1     # Update the index
   if (val == 6){
     break         # Stop the loop (break out of the loop)
   }
 }
 
 
 val_list <- 1:10         # You can prespecify the list in R
 n <- length(val_list)    # Always keep the number of loops
 outcome <- rep(NA, 10)   # Create a storage for outcomes

  for(k in 1:n){          # Index can be anything (other than i)
   outcome[k] <- 2*k + 3   # You can put "k" (index) directly into an equation
   cat("We multiplied", val_list[k], " by 2 and added 3 to obtain", outcome[k], "\n")
 }
 
 
 # Loop with one condition 
 result <- rep(NA, 10)
 for(j in 1:10){

   if (j >= 5){         # If condition starts here
      result[j] <- 2*j
   }                   # If condition ends here

   print(result[j])      # Printing results
#   cat(result[j], "\n")  # Concatenating the representations (compare)
    }
 
 
 # Loop with two conditions
 result <- rep(NA, 10)
 for(j in 1:10){
   
   if (j >= 5){        # If condition starts here
     result[j] <- 2*j
   }                   # If condition ends here
   
   else{               # Else condition starts here
     result[j] <- j/2  
   }                   # Else condition ends here
   
   print(result[j])      # Printing results
   #   cat(result[j], "\n")  # Concatenating the representations (compare)
 }
 

 
 # Loop with three conditions
 result <- rep(NA, 10)
 for(j in 1:10){
   
   if (j >= 7){       
     result[j] <- 100*j
   
      } else if (j >= 4){  # You can further specify condition            
     result[j] <- 10*j
   
      } else{              # "else" is always the residual category
     result[j] <- j
      }  
   
   print(result[j])      # Printing results
 }
 
 
 
###############################################################################################    
# READING DATA
###############################################################################################    
# You can use R's built-in datasets
# You can read your own data from your working environment
 
 rm(list=ls())
 library(foreign)
 
 data()        # See the pre-loaded datasets
 data("uspop") # US POPULATION CENSUS DATA

 head(uspop) 
 uspop 
 View(uspop)

 write.table(uspop, "uspop_save.csv", sep=",", row.names=F) # Output in csv
 write.dta(as.data.frame(uspop), "uspop_save.dta")          # Output in dta
 save(uspop, file = "uspop_save.RData")                     # Output in RData

 rm(list=ls()) 
 load("uspop_save.RData")                                   # Try loading RData
  
 
 data("sleep") # STUDENTS' SLEEPING DATA
 head(sleep)   # Show the head of the data
 sleep         # Show the entire data
 View(sleep)   # View the entire data in a spread sheet format
               # You can also lick "table mark" in the environment
 rm(uspop,sleep) # Remove datasets
 
 
 getwd()     # Get working directory (data files must be in here)
 setwd("C:/Users/YUKI/Desktop/Folders/CourseWorks/10. Teaching/3. Social Analysis and Simulation in R 2019")
             # You can set your working directory like this
             # Or, you can go "Session" < "Set Working Directory" 
             # If you choose "To Source Location", it'll set the location of your source code (R Code)

 
 dat <- read.csv("franseze.csv", header=T, sep=",") # Read the data
                                                    # Header argument
                                                    # Seperation argument
 # Data in other formats can be loaded as well
 # read.dta("data_name.dta", header=T)              # dta file with version less than 13
 # read.dta13("data_name.dta", header=T)            # Over 13 (must do "library(readstata13)") first
 # read.table("data_name.txt", header=T, sep=",)    # Text file

 head(dat)  
 colnames(dat) # Variable names
 dim(dat)      # Dimension
 dim(dat)[1]   # Number of rows
 dim(dat)[2]   # Number of columns
 
 
 # Now, you can acess any data point in the data in a quasi-matrix form (data.frame)
 dat[1,1]      # 1st row, 1st column (1st observation, 1st variable)
 dat[1,1:3]    # 1st row, 1-3rd columns
 dat[1, ]      # 1st row, all columns
 dat[1:5, 1]   # 1-5th rows, 1st column
 dat[1:5, 1:3] # 1-5th rows, 1-3rd columns  

 dat_mat <- as.matrix(dat) # Transform into a matrix class
 class(dat)                # This is data.frame
 class(dat_mat)            # This is matrix
  

 words <- rep("Version1", times=dim(dat)[1]) # rep(x, times): replicats values in x
 
 dat$new_variable <- words # Create a variable named "new_variable" and put "words" in it
 dat_mat <- as.matrix(dat) # Again, transform into matrix form
 head(dat_mat)             # See what happens (data.frame v. matrix)
 
 colnames(dat) <- c("Country","Year", "Inflation", "Inflation_lag", "Inflation_lag2", 
                    "CBI_lag1", "G", "U", "T", "NFA", "F", "CWB", "Version1") # Give new names
 head(dat)
 
 dat_abb  <- dat[,1:4] # Subset the data (keeping only 1-4th columns)
 dat_abb2 <- dat[1:5,] # Subset the data (keeping only 1-5th rows) 
 head(dat_abb)
 head(dat_abb2)
 dim(dat_abb)
 dim(dat_abb2)
 
 
###############################################################################################    
# DATA MANIPULATION
###############################################################################################     
# You can perform various data manipulation
 
 rm(list=ls())
 dat <- read.csv("franseze.csv", header=T, sep=",") # Read the data

 country <- dat[,1] # Extract only the country variable
 
 dat2 <- dat # It is always a good idea to keep the original data and 
                 # perform data modification on its copy
 
 unique(dat2$YEAR) # Show unique values in "YEAR"
 dat2$YEAR_dummy[dat2$YEAR <= 1979] <- "Old" # You can subset data with conditions inside []
 dat2$YEAR_dummy[dat2$YEAR > 1979] <- "New"  
 
 # You can do the same thing with "ifelse" command
 dat2$YEAR_dummy_if <- ifelse(dat2$YEAR <= 1979, "Old", "New") # see ?ifelse
 dat2$YEAR_dummy_if <- ifelse(dat2$YEAR <= 1979, 0, 1)
 dat2$YEAR_dummy_if <- ifelse(dat2$YEAR <= median(dat2$YEAR), 0, 1) # You can combine with function
 
 dat2 <- dat2[,c(1:4, 14)] # Further subsetting the data (see how "c()" does its job)
 head(dat2) 

###############################################################################################    
# DATA EXPLOTATION: SUMMARY STATISTICS AND VISUALIZATION
###############################################################################################     
# You can show various summary statistics and visualize properties of the data
 rm(list=ls())
 dat <- read.csv("franseze.csv", header=T, sep=",") # Read the data

 summary(dat$INF) # Show summary statistics for one variable
 summary(dat)     # Show them for all variables
 
 hist(dat$INF)    # Draw a histogram
 hist(dat$INF, breaks=40) # Change the size of bins
 hist(dat$INF, xlab="Inflation Level", ylab="Frequency", main="Inflation Variable") 
 hist(dat$INF, xlim=c(-10, 40)) # Change the range of x-axis
 hist(dat$INF, xlim=c(-30, 60)) # Change the range of x-axis
 
 # You can add more information
 hist(dat$INF, xlab="Inflation Level", ylab="Frequency", main="Inflation Variable") 
 abline(v=mean(dat$INF),   col="navy", lwd="2", lty=2)      # Add a line
 abline(v=median(dat$INF), col="firebrick", lwd="2", lty=2) # Add another line
 legend("topright", legend=c("Mean", "Median"),             # Add legend
        col=c("navy", "firebrick"), lty=c(2,2)) 
 
 # You can save your plot by code as well as by hand
 pdf("Inf_hist.pdf")
 hist(dat$INF)
 dev.off()
 
 
 boxplot(dat$INF, dat$CBI..1., dat$G..1.) # Box plot
 
 library(vioplot)
 vioplot(dat$INF, dat$CBI..1., dat$G..1., col="gold") # Violin plot
 
 
 # Look at the bivariate relationships
 cor(dat$INF, dat$CBI..1.)      # Correlation coefficient
 cor(dat[,1:4])                 # Correlation matrix
 round(cor(dat[,1:4]), digit=2) # Rounding numbers
 
 library(corrplot)
 co_mat <-  cor(dat[,1:6])            # Saving correlation matrix
 corrplot(co_mat, type="upper")
 
 library(PerformanceAnalytics) 
 chart.Correlation(dat[,1:6], histogram=TRUE, pch=19)

 
 # Scatter plots
 par(mfrow=c(1,2))  # This controls the number of plots in one windown
                    # Or, more precisely, numbers of rows and columns
                    # via c(# row, # column)
 plot(dat$INF ~ dat$CBI..1.) # Y-axis ~ X-axis 
 plot(dat$CBI..1., dat$INF)  # X-axis, Y-axis

 
 par(mfrow=c(1,1))  # You need to reset this if you want to change the setting
 plot(dat$INF ~ dat$CBI..1., col="dimgray", cex=2, pch=4,  # Color, Size, Shape
                             xlab="CBI Score", ylab="Inflation Level")
 abline(h=mean(dat$INF), lty=2)      # Add a horizontal line
 abline(v=mean(dat$CBI..1.), lty=2)  # Add a vertical line
 arrows(x0=0.7, y0=20, x1=0.65, y1=17, length=0.1, angle=30) # Add an arrow
 text(x=0.75, y=22, labels="Peculiar case")                  # Add a text
 title("Does Central Bank Independence Affect Inflation Rate?:\nKinf of.. (Correlation = -0.36 )")
 # Add a title later
 # Using "\n" let you use another line 
 
 # You can add more information via shapes and colors
 dat$Col <- ifelse(dat$YEAR <= 1979, "red", "blue")
 plot(dat$INF ~ dat$CBI..1., col=dat$Col, cex=2, pch=4,  # Color, Size, Shape
      xlab="CBI Score", ylab="Inflation Level")  
 legend("topright", legend=c("-1979", "1980-"), col=c("red", "blue"), pch=4)
   
 
###############################################################################################    
# DATA EXPLOTATION: RUNNING REGRESSIONS
###############################################################################################     
 
 rm(list=ls())
 dat <- read.csv("franseze.csv", header=T, sep=",") # Read the data
 
 # OLS (Ordinary Least Squares)
 m_ols <- lm(INF ~ CBI..1., data=dat) # Normal-identity model
 m_ols                                # Simple result

 # How to access the information in the saved model
 m_ols # Put "$" right after this command and see what happens 
 m_ols$coefficients      # Obtain alpha and beta coefficients
 m_ols$coefficients[2]   # Only beta coefficient
 
 m_ols$residuals         # Residuals
 m_ols$fitted.values     # Fitted values (predicted values)
 m_ols$model             # "model" includes all the data points you used to estimate the model
                         # So, "model" is more like "data" here 

 summary(m_ols)                # Detailed result
 summary(m_ols)$coefficient    # Obtain more detailed information
 summary(m_ols)$coefficient[7] # Extract p-value (You may want to draw a histogram of p-values)
 
  
 # GLM (Generalized Lienar Models)
 m_glm_lin <- glm(INF ~ CBI..1., family="gaussian", data=dat) 
 m_glm_lin
 m_ols

 dat$INF_binary <- ifelse(dat$INF >= mean(dat$INF), 1, 0) # Make a dichotomize the outcome
 m_glm_logit <- glm(INF_binary ~ CBI..1.,  family="binomial",  data=dat) # Bernoulli-logistic model
 summary(m_glm_logit) 
 
 
 # With more variables
 m_ols_multi <- lm(INF ~ CBI..1. + INF..1. + as.factor(YEAR), data=dat) # Normal-identity model
 summary(m_ols_multi)  # Detailed result
 
 
 # A Tip for outputting
 library(xtable) 
 xtable(m_glm_logit) # You can output the result in a LaTeX format
 
 
 # Combine with scatter plots
 plot(dat$INF ~ dat$CBI..1., pch=16) 
 abline(m_ols, col="red")             # This draws a fitted line
 
 
 range(dat$CBI..1.)                   # Check the range of x-values
 xrange <- seq(from=0, 1, by=0.01)    # Specify the range of x-axis
 pred_prob <- predict(m_glm_logit, list(CBI..1. = xrange), type="response") # Estimate predicted values
 plot(dat$INF_binary ~ dat$CBI..1., pch=16, xlim=c(0,1)) 
 lines(pred_prob ~ xrange, col="red") # This adds a predicted value line
 
###############################################################################################    
# PRAISE
###############################################################################################
 
 rm(list=ls())
 library("praise")
 
 praise()
 
  
 

 
 
##################################################################################################################    
# END OF THIS R SOURCE FILE
##################################################################################################################    