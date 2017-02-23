######## Test script for udallCheckDataWide.R 
# Instructions: 
#
# 1) set working directory to test 
# 2) source this script
#    ~ error messages should indicate erros within the udallCheckDataWide.R script 


#########################################
########## Libraries ####################
#########################################

library(evaluate) # load this library to evaluate error messages

##########################################
########## Helper Functions ##############
##########################################

#################################
### Evaluate Error Messages #####
# compares expected string error message with actual error message
eval_error <- function(df, chexp) {
  ch <- evaluate("udallCheckDataWide(df)") # list of error information 
  if (length(ch) == 1) {
    stopifnot(length(ch) == chexp)
    #   do not conintue if no match with expected 
  } else {
    stopifnot(ch[[2]][1] == chexp) # index through ch for error message, compare with expected
    #   do not continue if no match with expected 
  }
}

#################################

##########################################
############ Test Data Frame #############
##########################################
test <- data.frame("idnum" = 1:10, "sex" = factor(rep(c(1,2), 5))) # small test data frame
#   "idnum": subject IDs 1-10
#   "sex": factors of 1 and 2 for two levels of gender
#   n = 10 

###########################################
############ Testing function #############
###########################################

udallCheckDataWideTest <- function(df) {
  orig <- df
  #############################
  ### Testing Duplicate IDs ###
  # testing one duplicate 
  df$idnum[3] <- 2
  eval_error(df, "Duplicate IDs for  1 subjects: 2")
  
  # testing a few duplicates
  df$idnum[5:10] <- rep(c(1,2), 3)
  eval_error(df, "Duplicate IDs for  2 subjects: 1 2")
  
  # testing all duplicates 
  df$idnum[1:10] <- rep(c(3,4,5,6,7), 2)
  eval_error(df,"Duplicate IDs for  5 subjects: 3 4 5 6 7")
  #############################
  
  #############################
  ### Testing Sex Column ######
  df <- orig
  eval_error(df, 1) # No missing data
  #   no error expected, length of error output list should be 1
  
  df$sex[5] <- NA # One missing value
  eval_error(df, "Missing sex for  1 subjects:  5")
  #   compare character format of expected error message with error from output list
  
  df$sex[1:7] <- NA # Several (7) missing values
  eval_error(df, "Missing sex for  7 subjects:  1 2 3 4 5 6 7")
  #   compare...expected...with error...
  
  df$sex <- NA # All values missing
  eval_error(df, "Missing sex for  10 subjects:  1 2 3 4 5 6 7 8 9 10")
  #   compare...expected...with error...
  #############################
  
  print("udcallCheckDataWide function is working correctly")
}


udallCheckDataWideTest(test) # call to function 

