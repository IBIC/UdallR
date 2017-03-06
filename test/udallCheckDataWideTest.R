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
    warningIfNot(as.character(ch[[2]][1]) == chexp, paste(chexp)) # index through ch for error message, compare with expected
    #   do not continue if no match with expected 
  }
}

#################################

##########################################
############ Test Data Frame #############
##########################################

test <- as.data.frame(read.csv("UdallR/data/test.csv"))

###########################################
############ Testing function #############
###########################################

udallCheckDataWideTest <- function(df) {
  orig <- df
  #############################
  ### Testing Duplicate IDs ###
  # testing one duplicate 
  df$idnum[1] <- 100054
  eval_error(df, "Duplicate IDs for  1 subjects: 100054")
  
  # testing a few duplicates
  df$idnum[5:10] <- rep(c(100044, 100054), 3)
  eval_error(df, "Duplicate IDs for  2 subjects: 100044 100054")
  
  # testing all duplicates 
  df$idnum[1:10] <- rep(c(100044, 100054, 100165, 110213, 110280), 2)
  eval_error(df,"Duplicate IDs for  5 subjects: 100044 100054 100165 110213 110280")
  #############################
  
  #############################
  ### Testing Sex Column ######
  df <- orig
  eval_error(df, 1) # No missing data
  #   no error expected, length of error output list should be 1
  
  df$sex[5] <- NA # One missing value
  eval_error(df, "Missing sex for  1 subjects:  110280")
  #   compare character format of expected error message with error from output list
  
  df$sex[1:7] <- NA # Several (7) missing values
  eval_error(df, "Missing sex for  7 subjects:  100044 100054 100165 110213 110280 110281 110339")
  #   compare...expected...with error...
  
  df$sex <- NA # All values missing
  eval_error(df, "Missing sex for  24 subjects:  100044 100054 100165 110213 110280 110281 110339 110415 120445 120459 120520 120526 130542 140590 140593 140599 140605 150613 150623 150624 160630 160631 160633 160642")
  #   compare...expected...with error...
  #############################
  
  ###################################
  ### Testing UDALL scoring items ###
  
  #scoreUPDRSTest()
  
  print("udcallCheckDataWide function is working correctly")
}


udallCheckDataWideTest(test) # call to function 

