#################################################
##### Test script for scoreUDPRSTest function 
#### ~ compares computed total score on the UPDRS Part III with expcted when original values are replaed
#### by missing values 
#### Before running function, set working directory to UdallR's parent directory 
# source private scoreUPDRS.R functions
source("UdallR/R/scoreUPDRS.R")

# test data frame from data of original data frame, read as csv and as data.frame
testdf <- as.data.frame(read.csv("UdallR/data/test.csv"))

# Compares UPDRS score when desired (..., row, col, ...) dimensions are NA with expected vector.
# 'row' is false when wanting to compare total with entire column as NA. 
compare_total_with_NA <- function(df, row, col, exp, total_col) {
  if (row == FALSE) { # if row is specified as FALSE, replace entire cclumn with NA
    df[, col] <- NA
  } else { # otherwise, replace element at specified (..., row, col, ...) dimension
    df[row, col] <- NA
  }
  obs <- scoreUPDRS(df)[, total_col] # total score if specified values are NA
  stopifnot(obs == exp) # compare observed total score with expected
}

####### Functions testing scoreUPDRS(): An error will be produced if tests do not pass. 

## Test function for "on" UPDRS Part III
scoreUPDRS3Test <- function(dat) {
  ######################
  #### Test Missing ####
  ######################
  ### UPDRS PART III ###
  # row: 1 column: 153
  compare_total_with_NA(dat, 1, 154, c(18, 25, 30, 11, 45, 29, 41, 33, 19, 37, 14, 25, 30, 17, 3, 1, 0, 0, 0, 2, 0, 1, 0, 19), "on_updrs_3_total")
  
  # row: 20 column: 153
  compare_total_with_NA(dat, 20, 154, c(19, 25, 30, 11, 45, 29, 41, 33, 19, 37, 14, 25, 30, 17, 3, 1, 0, 0, 0, 2, 0, 1, 0, 19), "on_updrs_3_total")
 
  # row: 10 column: 153
  compare_total_with_NA(dat, 10, 154, c(19, 25, 30, 11, 45, 29, 41, 33, 19, 35, 14, 25, 30, 17, 3, 1, 0, 0, 0, 2, 0, 1, 0, 19), "on_updrs_3_total")
  
  # row: 0 column: 153
  compare_total_with_NA(dat, FALSE, 154, c(18, 24, 30, 11, 43, 28, 39, 32, 19, 35, 14, 24, 28, 17, 3, 1, 0, 0, 0, 2, 0, 1, 0, 18), "on_updrs_3_total")
  
  # row: 22 column: 172
  compare_total_with_NA(dat, 22, 172, c(19, 25, 30, 11, 45, 29, 41, 33, 19, 37, 14, 25, 30, 17, 3, 1, 0, 0, 0, 2, 0, 1, 0, 19), "on_updrs_3_total")
  
  # row: 20 column: 172
  compare_total_with_NA(dat, 20, 172, c(19, 25, 30, 11, 45, 29, 41, 33, 19, 37, 14, 25, 30, 17, 3, 1, 0, 0, 0, 2, 0, 1, 0, 19), "on_updrs_3_total")
  
  # row: 10 column: 172
  compare_total_with_NA(dat, 10, 172, c(19, 25, 30, 11, 45, 29, 41, 33, 19, 36, 14, 25, 30, 17, 3, 1, 0, 0, 0, 2, 0, 1, 0, 19), "on_updrs_3_total")
  
  # row: 0 column: 172
  compare_total_with_NA(dat, FALSE, 172, c(18, 24, 29, 11, 43, 28, 40, 31, 18, 36, 13, 24, 28, 16, 3, 1, 0, 0, 0, 2, 0, 1, 0, 18), "on_updrs_3_total")
  
  # row: 1 column: 196
  compare_total_with_NA(dat, 1, 196, c(19, 25, 30, 11, 45, 29, 41, 33, 19, 37, 14, 25, 30, 17, 3, 1, 0, 0, 0, 2, 0, 1, 0, 19), "on_updrs_3_total")
  
  # row: 20 column: 196
  compare_total_with_NA(dat, 20, 196, c(19, 25, 30, 11, 45, 29, 41, 33, 19, 37, 14, 25, 30, 17, 3, 1, 0, 0, 0, 2, 0, 1, 0, 19), "on_updrs_3_total")
  
  # row: 10 column: 196
  compare_total_with_NA(dat, 10, 196, c(19, 25, 30, 11, 45, 29, 41, 33, 19, 37, 14, 25, 30, 17, 3, 1, 0, 0, 0, 2, 0, 1, 0, 19), "on_updrs_3_total")
  
  # row: 0 column: 196
  compare_total_with_NA(dat, FALSE, 196, c(19, 25, 30, 11, 45, 29, 41, 33, 19, 37, 14, 25, 30, 17, 3, 1, 0, 0, 0, 2, 0, 1, 0, 19), "on_updrs_3_total")

  # print if all tests passed 
  print("All tests were successful.")
}

#### Function for "on" UPDRS Part IV
scoreUPDRS4Test <- function(dat) {
  ###########################
  ###### TEST MISSING #######
  ###########################
  #### TEST UPDRS PART IV ###
  
  # row: 1, column: 192
  compare_total_with_NA(dat, 1, 192, c(0, 5, 6, 8, 1, 5, 2, 0, 7, 0, 6, 0, 1, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4), "on_updrs_4_total")
  
  # row: 20, column: 195
  compare_total_with_NA(dat, 20, 195, c(0, 5, 6, 8, 1, 5, 2, 0, 7, 0, 6, 0, 1, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4), "on_updrs_4_total")
  
  # row: 10, column: 196
  compare_total_with_NA(dat, 10, 196, c(0, 5, 6, 8, 1, 5, 2, 0, 7, 0, 6, 0, 1, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4), "on_updrs_4_total")
  
  # row: 0, column: 197
  compare_total_with_NA(dat, 0, 197, c(0, 5, 6, 7, 0, 5, 2, 0, 4, 0, 4, 0, 1, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4), "on_updrs_4_total")

  # print if all tests passed 
  print("All tests were successful.")
}

# test on_updrs_3 items
scoreUPDRS3Test(testdf)

# testing on_updrs_4 itms
scoreUPDRS4Test(testdf)
