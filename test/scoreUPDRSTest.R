#################################################
##### Test script for scoreUDPRSTest function 
#### ~ compares computed total score on the UPDRS Part III with expcted when original values are replaed
#### by missing values 

# test data frame from data of original data frame 
testdf <- data.frame(on_updrs_3a = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1), 
                     on_updrs_3b = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, 7, 7, NA, NA, 7, NA, NA, 1), 
                     on_updrs_3c = c(1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 7, 7, 0, 0, 7, 0, 0, 1), 
                     on_updrs_3_1 = c(1, 0, 0, 2, 2, 1, 2, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 
                     on_updrs_3_2 = c(1, 1, 0, 2, 2, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 
                     on_updrs_3_3_neck = c(1, 2, 1, 2, 2, 1, 2, 1, 3, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1), 
                     on_updrs_3_3_rue = c(1, 1, 1, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2), 
                     on_updrs_3_3_lue = c(0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 
                     on_updrs_3_3_rle = c(1, 1, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 
                     on_updrs_3_3_lle = c(2, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                     on_updrs_3_4_r = c(1, 2, 0, 2, 1, 3, 3, 2, 1, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1), 
                     on_updrs_3_4_l = c(1, 3, 1, 2, 1, 3, 3, 1, 1, 2, 3, 0, 0, 0, 0, 0, 0, 1, 0, 0), 
                     on_updrs_3_5_r = c(1, 1, 0, 2, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 
                     on_updrs_3_5_l = c(1, 2, 1, 2, 0, 1, 2, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                     on_updrs_3_6_r = c(0, 1, 0, 1, 1, 1, 2, 1, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                     on_updrs_3_6_l = c(0, 2, 1, 2, 1, 2, 2, 1, 3, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                     on_updrs_3_7_r = c(0, 2, 1, 2, 1, 2, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                     on_updrs_3_7_l = c(1, 2, 1, 3, 1, 1, 2, 2, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                     on_updrs_3_8_r = c(0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                     on_updrs_3_8_l = c(1, 1, 1, 2, 1, 1, 1, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                     on_updrs_3_9 = c(0, 0, 0, 1, 3, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2), 
                     on_updrs_3_10 = c(1, 1, 0, 2, 1, 2, 1, 1, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1), 
                     on_updrs_3_11 = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                     on_updrs_3_12 = c(1, 1, 0, 3, 2, 3, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                     on_updrs_3_13 = c(2, 1, 0, 2, 3, 2, 2, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2), 
                     on_updrs_3_14 = c(0, 1, 1, 2, 2, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1), 
                     on_updrs_3_15_r = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                     on_updrs_3_15_l = c(0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                     on_updrs_3_16_r = c(0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1), 
                     on_updrs_3_16_l = c(0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 
                     on_updrs_3_17_rue = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0), 
                     on_updrs_3_17_lue = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                     on_updrs_3_17_rle = c(0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                     on_updrs_3_17_lle = c(0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                     on_updrs_3_17_lipjaw = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                     on_updrs_3_18 = c(0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0), 
                     on_updrs_3_hoehn_yahr = c(2, 2, 2, 3, 2, 3, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 2), 
                     on_updrs_4_1 = c(5, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, NA, 10, 10, 10, 10, 10, NA, 10, 0), 
                     on_updrs_4_2 = c(5, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, NA, 10, 10, 10, 10, 10, NA, 10, 4), 
                     on_updrs_4_3 = c(5, 1, 1, 0, 1, 0, 0, 1, 0, 0, 2, NA, 10, 10, 10, 10, 10, NA, 10, 0), 
                     on_updrs_4_4 = c(5, 1, 1, 0, 0, 0, 0, 1, 0, 0, 2, NA, 10, 10, 10, 10, 10, NA, 10, NA), 
                     on_updrs_4_5 = c(5, 4, 3, 0, 1, 0, 0, 1, 0, 0, 2, NA, 10, 10, 10, 10, 10, NA, 10, 0), 
                     on_updrs_4_6 = c(5, 0, 1, 1, 0, 0, 0, 2, 0, 0, 1, NA, 10, 10, 10, 10, 10, NA, 10, 0))

# Compares UPDRS score when desired (..., row, col, ...) dimensions are NA with expected vector.
# 'row' is false when wanting to compare total with entire column as NA. 
compare_total_with_NA <- function(df, row, col, exp, total_col) {
  if (row == FALSE) { # if row is specified as FALSE, replace entire cclumn with NA
    df[, col] <- NA
  } else { # otherwise, replace element at specified (..., row, col, ...) dimension
    df[row, col] <- NA
  }
  obs <- scoreUPDRS(df)[, total_col] # total score if specified values are NA
  stopifnot(identical(obs, exp)) # compare observed total score with expected
}

# Functions testing scoreUPDRS(): An error will be produced if tests do not pass. 

## Test function for "on" UPDRS Part III
scoreUPDRS3Test <- function(dat) {
  ######################
  #### Test Missing ####
  ######################
  ### UPDRS PART III ###
  # test row numbers 
  middle_row = 10
  last_row = 20
  
  # test column numbers
  middle_col = 22
  last_col = 37
  
  # no extra NA values
  #stopifnot(identical(scoreUPDRS(dat)$on_updrs_3_total, c(22, 32, 14, 48, 44, 36, 40, 17, 28, 33, 20, 3, 15, 14, 0, 2, 14, 1, 0, 22)))
  
  # 1st column, 1st row
  compare_total_with_NA(dat, 1, 1, c(19, 30, 11, 45, 41, 33, 37, 14, 25, 30, 17, 3, 1, 0, 0, 2, 0, 1, 0, 19), "on_updrs_3_total")

  # 1st column, last row 
  compare_total_with_NA(dat, last_row, 1, c(19, 30, 11, 45, 41, 33, 37, 14, 25, 30, 17, 3, 1, 0, 0, 2, 0, 1, 0, 19), "on_updrs_3_total")

  # 1st column, middle row
  compare_total_with_NA(dat, middle_row, 1,  c(19, 30, 11, 45, 41, 33, 37, 14, 25, 30, 17, 3, 1, 0, 0, 2, 0, 1, 0, 19), "on_updrs_3_total")
  
  # entire 1st column
  compare_total_with_NA(dat, FALSE, 1, c(19, 30, 11, 45, 41, 33, 37, 14, 25, 30, 17, 3, 1, 0, 0, 2, 0, 1, 0, 19), "on_updrs_3_total")
  
  # middle column, 1st row
  compare_total_with_NA(dat, 1, middle_col, c(18, 30, 11, 45, 41, 33, 37, 14, 25, 30, 17, 3, 1, 0, 0, 2, 0, 1, 0, 19), "on_updrs_3_total")
  
  # middle column, last row 
  compare_total_with_NA(dat, last_row, middle_col, c(19, 30, 11, 45, 41, 33, 37, 14, 25, 30, 17, 3, 1, 0, 0, 2, 0, 1, 0, 18), "on_updrs_3_total")
  
  # middle column, middle row
  compare_total_with_NA(dat, middle_row, middle_col, c(19, 30, 11, 45, 41, 33, 37, 14, 25, 28, 17, 3, 1, 0, 0, 2, 0, 1, 0, 19), "on_updrs_3_total")
  
  # entire middle column
  compare_total_with_NA(dat, FALSE, middle_col, c(18, 29, 11, 43, 40, 31, 36, 13, 24, 28, 16, 3, 1, 0, 0, 2, 0, 1, 0, 18), "on_updrs_3_total")
  
  # last column, 1st row
  compare_total_with_NA(dat, 1, last_col, c(17, 30, 11, 45, 41, 33, 37, 14, 25, 30, 17, 3, 1, 0, 0, 2, 0, 1, 0, 19), "on_updrs_3_total")
  
  # 1ast column, last row
  compare_total_with_NA(dat, last_row, last_col, c(19, 30, 11, 45, 41, 33, 37, 14, 25, 30, 17, 3, 1, 0, 0, 2, 0, 1, 0, 17), "on_updrs_3_total")
  
  # last column, middle row
  compare_total_with_NA(dat, middle_row, last_col, c(19, 30, 11, 45, 41, 33, 37, 14, 25, 28, 17, 3, 1, 0, 0, 2, 0, 1, 0, 19), "on_updrs_3_total")

  # entire last column
  compare_total_with_NA(dat, FALSE, last_col, c(17, 28, 9, 42, 39, 30, 35, 12, 23, 28, 15, 3, 1, 0, 0, 2, 0, 1, 0, 17), "on_updrs_3_total")
  
  # print if all tests passed 
  print("All tests were successful.")
}

#### Function for "on" UPDRS Part IV
scoreUPDRS4Test <- function(dat) {
  ###########################
  ###### TEST MISSING #######
  ###########################
  #### TEST UPDRS PART IV ###
  
  # test row numbers
  middle_row = 10
  last_row = 20
  
  # test colum numbers
  last_col = 43
  
  # no NA
  stopifnot(identical(scoreUPDRS(dat)$on_updrs_4_total, c(30, 6, 8, 1, 2, 0, 0, 6, 0, 1, 7, 0, 60, 60, 60, 60, 60, 0, 60, 4)))
  
  # first row, last column
  compare_total_with_NA(dat, 1, last_col, c(25, 6, 8, 1, 2, 0, 0, 6, 0, 1, 7, 0, 60, 60, 60, 60, 60, 0, 60, 4), "on_updrs_4_total")
  
  # middle row, last column 
  compare_total_with_NA(dat, middle_row, last_col, c(30, 6, 8, 1, 2, 0, 0, 6, 0, 1, 7, 0, 60, 60, 60, 60, 60, 0, 60, 4), "on_updrs_4_total")
  
  # last row, last column 
  compare_total_with_NA(dat, last_row, last_col, c(30, 6, 8, 1, 2, 0, 0, 6, 0, 1, 7, 0, 60, 60, 60, 60, 60, 0, 60, 4), "on_updrs_4_total")
  
  # entire last row
  compare_total_with_NA(dat, FALSE, last_col, c(25, 6, 7, 0, 2, 0, 0, 4, 0, 1, 6, 0, 50, 50, 50, 50, 50, 0, 50, 4), "on_updrs_4_total")
  
  # print if all tests passed 
  print("All tests were successful.")
} 

# test on_updrs_3 items
scoreUPDRS3Test(testdf)

# testing on_updrs_4 itms
scoreUPDRS4Test(testdf)
