#' Give a warning if the condition is not true, consisting of message
#' @param condition An expression or flag that evaluates to a boolean
#' @param message A message to be printed if condition is FALSE (not true)
warningIfNot<- function (condition, message)
    if(condition== FALSE) {
        warning(message)
    }

