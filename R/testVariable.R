#' Test demographic variable
#'
#' Given a column and data frame, compares values of column for PD and
#' control/unaffected participants.
#'
#' @param col Column name
#' @param all.subjects Data frame containing information for all subjects.
#'
#' @return Vector of length 7 with PD group mean/count; PD group SD/percent;
#' control group mean/count; control group SD/percent; p-value (when
#' appropriate); total mean/count; total SD/percent
#'
#' @export

testVariable <- function(col, all.subjects, grouping, groups){

  if (!(grouping %in% colnames(all.subjects)))
  {
    stop(paste("Column", grouping, "is not present in all.subjects"))
  }

  # Select data subsets based on groups
  subsets <- as.list(rep(NA, length(groups)))
  for (i in 1:length(groups))
  {
    subsets[[i]] <- all.subjects[all.subjects[, grouping]==groups[i], col]
  }
  names(subsets) <- groups

  # Name output for assignment
  return.vec <- rep(NA, length(subsets) * 2 + 3)
  names(return.vec)[seq(1, length(return.vec) - 3, by = 2)] <- paste0(groups,
                                                                      ".m")
  names(return.vec)[seq(2, length(return.vec) - 3, by = 2)] <- paste0(groups,
                                                                      ".sd")
  names(return.vec)[length(return.vec):(length(return.vec) - 2)] <- c("total.sd",
                                                                      "total.m",
                                                                      "p")


  # Only total subjects that belong to an eligible group
  # Assign total information to total.*
  total <- all.subjects[all.subjects[, grouping] %in% groups, c(grouping, col)]

  total.omitted <- na.omit(total)
  omitted.groups <- unique(total.omitted[, grouping])

  if (length(omitted.groups) != length(groups))
  {
    warning("One or more groups was ommitted entirely from ANOVA.")
  }

  if (is.numeric(total[, col]))
  {
    return.vec["total.m"] <- mean(total[, col], na.rm = TRUE)
    return.vec["total.sd"] <- sd(total[, col], na.rm = TRUE)

    if (length(omitted.groups) > 1)
    {
      message(paste("Running ANOVA for group differences:", col))
      anova.results <- aov(total[, col] ~ total[, grouping])
      return.vec["p"] <- summary(anova.results)[[1]][["Pr(>F)"]][[1]]
    }
    else
    {
      warning(paste("Not enough groups to do an ANOVA for", col))
      return.vec["p"] <- NA
    }
  }

  for (i in 1:length(subsets))
  {
    s <- subsets[[i]]
    name <- names(subsets)[i]

    if (is.numeric(s))
    {
      if (all(is.na(s)))
      {
        warning("Column is all NA")
        return.vec[c(paste0(name, ".m"), paste0(name, ".sd"))] <- NA
      }
      else
      {
        m <- mean(s, na.rm = TRUE)
        sd <- sd(s, na.rm = TRUE)

        return.vec[paste0(name, ".m")] <- m
        return.vec[paste0(name, ".sd")] <- sd
      }
    }
    else if (is.factor(s) || is.character(s))
    {
      s <- as.factor(s)
      message(paste(col, "is a factor vector. Returning count for",
                    levels(s)[1]))
      if (length(levels(s)) != 2)
      {
        warnings("Error: More than two factor levels in column.")
      }

      total.c <- sum(total.omitted[, col] == levels(s)[1], na.rm = TRUE)
      return.vec["total.m"] <- total.c
      return.vec["total.sd"] <- total.c / length(total.omitted[, col])

      count <- sum(s == levels(s)[1], na.rm = TRUE)
      proportion <- count / length(s)

      return.vec[paste0(name, ".m")] <- count
      return.vec[paste0(name, ".sd")] <- proportion


      # total.c <- sum(total.col == levels(pd.col)[1], na.rm = TRUE)
      # total.p <- total.c / length(total.col)
      #
      #
      #
      # return.vec <- c(pd.c, pd.p, ctrl.c, ctrl.p, NA, total.c, total.p)
    }
  }

  return(return.vec)
}
