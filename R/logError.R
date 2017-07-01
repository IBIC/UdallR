
#' @export

logError <- function(dat, column, problem, save.file, id.column="idnum")
{
  # Conjuction with !is.na leaves out NAs
  bad.ids <- as.character(dat[isTRUE(column), id.column])

  if (length(bad.ids) > 0 & !is.na(save.file))
  {
    out <- as.data.frame(cbind(bad.ids, problem))

    write.table(out, file = save.file, append = TRUE, sep = "\t",
                quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
}
