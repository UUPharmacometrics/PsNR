#' Creates a data frame with ERROR records
#'     
#' @param first_column A string of the first column name in the output data frame. By default is empty string.
#' @param col Number of needed columns of the output data frame. By default is set to 2.
#' @param column_names A string vector of the column names. If missing and number of columns are less than 2 then will be set to NULL, 
#' otherwise second column will be set to "dOFV" and all other columns will be set to empty strings.
#' 
#' @return A data frame with the ERROR records.
#' @export
error_table <- function(first_column="",col=2,column_names) {
  if(missing(column_names)) {
    if(col>=2) column_names <- c("","dOFV",rep("",(col-2)))
    if(col<2) column_names <- NULL
  }
  nr_rows <- length(first_column)
  if(col > 2) {
    error.table <- data.frame(matrix(c("ERROR",rep("",col-2)),1,(col-1)),stringsAsFactors = F)
    error.table <- cbind(first_column,error.table)
    colnames(error.table) <- column_names
  } else if(col == 2){
    error.table <- data.frame(first_column,"ERROR",stringsAsFactors = F)
    colnames(error.table) <- column_names
  } else {
    if(first_column=="") {
      first_column <- "ERROR"
    }
    error.table <- data.frame(first_column,stringsAsFactors = F)
    colnames(error.table) <- column_names
  }
  return(error.table)
}