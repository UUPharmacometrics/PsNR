#' Create a kable table basen on chosen Rmarkdown format (latex or html). Special case if only one column in the input dataframe.
#'    
#' @param table A data frame.
#' @param format A string of the Rmarkdown format. Possible values "latex" or "html". Not tested with other values like "markdown", "pandoc", and "rst".
#' @param ... All other arguments that can be passed to the kable() function.
#' 
#' @return A kable dataframe.
#' @export
kable_table <- function(table,format,...) {
  # library(kableExtra)
  table_new <- knitr::kable(table, format,...)
  if(format=="latex") {
    if(ncol(table)>1 && all(is.null(colnames(table)))) {
      table_new <- table_new
    } else {
      table_new <- table_new %>% kableExtra::row_spec(0,bold=T)
    }
  }
  if(format=="html") {
    if(ncol(table)==1 && is.null(colnames(table))) {
      table_new <- table_new %>% kableExtra::row_spec(1,bold=T)
    }
  }
  return(table_new)  
}