#' Render table
#'
#' @param qa_results QA result object as returned by \code{\link{retrieve_qa_results}}
#' @param settings Settings object 
#'
#' @return Knitted table
#' @export
render_ofv_table <- function(qa_results, settings = qa_settings()){
  
  lbls <- c(
    nonlin = "Nonlinear base model",
    lin_init = "Linearized base model before estimation",
    lin_final = "Linearized base model after estimation",
    lin_final_iofv = "Sum of individual OFV values"
  )
  ofv_table <- qa_results$linearize$ofv_comparison %>% 
    dplyr::transmute(
      label = lbls[.data$name],
      OFV = dplyr::if_else(!is.na(.data$ofv), format(.data$ofv), "ERROR")
    )
  
  qa_kable(ofv_table, col.names = c("", "OFV"), booktabs=TRUE, longtable=TRUE, align=c("l","l"), linesep="") %>%
    kableExtra::kable_styling(position="c",full_width = FALSE)
}

qa_kable <- function(table,...) {
  table_new <- knitr::kable(table,...)
  if(knitr::is_latex_output()) {
    if(!(ncol(table)>1 && all(is.null(colnames(table))))) {
      table_new <- kableExtra::row_spec(table_new, 0,bold=T)
    }
  }
  if(knitr::is_html_output()) {
    if(ncol(table)==1 && is.null(colnames(table))) {
      table_new <- table_new %>% kableExtra::row_spec(1,bold=T)
    }
  }
  return(table_new)  
}