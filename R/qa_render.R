#' Render table
#'
#' @param qa_results QA result object as returned by \code{\link{retrieve_qa_results}}
#' @param settings Settings object 
#'
#' @return Knitted table
#' @name render_qa_table
NULL

#' @export
#' @describeIn render_qa_table Renders the OFV overview table in the beginning of the report 
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


#' @export
#' @describeIn render_qa_table Renders the covariates table
render_covariates_table <- function(qa_results, settings = qa_settings()){
  scm_table <- qa_results$scm %>% 
    dplyr::select("parameter", "covariate", "bin_split", "dofv", "prm_value") %>% 
    dplyr::mutate(dofv = round(.data$dofv, 1),
                  prm_value = round(.data$prm_value, 3),
                  bin_split = dplyr::if_else(is.na(bin_split), "", as.character(bin_split))) %>% 
    dplyr::arrange(.data$parameter, .data$covariate, .data$bin_split)
  
  qa_kable(scm_table, col.names = c("Parameter", "Covariate", "", "dOFV", "Coefficient"), align = "lllrr", 
           booktabs=T,longtable=T,linesep="") %>% 
    kableExtra::kable_styling(position="c",full_width = F)
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