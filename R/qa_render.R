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
  ofv_table <- get_result(qa_results$linearize$ofv_comparison) %>% 
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
  if(!has_errors(qa_results$scm)){
    scm_table <- get_result(qa_results$scm) 
    frem_res <- get_result(qa_results$frem)
    
    output_table <- scm_table %>% 
      dplyr::select("parameter", "covariate", "bin_split", "dofv", "prm_value") %>% 
      dplyr::mutate(dofv = round(.data$dofv, 1),
                  prm_value = round(.data$prm_value, 3),
                  bin_split = dplyr::if_else(is.na(bin_split), "", as.character(bin_split))) %>% 
      dplyr::arrange(.data$parameter, .data$covariate, .data$bin_split) 
    
    
    scm_sum <- scm_table %>% 
      dplyr::summarise(dofv = round(sum(dofv, na.rm = TRUE), 1),
                       covariate = "univ. sum") %>% 
      to_character_tbl() 
    
    if(!has_errors(frem_res)){
      frem_dofv <- tibble::tibble(covariate = "FREM", dofv = round(frem_res$dofv, 1)) %>% 
        to_character_tbl()
    }else{
      frem_dofv <- tibble::tibble(covariate = "FREM", dofv = "ERROR") 
    }
      
    extra_rows <- dplyr::bind_rows(scm_sum, frem_dofv) %>% 
      add_blank_cols(colnames(output_table))

    to_character_tbl(output_table) %>% 
      dplyr::bind_rows(extra_rows) %>% 
      qa_kable(col.names = c("Parameter", "Covariate", "", "dOFV", "Coefficient"), align = "lllrr", 
           booktabs=T,longtable=T,linesep="") %>% 
      kableExtra::collapse_rows(1:2) %>% 
      kableExtra::pack_rows("Overall", nrow(output_table)+1, nrow(output_table) + 1) %>% 
      kableExtra::kable_styling(position="c",full_width = F) 
  }
}

# converts all numeric column to character
to_character_tbl <- function(df){
  dplyr::mutate_if(df, is.numeric, as.character)
}


add_blank_cols <- function(df, cols){
  cols <- setdiff(cols, colnames(df))
  tibble::add_column(df, !!!purrr::set_names(purrr::rep_along(cols, ""), cols))
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