#' Table of difference between model 2 and model 4 OFV values and number of added parameters. In case of non-missing dofv value of the full omega block, 
#' frem dofv value will be calculated as difference between model 2 and model 4 OFV values and dofv value of the full omega block.
#'
#' @param frem_directory Frem run directory. Will search for the file raw_results.csv in the subdirectories model2_modelfit_dir1 and model4_modelfit_dir1.
#' @param nr_cov Number of covariates.
#' @param nr_param Numbner of parameters.
#' @param dofv_full_block The dOFV value of the full omega block.
#' @param skip A character vector with names of the skipped parts in the qa run. Will check if "frem" is one of the vector elements.
#' By default skip=NULL.
#' @param quiet A logical indicating whether function should not write the warning message if some file not found. By default quiet=FALSE.
#' 
#' @return A list of 2 elements:
#' frem_table - a data frame with joint improvement in OFV when taking correlations into account
#' frem_files_exists - logical argument indicating whether needed files exist in the input directory and there is at least one covariate
#' @export
get_all_covariates <- function(frem_directory,nr_cov,nr_param,dofv_full_block,skip=NULL,quiet=F) {
  if(any(skip=="frem")) {
    frem_files_exists <- FALSE
    frem_table <- data.frame("FREM","SKIPPED","",stringsAsFactors = F)
  } else {
    if(nr_cov!=0) {
      frem_files_exists <- (file.exists(file.path(frem_directory,"model2_modelfit_dir1/raw_results.csv")) && 
                              file.exists(file.path(frem_directory,"model4_modelfit_dir1/raw_results.csv")))
      if(frem_files_exists) {
        ofv_frem_all_cov <- .get_rawres_ofv(file.path(frem_directory,"model2_modelfit_dir1/raw_results.csv"))
        ofv_frem_no_cov <- .get_rawres_ofv(file.path(frem_directory,"model4_modelfit_dir1/raw_results.csv"))
        if(all(!is.na(ofv_frem_all_cov) && !is.na(ofv_frem_no_cov))) {
          dofv_frem <- ofv_frem_all_cov - ofv_frem_no_cov
        } else {
          dofv_frem <- "ERROR"
        }
        if(class(dofv_full_block)!="character" && class(dofv_frem)!="character") {
          dofv_frem <- dofv_frem - dofv_full_block
        }
        frem_table <- data.frame("ALL",dofv_frem,(nr_param*nr_cov),stringsAsFactors = F)
      } else {
        if(!file.exists(file.path(frem_directory,"model2_modelfit_dir1/raw_results.csv")) && !quiet) {
          message("WARNING: File ",file.path(frem_directory,"model2_modelfit_dir1/raw_results.csv")," not found!")
        }
        if(!file.exists(file.path(frem_directory,"model4_modelfit_dir1/raw_results.csv")) && !quiet) {
          message("WARNING: File ",file.path(frem_directory,"model4_modelfit_dir1/raw_results.csv")," not found!")
        }
        frem_table <- cbind(error_table("FREM"),"",stringsAsFactors = F)
      }
    } else {
      if(!quiet) {
        message("WARNING: Both continuous and categorical covariate vectors are empty!")
      }
      frem_files_exists <- FALSE
      frem_table <- data.frame("FREM","NA","",stringsAsFactors = F)
    }
  }
  colnames(frem_table) <- c("","dOFV","Add.params")
  return(list(frem_files_exists=frem_files_exists,
              frem_table=frem_table))
}
