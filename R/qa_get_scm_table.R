#' Table of model parameter-covariate relationship, resulting improvement in OFV and estimated covariate coefficient.
#'
#' @param scm_directory Scm run directory. Will search for the file raw_results_scm.csv. Columns relation, ofv, step.number 
#' and all columns with estimated covariate coefficient values (like, CLWGT.4.1) will be used in the function.
#' @param nr_cov Number of covariates from the scm point of view, because categorical covariates can be binarized.
#' @param nr_param Numbner of parameters.
#' @param skip A character vector with names of the skipped parts in the qa run. Will check if "scm" is one of the vector elements.
#' By default skip=NULL.
#' @param quiet A logical indicating whether function should not write the warning message if some file not found. By default quiet=FALSE.
#' 
#' @return A list of 3 elements:
#' scm_table - a data frame of model parameter covariate relationship, resulting improvement in OFV and estimated covariate coefficient
#' max_scm_table - a data frame with only one row from scm_table with the highest dofv value
#' scm_files_exists - logical argument indicating whether needed file exists in the input directory and whether there is at least one covariate and one parameter.
#' @export
get_scm_table <- function(scm_directory,nr_cov,nr_param,skip=NULL,quiet=F){
  rawres_file <- file.path(scm_directory,"raw_results_scm.csv")
  scm_files_exists <- file.exists(rawres_file)
  if(any(skip=="scm")) {
    scm_table <- data.frame("SCM","SKIPPED",stringsAsFactors = F)
    colnames(scm_table) <- c("","dOFV")
    max_scm_table <- cbind(scm_table,"",stringsAsFactors = F)
  } else {
    if(nr_param!=0 && nr_cov!=0) {
      if(scm_files_exists) {
        scm_table <- read.csv(rawres_file,stringsAsFactors = F) %>%
          dplyr::mutate(dOFV = ofv[step.number==0]-ofv) %>%
          dplyr::slice(-1) %>%
          dplyr::select(relation, dOFV)
        colnames(scm_table) <- c("","dOFV")
        #max_table
        if(all(is.na(scm_table$dOFV))) {
          if(!quiet) {
            message("WARNING: In file ",rawres_file," all dofv values are NA.")
          }
          max_scm_table <- cbind(error_table("SCM"),"",stringsAsFactors = F)
        } else {
          max_scm_table <- cbind(scm_table[which.max(scm_table$dOFV),],1)
        }
        
        # add coefficient
        scm_table_coef <- read.csv(rawres_file,stringsAsFactors = F) %>%
          dplyr::slice(-1)
        column_names <- paste0(scm_table_coef$relation,"-1")
        column_names <- gsub("\\-","\\.",column_names)
        coef_table <- scm_table_coef[, which(colnames(scm_table_coef) %in% column_names), drop=F]
        scm_table <- cbind(scm_table,"Coef"=as.numeric(as.character(rep(NA,nrow(scm_table)))))
        for(i in 1:length(coef_table)) {
          row_nr <- which(paste0(gsub("\\-","\\.",scm_table[,1]),".1") == colnames(coef_table[i]))
          if(!all(is.na(coef_table[i]))) {
            scm_table$Coef[row_nr] <- as.numeric(coef_table[!is.na(coef_table[i]),i])
          }
        }
        colnames(scm_table) <- c("","dOFV","Coef")
      } else {
        if(!quiet) {
          message("WARNING: File ",rawres_file," not found!")
        }
        scm_table <- error_table("SCM")
        max_scm_table <- cbind(scm_table,"",stringsAsFactors = F)
      }
    } else {
      if(!quiet) {
        message("WARNING: Parameter and covariate vectors are empty!")
      }
      scm_files_exists <- FALSE
      scm_table <- data.frame("SCM","NA",stringsAsFactors = F)
      colnames(scm_table) <- c("","dOFV")
      max_scm_table <- cbind(scm_table,"",stringsAsFactors = F)
    }
  }
  
  colnames(max_scm_table) <- c("","dOFV","Add.params")
  return(list(scm_files_exists=scm_files_exists,
              scm_table=scm_table,
              max_scm_table=max_scm_table))
}
