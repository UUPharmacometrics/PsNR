#' Create covariate table for the summary part and the table of model parameter covariate relationship, resulting improvement in OFV and estimated covariate coefficient. 
#' Furthermore, the sum of all univariate improvements in OFV ignoring between covariate correlations (sum(SCMu)), as well as joint improvement in OFV 
#' when taking correlations into account (FREM).
#'
#' @param frem_table A data frame of one row with column names c("","dOFV","Add.params") or c("","dOFV") if frem model run crash or was not run.
#' @param scm_table A data frame of model parameter covariate relationship, resulting improvement in OFV and estimated covariate coefficient. 
#' Column names c("","dOFV","Coef").
#' @param max_scm_table A data frame of one row with column names c("","dOFV","Add.params") or c("","dOFV") if scm model run crash or was not run.
#' 
#' @return A list of 2 elements:
#' covariates_table - a data frames with two rows, one for frem record and one for scm record (for summary section)
#' covariates_extra_table - a data frame of model parameter covariate relationship, resulting improvement in OFV and 
#' estimated covariate coefficient, with two extra rows - sum of all scm dofv values and frem dofv value (for covariates section)
get_covariates_table <- function(frem_table,scm_table,max_scm_table) {
  covariates_table <- rbind(c("FREM",frem_table[,2:3]),max_scm_table,stringsAsFactors=F)
  if(any(c("ERROR","NA","SKIPPED")==scm_table$dOFV[1]) && length(scm_table$dOFV)==1) {
    if(any(c("ERROR","NA","SKIPPED")==frem_table$dOFV)) {
      covariates_extra_table <- rbind(scm_table,c("FREM",frem_table$dOFV),stringsAsFactors=F)
    } else {
      covariates_extra_table <- rbind(scm_table,c("FREM",format(round(as.numeric(frem_table$dOFV),2),digits=1,nsmall = 1)),stringsAsFactors=F)
    }
    colnames(covariates_extra_table) <- c("Covariate","dOFV")
  } else {
    sum_scm_dofv <- sum(as.numeric(scm_table$dOFV[!is.na(scm_table$dOFV)]))
    scm_table$Coef <- format(round(scm_table$Coef,3),trim=T,digits=1,nsmall=2,scientific=F)
    # if(any(colnames(scm_table)=="Coef_sd")) {
    #   scm_table$Coef_sd <- format(round(scm_table$Coef_sd,3),trim=T,digits=1,nsmall=2,scientific=F)
    # }
    covariates_extra_table <- rbind(scm_table,c("sum(SCMu)",sum_scm_dofv,rep("",ncol(scm_table)-2)),
                                    stringsAsFactors=F)
    # if(any(colnames(scm_table)=="Coef_sd")) {
    #   colnames(covariates_extra_table) <- c("Covariate","dOFV","Coefficient","Coefficient/SD")
    # } else {
      colnames(covariates_extra_table) <- c("Covariate","dOFV","Coefficient")
    # }

    if(class(frem_table$dOFV)=="character") {
      covariates_extra_table$dOFV <- format(round(as.numeric(covariates_extra_table$dOFV),2),trim=T,digits=1,nsmall=1,scientific=F)
      covariates_extra_table <- rbind(covariates_extra_table,c("FREM",frem_table$dOFV,rep("",ncol(scm_table)-2)),
                                      stringsAsFactors=F)
    } else {
      covariates_extra_table <- rbind(covariates_extra_table,c("FREM",frem_table$dOFV,rep("",ncol(scm_table)-2)),
                                      stringsAsFactors=F)
      covariates_extra_table$dOFV <- format(round(as.numeric(covariates_extra_table$dOFV),2),trim=T,digits=1,nsmall=1,scientific=F)
    }

  }
  return(list(covariates_table=covariates_table,
              covariates_extra_table=covariates_extra_table))
}