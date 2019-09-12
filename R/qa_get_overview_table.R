#' Get overview table.
#'
#' @param structural_overview a data frame of structural models, corresponding dofv values and number of added parameters
#' @param param_var_overview a data frame of patameter models, corresponding dofv values and number of added parameters
#' @param covariates_overview a data frame of covariate models, corresponding dofv values and number of added parameters
#' @param resmod_ruv_overview a data frame of residual error models, corresponding dofv values and number of added parameters
#' @param infl_indiv_overview a data frame of most influential individual, corresponding dofv value and number of added parameters
#' @param outliers_overview a data frame of most outlying individual, corresponding dofv value and number of added parameters
#' 
#' @return A list of 4 arguments.
#' overview_table - a data frame
#' n.rgroup - a numeric vector with number of rows in each group
#' rgroup_names - a character vector of group names
#' row_groups - a data frame with group names as well as a cutoff for the first and last row number in the overview_table for each group
#' @export
get_overview_table <- function(structural_overview,param_var_overview,covariates_overview,resmod_ruv_overview,infl_indiv_overview,outliers_overview) {
  overview_list <- list(structural_overview,param_var_overview,covariates_overview,resmod_ruv_overview,infl_indiv_overview,outliers_overview)
  rgroup_names <- c("Structural Model","Parameter Variability Model","Covariates","Residual Error Model","Influential Individuals","Outliers")
  n.rgroup <- c()
  row_groups <- as.data.frame(array(0,c(length(rgroup_names),3)),stringsAsFactors = F)
  j <- 1
  for(i in 1:length(overview_list)) {
    if(ncol(overview_list[[i]])!=3) {
      overview_list[[i]] <- cbind(overview_list[[i]],rep("",nrow(overview_list[[i]])),stringsAsFactors=F)
      colnames(overview_list[[i]]) <- c("","dOFV","Add.params")
    }
    row_groups[i,1] <- rgroup_names[i]
    row_groups[i,2] <- j
    n.rgroup[i] <- nrow(overview_list[[i]])
    j <- j + n.rgroup[i]
    row_groups[i,3] <- j - 1
    
    if(i == 1) {
      overview_table <- overview_list[[i]]
    } else {
      overview_table <- rbind(overview_table,overview_list[[i]])
    }
  }
  rownames(overview_table) <- NULL
  colnames(overview_table) <- c("","dOFV","Additional parameters")
  #get only numeric values
  j <- 0
  dofv_values <- c()
  for(i in 1:nrow(overview_table)){
    if(all(c("ERROR","NA","","SKIPPED")!=overview_table$dOFV[i])) {
      j <- j + 1
      dofv_values[j] <- as.numeric(overview_table$dOFV[i])
    }
  }
  if(length(dofv_values)>0) {
    dofv_values <- format(round(dofv_values,2),trim=T,digits=1,nsmall=1,scientific = F)
  }
  # format all dofv values in overview table
  j <- 0
  for(i in 1:nrow(overview_table)){
    if(all(c("ERROR","NA","","SKIPPED")!=overview_table$dOFV[i])) {
      j <- j + 1
      overview_table$dOFV[i] <- dofv_values[j]
    }
  }
  
  return(list(overview_table=overview_table,
              n.rgroup=n.rgroup,
              rgroup_names=rgroup_names,
              row_groups=row_groups))
}