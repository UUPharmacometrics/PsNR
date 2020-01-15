#' Captions for the structural part plots and tables based on the idv name.
#'     
#' @param idv_all A string vector of all possible idv names for specific run.
#' @param idv_name The string of the idv name from the -idv option in the qa run.
#' @param which_idv The string of the idv name which captions are needed.
#' @param perc A logical if values in the structural tables and plots are presented in percentages.
#' 
#' @return A list of the captions for structural plots and tables of specific idv.
#' @export
captions_structural <- function(idv_all,idv_name,which_idv,perc) {
  #captions
  #order TIME,TAD,PRED
  order <- c(idv_name,"TAD","PRED")
  vpc_captions_all <- c(paste0("VPC of observations (DV) vs. binned time (",idv_name,") before correcting for the estimated structural bias by ",idv_name," bin."),
                        "VPC of observations (DV) vs. binned time after dose (TAD) before correcting for the estimated structural bias by TAD bin.",
                        "VPC of observations (DV) vs. binned population predictions (PRED) before correcting for the estimated structural bias by PRED bin.")
  
  resmod_dofv_table_captions_all <- c(paste0("Expected improvement in OFV from addressing structural biases determined as a function of ",idv_name, "."),
                                      "Expected improvement in OFV from addressing structural biases determined as a function of TAD",
                                      "Expected improvement in OFV from addressing structural biases determined as a function of PRED.")

  if(perc) {
    structural_bias_tables_captions_all <- c("Estimated structural bias in CWRES by time bin and corresponding bias on the percent conditional model prediction (\\%CPRED) scale.",
                                             "Estimated structural bias in CWRES by TAD bin and corresponding bias on the percent conditional model prediction (\\%CPRED) scale.",
                                             "Estimated structural bias in CWRES by PRED bin and corresponding bias on the percent conditional model prediction (\\%CPRED) scale.")

    structural_bias_plots_captions_all <- c(paste0("Estimated structural bias on the percent CPRED scale vs. binned time (",idv_name,")."),
                                            "Estimated structural bias on the percent CPRED scale vs. binned TAD.",
                                            "Estimated structural bias on the percent CPRED scale vs. binned PRED.")
  } else {
    structural_bias_tables_captions_all <- c("Estimated structural bias in CWRES by time bin and corresponding bias on the conditional model prediction (CPRED) scale.",
                                             "Estimated structural bias in CWRES by TAD bin and corresponding bias on the conditional model prediction (CPRED) scale.",
                                             "Estimated structural bias in CWRES by PRED bin and corresponding bias on the conditional model prediction (CPRED) scale.")

    structural_bias_plots_captions_all <- c(paste0("Estimated structural bias on the CPRED scale vs. binned time (",idv_name,")."),
                                            "Estimated structural bias on the CPRED scale vs. binned TAD.",
                                            "Estimated structural bias on the CPRED scale vs. binned PRED.")
  }
  
  idv <- c()
  vpc_captions <- c()
  resmod_dofv_table_captions <- c()
  structural_bias_tables_captions <- c()
  structural_bias_plots_captions <- c()
  j <- 1
  for (i in 1:length(order)) {
    if(any(idv_all==order[i])) {
      idv[j] <- order[i]
      vpc_captions[j] <- vpc_captions_all[i]
      resmod_dofv_table_captions[j] <- resmod_dofv_table_captions_all[i]
      structural_bias_tables_captions[j] <- structural_bias_tables_captions_all[i]
      structural_bias_plots_captions[j] <- structural_bias_plots_captions_all[i]
      j <- j + 1
    }
  }
  
  #which of idv captions to print out
  nr_of_idv <- which(idv == which_idv)
  idv_resmod_dofv_table_captions <- resmod_dofv_table_captions[nr_of_idv]
  idv_structural_bias_tables_captions <- structural_bias_tables_captions[nr_of_idv]
  idv_structural_bias_plots_captions <- structural_bias_plots_captions[nr_of_idv]
  idv_vpc_captions <- vpc_captions[nr_of_idv]
  
  out <- list(idv_resmod_dofv_table_captions=idv_resmod_dofv_table_captions,
              idv_structural_bias_tables_captions=idv_structural_bias_tables_captions,
              idv_structural_bias_plots_captions=idv_structural_bias_plots_captions,
              idv_vpc_captions=idv_vpc_captions)
  return(out)
}