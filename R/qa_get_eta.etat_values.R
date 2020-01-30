#' Calculate ETAT values for each eta in the phi file of the parameter model based on the formula:
#' If have boxcox model: (exp(eta)^lambda -1)/lambda
#' If have tdist model: eta(1+((eta^2 + 1)/(4deg_of_freedom))+((5eta^4 + 16eta^2 + 3)/(96deg_of_freedom^2))+((3eta^6 + 19eta^4 + 17eta^2 - 15)/(384deg_of_freedom^3)))
#'
#' @param param_model The parameter model file name. Phi file will be used.
#' @param theta_values A data frame of lambda values for each ETA (if boxcox model) or a data frame of degrees of freedom for each ETA (if tdist model).
#' @param quiet A logical indicating whether function should not write the warning message if some file not found. By default quiet=FALSE.
#'
#' @return A data frame
#' @export
get_eta.etat_values <- function(param_model,theta_values,quiet=F) {
  param_phi_file <- sub("(\\.[^.]+)$",".phi",param_model)
  if(file.exists(param_phi_file)) {
    #which etas to select
    nr <- gsub("\\D","",theta_values$ETA)

    #select only ETA columns
    eta_table <- read.table(param_phi_file,skip=1,header=T,stringsAsFactors = F) %>%
      dplyr::select(unique(grep(paste(paste0("ETA.",nr,"\\."),collapse="|"),colnames(.))))

    for(i in 1:ncol(eta_table)) {
      eta_name <- gsub("[\\(|\\)]","\\.",theta_values$ETA[i])
      if(grepl("boxcox",param_model)) {
        lambda <- as.numeric(theta_values$Lambda[i])
        eta <- eta_table[,eta_name]
        eta_table[,eta_name] <- (exp(eta)^lambda -1)/lambda
      }
      if(grepl("tdist",param_model)) {
        deg_of_freedom <- as.numeric(theta_values$`Degrees of freedom`[i])
        eta <- eta_table[,eta_name]
        eta_table[,eta_name] <- eta*(1+((eta^2 + 1)/(4*deg_of_freedom))+((5*eta^4 + 16*eta^2 + 3)/(96*deg_of_freedom^2))
                                     +((3*eta^6 + 19*eta^4 + 17*eta^2 - 15)/(384*deg_of_freedom^3)))
      }
    }
    eta_table <- eta_table %>% tidyr::gather(ETA_name) %>%
      dplyr::mutate(ETA_name=sub("\\.","\\(",.$ETA_name)) %>%
      dplyr::mutate(ETA_name=sub("[.]$","\\)",.$ETA_name))
  } else {
    if(!quiet) {
      message("WARNING: File ",param_phi_file," not found!")
    }
    eta_table <- data.frame()
  }
  return(eta_table)
}