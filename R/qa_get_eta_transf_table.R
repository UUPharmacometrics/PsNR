#' Calculate ETAT values for each eta in normal distribution. Calculations done based on the formula: 
#' If boxcox model: (exp(eta)^lambda -1)/lambda
#' If tdist model: eta(1+((eta^2 + 1)/(4deg_of_freedom))+((5eta^4 + 16eta^2 + 3)/(96deg_of_freedom^2))+((3eta^6 + 19eta^4 + 17eta^2 - 15)/(384deg_of_freedom^3)))
#'
#' @param input_table A data frame of ETA name in the column ETA, lambda/degrees of freedom values in the column Lambda/Degrees of freedom and 
#' standard deviation (from parameter model) of the ETA in the third column.
#' @param seq_length.out Desired length of the sequence for the seq function. So many eta values for each ETA and each ETAT will be in the output dataframe. 
#' By default seq_length.out=1000.
#' 
#' @return A list of one data frame (eta_transf_table) of the transformed ETA values, hight of the figure (fig_height) which is based on how many ETAs are in the table and
#'  a logical identifier if eta transformation plot should be created (make_eta_transf_plot).
get_eta_transf_table <- function(input_table,seq_length.out=1000) {
  fig_height <- 15
  if(any(grepl('ETA',input_table$ETA)) && (any(colnames(input_table)=="Lambda") || any(colnames(input_table)=="Degrees of freedom")))  {
    make_eta_transf_plot <- TRUE
    
    for(i in 1:nrow(input_table)) {
      sd <- as.numeric(input_table[i,3])
      if(any(colnames(input_table)=="Lambda")) {
        lambda <- as.numeric(input_table$Lambda[i])
      } else {
        deg_of_freedom <- as.numeric(input_table$`Degrees of freedom`[i])
      }
      eta <- qnorm(seq(1E-7, 1-1E-7, length.out = seq_length.out), sd=sd)
      density <- dnorm(eta,sd=sd)
      if(any(colnames(input_table)=="Lambda")) {
        ETAT <- (exp(eta)^lambda -1)/lambda
      } else {
        ETAT <- eta*(1+((eta^2 + 1)/(4*deg_of_freedom))+((5*eta^4 + 16*eta^2 + 3)/(96*deg_of_freedom^2))
                     +((3*eta^6 + 19*eta^4 + 17*eta^2 - 15)/(384*deg_of_freedom^3)))
      }
      table_per_eta <- data.frame(ETA_name=input_table$ETA[i],ETA=eta,density=density,ETAT=ETAT,stringsAsFactors = F)
      table_per_eta <- table_per_eta %>% tidyr::gather(type,eta,-ETA_name,-density)
      if(i > 1) {
        eta_transf_table <- rbind(eta_transf_table,table_per_eta)
      } else {
        eta_transf_table <- table_per_eta
      }
    }
    
    #count unique ETAs
    if(length(unique(eta_transf_table$ETA_name)) <= 8) {
      fig_height <- 6.25
    }
    out <- list(eta_transf_table=eta_transf_table,
                make_eta_transf_plot=make_eta_transf_plot,
                fig_height=fig_height)
  } else {
    make_eta_transf_plot <- FALSE
    out <- list(make_eta_transf_plot=make_eta_transf_plot,
                fig_height=fig_height)
  }
  
  return(out)
}