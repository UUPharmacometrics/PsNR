#' Get initial estimate values from specifick iteration row.
#' 
#' @param filename Name of the file.
#' @param select Can be set to "omega","theta","sigma" or "all"(means thetas, omegas and sigmas). By default is "all".
#' @param iteration The value of iteration. By default is -1000000000.
#' @param do.stop A logical indicating whether function should stop if argument 'select' is set to something different than allowed string. By default do.stop=TRUE
#' @return The data frame of initial estimate values.
#' @export
get_initial_estimate_values <- function(filename,select="all",iteration=-1000000000,do.stop=TRUE) {
  init.est <- read.table(filename,header=TRUE,skip=1,stringsAsFactors = F) %>%
    dplyr::filter(ITERATION==iteration)
  if(select=="all") {
    init.est <- init.est %>% dplyr::select(grep("(^OMEGA|^THETA|^SIGMA)",colnames(.)))
  } else if(select=="omega") {
    init.est <- init.est %>% dplyr::select(grep("^OMEGA",colnames(.)))
  } else if(select=="theta") {
    init.est <- init.est %>% dplyr::select(grep("^THETA",colnames(.)))
  } else if(select=="sigma") {
    init.est <- init.est %>% dplyr::select(grep("^SIGMA",colnames(.)))
  } else {
    message("Argument 'select' can be set only to strings 'all', 'omega', 'theta' or 'sigma'.")
    if(do.stop) {
      stop()
    }
  }
  if(ncol(init.est)==0) {
    init.est <- data.frame()
  }
  return(init.est)  
}


#' Count number of THETAs.
#' 
#' @param filename Name of the file.
#' @param iteration The value of iteration. By default is -1000000000.
#' @param keep_na Count or not to count NA values. By default is FALSE.
#' @return Number of THETAs.
#' @export
count_thetas <- function(filename,iteration=-1000000000,keep_na=FALSE) {
  ext_file <- read.table(filename,header=TRUE,skip=1,stringsAsFactors = F) %>%
    dplyr::filter(ITERATION==iteration)
  TH_values <- ext_file[grep("^THETA+[0-9]$",colnames(ext_file))]
  nr_of_thetas <- length(TH_values)
  if(!keep_na) {
    nr_of_thetas <- length(TH_values[!is.na(TH_values)])
  }
  return(nr_of_thetas)
}


#' Get ofv value from specific iteration row
#' 
#' @param filename Name of the file.
#' @param iteration The value of iteration. By default is -1000000000.
#' @return The OFV value.
#' @export
get_ext_ofv <- function(filename,iteration=-1000000000){
  read.table(filename,header=TRUE,skip=1,stringsAsFactors = F) %>%
    dplyr::filter(ITERATION==iteration) %>%
    dplyr::select(OBJ) %>% 
    as.numeric()
}


#' Get omega values
#' 
#' @param filename Name of the file.
#' @param omegas Can be set to "var" (only variances will be filtered out),"cov" (only covariances will be filtered out) or "all". By default is "all".
#' @param iteration The value of iteration. By default is -1000000000.
#' @param keep_zeroes A logical indicating whether function should keep omegas with zeroe values. By default keep_zeroes is FALSE.
#' @return The data frame of omega values.
#' @export
get_omega_values <- function(filename, omegas, iteration=-1000000000, keep_zeroes=FALSE){
  omega_table <- get_initial_estimates_from_ext(filename=filename,select="omega",iteration=iteration)
  omegas_v <- c()
  col.names_v <- c()
  omegas_c <- c()
  col.names_c <- c()
  if(ncol(omega_table)>0) {
    if(!keep_zeroes) {
      col_names <- colnames(omega_table)[which(omega_table!=0)]
      omega_table <- as.data.frame(omega_table[,which(omega_table!=0)])
    } else {
      col_names <- colnames(omega_table)
      omega_table <- as.data.frame(omega_table)
    }
    colnames(omega_table) <- col_names
    for(i in 1:length(omega_table)) {
      numeration <- sub('.*OMEGA\\.','',colnames(omega_table[i]))
      numeration <- substr(numeration, 1, nchar(numeration)-1) # delete last element in string
      first <- sub('\\..*','',numeration)
      second <- sub('.*\\.','',numeration)
      if(first==second) { # get omega values from the diagonals
        if(!keep_zeroes) {
          if(omega_table[i]!=0) {
            omegas_v <- c(omegas_v,omega_table[i])
            col.names_v <- c(col.names_v,colnames(omega_table[i]))
          }
        } else {
          omegas_v <- c(omegas_v,omega_table[i])
          col.names_v <- c(col.names_v,colnames(omega_table[i]))
        }
      } 
      if(first!=second) { # get omega values outside of the diagonals
        if(!keep_zeroes) {
          if(omega_table[i]!=0) {
            omegas_c <- c(omegas_c,omega_table[i])
            col.names_c <- c(col.names_c,colnames(omega_table[i]))
          }
        } else {
          omegas_c <- c(omegas_c,omega_table[i])
          col.names_c <- c(col.names_c,colnames(omega_table[i]))
        }
      }
      
    }
  } else {
    omega_table <- data.frame()
  }
  
  if(length(omegas_v)>0) {
    omegas_var <- data.frame(omegas_v,stringsAsFactors = F)
    colnames(omegas_var) <- col.names_v
  } else {
    omegas_var <- data.frame()
  }
  if(length(omegas_c)>0) {
    omegas_cov <- data.frame(omegas_c,stringsAsFactors = F)
    colnames(omegas_cov) <- col.names_c
  } else {
    omegas_cov <- data.frame()
  }
  
  #what to print out
  if(omegas=="all") {
    omega_values <- omega_table
  } else if(omegas=="var"){
    omega_values <- omegas_var
  } else if(omegas=="cov"){
    omega_values <- omegas_cov
  }
  
  return(omega_values)
}