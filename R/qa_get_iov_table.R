#' Consequences of including inter-occasion variability in terms of estimated standard deviation (SD) for inter-individual (IIV) 
#' and inter-occasion variability (IOV) as well as expected improvement in OFV.
#'   
#' @param original_max0_model The base model file name.
#' @param iov_model The iov model file name.
#' @param iov_etas A list of occasion vectors of eta numbers. (iov_etas <- list(occ1=c(1, 2, 5, 7), occ2=c(3, 4, 6, 8)))
#' @param dofv_iov Difference between base model and the model with added iov OFV values.
#' @param quiet A logical indicating whether function should not write the warning message if some file not found. By default quiet=FALSE.
#' 
#' @return A list of 2 elements:
#' iov_table - a data frame
#' iov_error - a logical indicating whether both ext files from models original_max0_model and iov_model exist in the folder
get_iov_table <- function(original_max0_model,iov_model,iov_etas,dofv_iov,quiet=F) {
  original_ext_file <- sub("(\\.[^.]+)$",".ext",original_max0_model)
  iov_ext_file <- sub("(\\.[^.]+)$",".ext",iov_model)
  # calculate how many omegas goes to each block (which goes to iiv and which to iov)
  if(length(iov_etas)>0 && file.exists(iov_ext_file) && file.exists(original_ext_file)) { # we need this table only if we have iov
    
    omegas <- colnames(get_omega_values(ext_file=original_ext_file,omegas="var"))
    iiv_etas_nr <- as.data.frame(strsplit(omegas,split="\\."),stringsAsFactors = F) %>%
      dplyr::slice(2) %>%
      as.numeric()
    new_omega_values <- get_omega_values(ext_file=iov_ext_file,omegas="var")
    #which omega columns to iiv
    iiv_omegas <- new_omega_values %>%
      dplyr::select((grep(paste(paste0("^OMEGA\\.",iiv_etas_nr,"\\.",iiv_etas_nr,"\\.$"),collapse = "|"),colnames(.)))) %>%
      as.numeric() %>%
      sqrt()
    iov_omegas <- new_omega_values %>%
      dplyr::select(grep(paste(paste0("^OMEGA\\.",iov_etas[[1]],"\\.",iov_etas[[1]],"\\.$"),collapse = "|"),colnames(.))) %>%
      as.numeric() %>%
      sqrt()
    
    old_omega_values <- get_omega_values(ext_file=original_ext_file,omegas="var") %>%
      as.numeric() %>%
      sqrt()
    # make table
    iov_table <- data.frame(etas=paste0("ETA(",c(iiv_etas_nr),")"),stringsAsFactors = F) %>%
      dplyr::mutate(IIV=iiv_omegas,IOV=iov_omegas,Old=old_omega_values)
    
    for(i in 2:ncol(iov_table)) {
      iov_table[,i] <- format(round(iov_table[,i],2),digits=1,trim=T,scientific = F,nsmall=2)
    }
    colnames(iov_table) <- c("Linked to","IIV","IOV","Old SD")
    if(class(dofv_iov)!="character") {
      iov_table <- rbind(iov_table,c("dOFV",format(round(dofv_iov,2),digits=1,scientific=F,nsmall=1),"",""))
    }
    iov_error <- FALSE
    
  } else {
    if(!file.exists(iov_ext_file) && !quiet) {
      message("WARNING: File ",iov_ext_file," not found!")
    }
    if(!file.exists(original_ext_file) && !quiet) {
      message("WARNING: File ",original_ext_file," not found!")
    }
    if(length(iov_etas)==0 && !quiet) {
      message("WARNING: iov_etas vector is empty!")
    }
    iov_error <- TRUE
    iov_table <- error_table(col=1)
  }
  return(list(iov_table=iov_table,
              iov_error=iov_error))
}