#' Consequences of including additional subject-level random effects on selected parameters in terms of estimated standard deviation (SD) of new 
#' and existing random effect parameters as well as expected improvement in OFV.
#'   
#' @param original_max0_model The base model file name.
#' @param add_etas_dir A name of the directory where add_etas_linbase.ext (nonlinear=FALSE) or add_etas.ext (nonlinear=TRUE) can be found.
#' @param added_etas A list of additional etas and their numeric position in the model. (added_etas <- list(TPEN=6, SIND=5))
#' @param dofv_add.etas Difference between base model and the model with additional etas OFV values.
#' @param nonlinear A logical indicating whether nonlinear qa have been run.
#' @param quiet A logical indicating whether function should not write the warning message if some file not found. By default quiet=FALSE.
#' 
#' @return A list of 2 arguments:
#' add_etas_table - a data frame
#' add_etas_error - a logical indicating whether both ext files original_max0_model and add_etas_linbase.ext/add_etas.ext exist in the folder
get_add_etas_table <- function(original_max0_model,add_etas_dir,added_etas,dofv_add.etas,nonlinear,quiet=F) {
  original_ext_file <- sub("(\\.[^.]+)$",".ext",original_max0_model)
  if(!nonlinear) {
    add_etas_ext_file <- file.path(add_etas_dir,"add_etas_linbase.ext")
  } else {
    add_etas_ext_file <- file.path(add_etas_dir,"add_etas.ext")
  }
  if(length(added_etas)>0 && file.exists(add_etas_ext_file) && file.exists(original_ext_file)) {
    #old SD
    old_omega_values <- get_omega_values(ext_file=original_ext_file,omegas="var")
    #get nr for etas
    eta_nr <- strsplit(colnames(old_omega_values),"[.]")
    #new SD
    new_omega_values <- get_omega_values(ext_file=add_etas_ext_file,omegas="var")
    
    #create a table
    add_etas_table <- as.data.frame(array(0,c(length(eta_nr),4)))
    colnames(add_etas_table) <- c("","Added","New SD","Old SD")
    for(i in 1:length(eta_nr)) {
      add_etas_table[i,1] <- paste0("ETA(",eta_nr[[i]][2],")")
      add_etas_table[i,2] <- "No"
      add_etas_table[i,3] <- sqrt(new_omega_values[,grep(paste0("^OMEGA\\.",eta_nr[[i]][2],"\\.",eta_nr[[i]][2],"\\.$"),colnames(new_omega_values))])
      add_etas_table[i,4] <- sqrt(old_omega_values[,grep(paste0("^OMEGA\\.",eta_nr[[i]][2],"\\.",eta_nr[[i]][2],"\\.$"),colnames(old_omega_values))])
    }
    add_etas_table[,4] <- format(round(add_etas_table[,4],2),digits=1,trim=T,scientific = F,nsmall=2)
    # order added_eta values
    added_etas[sapply(added_etas,is.null)] <- NA
    added_etas <- added_etas[order(unlist(added_etas))]
    
    #which omegas goes to added etas on parameters
    j <- nrow(add_etas_table)
    n <- 1
    f <- T
    for (i in unlist(added_etas)) {
      j <- j + 1
      if(!is.na(i)) {
        add_etas_table[j,1] <- names(added_etas)[[n]]
        add_etas_table[j,2] <- "Yes"
        add_etas_table[j,3] <- sqrt(new_omega_values[,grep(paste0("^OMEGA\\.",i,"\\.",i,"\\."),colnames(new_omega_values))])
        add_etas_table[j,4] <- ""
      } else {
        if(f == T) {
          add_etas_table[,3] <- format(round(add_etas_table[,3],2),digits=1,trim=T,scientific = F,nsmall=2)
          f <- F
        }
        add_etas_table[j,1] <- names(added_etas)[[n]]
        add_etas_table[j,2] <- "Not found"
        add_etas_table[j,3] <- ""
        add_etas_table[j,4] <- ""
      }
      n <- n + 1
    }
    if(all(!is.na(added_etas))) {
      add_etas_table[,3] <- format(round(add_etas_table[,3],2),digits=1,trim=T,scientific = F,nsmall=2)
    }

    if(class(dofv_add.etas)!="character") {
      add_etas_table <- rbind(add_etas_table,c("dOFV",format(round(dofv_add.etas,2),digits=1,scientific=F,nsmall=1),"",""))
    }
    add_etas_error <- FALSE
  } else {
    if(!file.exists(add_etas_ext_file) && !quiet) {
      message("WARNING: File ",add_etas_ext_file," not found!")
    }
    if(!file.exists(original_ext_file) && !quiet) {
      message("WARNING: File ",original_ext_file," not found!")
    }
    if(length(added_etas)==0 && !quiet) {
      message("WARNING: Added_etas vector is empty!")
    }
    add_etas_error <- TRUE
    add_etas_table <- error_table(col=1)
  }
  return(list(add_etas_table=add_etas_table,
              add_etas_error=add_etas_error))
}