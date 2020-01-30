#' Creates a data frame with OFV values from nonlinear model, linearized model before the estimation,
#' linearized model after the estimation and the sum of the individual OFV values (linearized).
#'
#' @param directory A string of the directory name where derivatives.ext, "model.filename"_linbase.ext
#' and "model.filename"_linbase.phi files can be found.
#' @param model.filename The original model file name.
#' @param quiet A logical indicating whether function should not write the warning message
#' if some file not found. By default quiet=FALSE.
#' @return A data frame with OFV values from nonlinear model, linearized model before the estimation,
#' linearized model after the estimation and the sum of the individual OFV values (linearized).
#' In case of missing ext or phi file instead of OFV value the string "ERROR" will appera in the data frame.
#' @export
ofv_summary_table <- function(directory,model.filename,quiet=F) {
  if(file.exists(file.path(directory,"derivatives.ext"))) {
    ofv_nonlin_base_mod <- .get_ext_ofv(file.path(directory,"derivatives.ext"))
  } else {
    ofv_nonlin_base_mod <- "ERROR"
    if(!quiet) {
      message("WARNING: File ",file.path(directory,"derivatives.ext")," not found!")
    }
  }
  if(file.exists(file.path(directory,paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext")))) {
    ofv_lin_base_mod_before_est <- .get_ext_ofv(file.path(directory,paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext")),0)
  } else {
    ofv_lin_base_mod_before_est <- "ERROR"
    if(!quiet) {
      message("WARNING: File ",file.path(directory,paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext"))," not found!")
    }
  }
  if(file.exists(file.path(directory,paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext")))) {
    ofv_lin_base_mod <- .get_ext_ofv(file.path(directory,paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext")))
  } else {
    ofv_lin_base_mod <- "ERROR"
    if(!quiet) {
      message("WARNING: File ",file.path(directory,paste0(sub('.([^.]*)$','',model.filename),"_linbase.ext"))," not found!")
    }
  }
  if(file.exists(file.path(directory,paste0(sub('.([^.]*)$','',model.filename),"_linbase.phi")))) {
    linbase_phi_table <- read.table(file.path(directory,paste0(sub('.([^.]*)$','',model.filename),"_linbase.phi")),skip=1,header=T,stringsAsFactors = F)
    linbase_phi_ofv <- sum(linbase_phi_table$OBJ)
  } else {
    linbase_phi_ofv <- "ERROR"
    if(!quiet) {
      message("WARNING: File ",file.path(directory,paste0(sub('.([^.]*)$','',model.filename),"_linbase.phi"))," not found!")
    }
  }

  ofv_table <- data.frame(c("Nonlinear base model","Linearized base model before estimation","Linearized base model after estimation","Sum of individual OFV values"),
                          as.character(c(ofv_nonlin_base_mod,ofv_lin_base_mod_before_est,ofv_lin_base_mod,linbase_phi_ofv)),
                          stringsAsFactors = F)
  colnames(ofv_table) <- c('','OFV')
  return(ofv_table)
}