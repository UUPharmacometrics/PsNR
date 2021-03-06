#' Open resmod_results.csv file and save output in the dataframe readable for R
#'
#' @param directory The qa run directory path. Will search for the resmod_results.csv file in the resmod_'idv' folder.
#' @param idv The string of the idv name.
#' @param quiet A logical indicating whether function should not write the warning message
#' if some file not found. By default quiet=FALSE.
#'
#' @return The list of two elements:
#' resmod_file_exists - logical argument indicating whether resmod_results.csv file exists in the expected directory
#' resmod_table - a resmod_results.csv data frame
#' @export
get_resmod_table <- function(directory, idv,quiet=F){
  resmod_file_exists <- file.exists(file.path(directory, paste0("resmod_", idv), "resmod_results.csv"))
  if(resmod_file_exists) {
    path <- file.path(directory, paste0("resmod_", idv), "resmod_results.csv")
    con <- file(path)
    lines <- readLines(con)
    close(con)
    fields <- stringr::str_split(lines, ",")
    #delete empty values ("") in each list, if exists
    for(i in 1:length(fields)) {
      if(any(fields[[i]] == "")) {
        fields[[i]] <- fields[[i]][which(fields[[i]]!="")]
      }
    }
    header <- fields[[1]]
    fields[[1]] <- NULL

    for(i in 1:length(fields)) {
      if(length(fields[[i]])<length(header)) {
        fields[[i]] <- c(fields[[i]],rep("NA",(length(header)-length(fields[[i]]))))
      }
    }

    save_each_list_element_to_one_row <- function(l){
      fields_with_header <- l[seq_along(header)]
      names(fields_with_header) <- tolower(header)
      fields_with_header[[length(header)]] <-  paste0(l[length(header):length(l)], collapse=",")
      fields_with_header
    }
    for(i in 1:length(fields)) {
      resmod_table_i <- save_each_list_element_to_one_row(l=fields[[i]])
      if(i==1) {
        resmod_table <- as.data.frame(matrix(resmod_table_i,1,length(resmod_table_i)),stringsAsFactors = F)
        names(resmod_table) <- names(resmod_table_i)
      } else {
        resmod_table <- dplyr::bind_rows(resmod_table,resmod_table_i)
      }
    }
    resmod_table <- resmod_table %>%
      dplyr::rename(dOFV=dofv)

    new_dofv <- c()
    for(i in 1:nrow(resmod_table)) {
      #if have "NA" in dOFV
      if(resmod_table$dOFV[i]=='NA') {
        new_dofv[i] <- as.numeric(NA)
      } else {
        new_dofv[i] <- abs(as.numeric(resmod_table$dOFV[i]))
      }
      # if have DVID values
      if(resmod_table$dvid[i]!="sum" && resmod_table$dvid[i]!="NA") {
        resmod_table$dvid[i] <- as.character(as.numeric(resmod_table$dvid[i]))
      }
    }
    resmod_table$dOFV <- new_dofv
    return(list(resmod_file_exists=resmod_file_exists,
                resmod_table=resmod_table))
  } else {
    if(!quiet) {
      message("WARNING: File ",file.path(directory, paste0("resmod_", idv), "resmod_results.csv")," not found!")
    }
    return(list(resmod_file_exists=resmod_file_exists))
  }
}
