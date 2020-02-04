#' Get initial estimates from the ext file.
#'
#' @param filename A string of the ext file name.
#' @param select A string of the initial estimates to get. Possible values "omega","theta","sigma" or "all"(means all three).
#' @param iteration A numeric value from the ITERATION column in the ext file. By default iteration=-1000000000.
#' @param do.stop A logical indicating whether function should stop if input of the select argument is wrong. By default do.stop=TRUE.
#'
#' @return A data frame of initial estimates.

#part can be "omega","theta","sigma" or "all"(means thetas, omegas and sigmas)
#' @export
get_initial_estimates_from_ext <- function(filename,select="all",iteration=-1000000000,do.stop=TRUE) {
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