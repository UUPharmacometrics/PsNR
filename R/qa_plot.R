
#' Plot result
#'
#' Print result if the provided result does not contain an error.
#'
#' @param r A result data structure
#'
#' @export
plot_result <- function(r){
  if(has_errors(r)) {
    e <- get_error(r)
    sink(stderr())
    print(e)
    sink()
    return(invisible(e))
  }
  else {
    p <- get_result(r)
    if(!rlang::is_bare_list(p)){
      print(p)
    }else{
      purrr::walk(p, print)
    }
    return(invisible(p))
  }
}

#' Prepare VA plot of FREM results
#'
#' @param qa_results QA result data structure
#'
#' @return A result data structure
#' @export
prepare_va_cov_plot <- function(qa_results){
  frem_path <- qa_results$files$frem$path
  derivatives_lst <- qa_results$files$linearize$derivatives_lst
  dvid <- NULL
  p <- tryCatch({
    if(qa_results$options$dvid_name!='') dvid <- qa_results$options$dvid_name
    input_base <- vaplot::prepare_va_nm(derivatives_lst)
    res_base <- vaplot::compute_va(input_base, facets = dvid)
    input_frem <- vaplot::prepare_va_frem(frem_path)
    res_frem <- vaplot::compute_va(input_frem, facets = dvid)
    p <- vaplot::plot_va_compare(`without covariates` = res_base,
                                 `with covariates` = res_frem,
                                 smooth = TRUE)+
      ggplot2::theme_bw()+
      ggplot2::theme(legend.position = "bottom")
    #if(!is.null(dvid)) p <- p + ggforce::facet_grid_paginate()

    if("TAD" %in% input_base$column_names && "TAD" %in% input_frem$column_names){
      res_base <- vaplot::compute_va(input_base, facets = dvid, idv = "TAD")
      res_frem <- vaplot::compute_va(input_frem, facets = dvid, idv = "TAD")
      p2 <- vaplot::plot_va_compare(`without covariates` = res_base,
                                   `with covariates` = res_frem,
                                   smooth = TRUE)+
        ggplot2::theme_bw()+
        ggplot2::theme(legend.position = "bottom")
      p <- list(p,p2)
    }
    p
  },
    error = function(e) return(e)
  ) %>% as_result()
  return(p)
}

#' Prepare VPC for parvar results
#'
#' @param qa_results QA result data structure
#' @param transformation Name of the parvar transformation to create the VPC for
#'
#' @return A result data structure
#' @export
prepare_parvar_vpc <- function(qa_results, transformation = c("fullblock", "boxcox", "tdist")){
  transformation <- match.arg(transformation)
  obs <- get_result(qa_results$simeval$original_tab)

  parvar_res <- get_result(qa_results$parvar[[transformation]])
  parvar_sim <- get_result(parvar_res$sim_tab)
  p <- tryCatch({
    original_sim <- get_result(qa_results$simeval$sim_tab1) %>%
      cbind(obs[qa_results$options$idv_name])
    facets <- "type"
    if(qa_results$options$dvid_name!=""){
      dvid_col <- obs[qa_results$options$dvid_name]
      original_sim <- cbind(original_sim, dvid_col)
      parvar_sim <- cbind(parvar_sim, dvid_col)
      facets <- c("type", qa_results$options$dvid_name)
    }
    obs_combined <- dplyr::bind_rows(original = obs, `with transformation` = obs, .id = "type")
    sim_combined <- dplyr::bind_rows(original = original_sim, `with transformation` = parvar_sim, .id = "type")
    vpc::vpc(
      sim = sim_combined,
      obs = obs_combined,
      obs_cols = list(
        dv = "DV",                             # these column names are the default,
        idv = qa_results$options$idv_name),                         # update these if different.
      sim_cols = list(
        dv = "DV",
        idv = qa_results$options$idv_name),
      bins = "percentiles",
      n_bins = 10,
      stratify  = facets
    )+
      theme_bw() +
      facet_wrap(facets)+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  },
  error = function(e) return(e)) %>%
    as_result()
  return(p)
}
