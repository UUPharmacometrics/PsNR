#' Read in QA results
#' 
#' The function is the central place for collecting all QA results. It uses the provided path to the QA directory
#' to read-in all necessary information and return them as a nested list structure.
#'
#' @param path Path of the QA run directory
#' @param model_filename Name of the model file QA was run on
#' @param psn_options List of options provided by PSN
#' @param settings Settings 
#'
#' @return List
#' @export
retrieve_qa_results <- function(path, model_filename, psn_options, settings = qa_settings()){
  files <- qa_files(path, model_filename, settings)
  
  ofv_df <- tryCatch(get_ofv_summary(nonlin_ext_path  = files$linearize$derivatives_ext,
                            lin_ext_path = files$linearize$linbase_ext,
                            lin_phi_path = files$linearize$linebase_phi), 
                     error = function(e) return(e)) %>% 
    as_result()
  
  scm_df <- tryCatch(retrieve_scm_results(files$scm$raw_results_csv, 
                                 parameters = psn_options$parameters, 
                                 continuous = psn_options$continuous, 
                                 categorical = psn_options$categorical),
                     error = function(e) return(e)) %>% 
    as_result()
  
  return(
    list(
      model_filename = model_filename,
      linearize = list(
        # table of OFV values
        ofv_comparison = ofv_df
      ),
      resmod = list(
        idvs = get_resmod_idvs(path)
      ),
      scm = scm_df,
      frem = NULL,
      cdd = NULL,
      simeval = NULL
    )
    )
}

#' Retrieve OFV summary table
#'
#' @param nonlin_ext_path Path to derivatives.ext
#' @param lin_ext_path Path to linbase.ext
#' @param lin_phi_path Path to linbase.phi
#'
#' @return data.frame(name,ofv,result)
get_ofv_summary <- function(nonlin_ext_path, 
                            lin_ext_path, 
                            lin_phi_path){
  
  nonlin <-  tryCatch(read_nm_ext(nonlin_ext_path) %>% get_final_ofvs(),
                      error = function(e) return(e)) %>% 
    as_result()
  
  lin_ext <- tryCatch(read_nm_ext(lin_ext_path),
                      error = function(e) return(e))
  if(!is_error(lin_ext)){
    lin_init <- tryCatch(get_initial_ofvs(lin_ext),
                         error = function(e) return(e)) %>% 
      as_result()
    lin_final <- tryCatch(get_final_ofvs(lin_ext),
                          error = function(e) return(e)) %>% 
      as_result()
  }else{
    lin_init <- lin_final <- as_result(lin_ext)
  }

  lin_final_iofv <- tryCatch(read_nm_phi(lin_phi_path) %>%  get_iofv_sum(),
                             error = function(e) return(e)) %>% 
    as_result()
  
  collect_results(nonlin = nonlin, 
                  lin_init = lin_init, 
                  lin_final = lin_final, 
                  lin_final_iofv = lin_final_iofv,
                  .id = "name", .result = "ofv")
}

get_resmod_idvs <- function(path){
  idvs <- list.files(path, "^resmod_") %>% sub(".*resmod_", "", .)
  return(idvs)
}

#' Retrieve SCM results
#'
#' @param scm_path Path to raw_result_scm.csv
#' @param parameters Character vector of parameter names
#' @param continuous Character vector of continuous covariate names
#' @param categorical Character vector of categorical covariate names 
#'
#' @return data.frame()
retrieve_scm_results <- function(scm_path, parameters, continuous = NULL, categorical = NULL ){
  tab <- read_psn_rawres(scm_path)
  
  relations <- tab$relation[-1]
  
  states <- c("none", "linear", "hockey-stick", "exponential", "power")
  # prepare table of expected relations and match against found ones
  expected_relations <- purrr::cross_df(list(
    covariate = c(continuous, categorical), 
    parameter = parameters)) %>% 
    dplyr::mutate(name = paste0(.data$parameter, .data$covariate),
                  is_categorical = .data$covariate %in% categorical,
                  state_name = purrr::map(.data$name, ~grep(.x, relations, fixed = TRUE, value = TRUE))
    ) %>% 
    tidyr::unnest(.data$state_name) %>% 
    # determine which SCM state and categorical split the state_name corresponds to
    dplyr::mutate(state = stringr::str_extract(.data$state_name, "\\d+$") %>% as.integer(),
                  relationship = states[state],
                  bin_split = stringr::str_extract(.data$state_name, "\\d+(?=-\\d+)") %>% as.integer())
 # determine runs that failed
 errors <- dplyr::filter(tab, grepl("run failed", .data$covariance_step_run, fixed = TRUE)) %>% 
   dplyr::select(state_name = "relation", error = "covariance_step_run") %>% 
   dplyr::mutate(error = purrr::map(error, ~cnd_nm_run_failed(reason = .)))

 tab <- dplyr::filter(tab, !grepl("run failed", .data$covariance_step_run, fixed = TRUE))
 # calculate dOFVs 
  dofvs <- dplyr::transmute(tab, 
                            state_name = .data$relation,  
                            dofv = .data$ofv[1] - .data$ofv) 
  # retrieve covariate prm values
  prm_values <- purrr::imap_dbl(paste0(tab$relation, "-1"), 
                                purrr::possibly(~tab[.y, .x, drop = TRUE], otherwise = NA_real_)) %>% 
    purrr::set_names(tab$relation) %>%
    tibble::enframe("state_name", "prm_value")
  # combine all results
  df <- dplyr::left_join(expected_relations, prm_values, by="state_name") %>% 
      dplyr::left_join(errors, by="state_name") %>% 
      dplyr::left_join(dofvs, by="state_name")  
  return(result_df(df))
}