#' Read in QA results
#' 
#' The function is the central place for collecting all QA results. It uses the provided path to the QA directory
#' to read-in all necessary information and return them as a nested list structure.
#'
#' @param path Path of the QA run directory
#' @param model_filename Name of the model file QA was run on
#' @param settings Settings 
#'
#' @return List
#' @export
retrieve_qa_results <- function(path, model_filename, settings = qa_settings()){
  files <- qa_files(path, model_filename, settings)
  
  ofv_df <- get_ofv_summary(nonlin_ext_path  = files$linearize$derivatives_ext,
                            lin_ext_path = files$linearize$linbase_ext,
                            lin_phi_path = files$linearize$linebase_phi)
  
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
  
  nonlin <-  do_safely(read_nm_ext(nonlin_ext_path) %>% get_final_ofvs())
  lin_ext <- do_safely(read_nm_ext(lin_ext_path))
  if(!is.na(lin_ext$result)){
    lin_init <- do_safely(get_initial_ofvs(lin_ext$result))
    lin_final <- do_safely(get_final_ofvs(lin_ext$result)) 
  }else{
    lin_init <- lin_ext
    lin_final <- lin_ext
  }
  lin_final_iofv <- do_safely(read_nm_phi(lin_phi_path) %>%  get_iofv_sum())
  
  list(nonlin = nonlin, lin_init = lin_init, lin_final = lin_final, lin_final_iofv = lin_final_iofv) %>% 
    purrr::imap(~purrr::update_list(.x, 
                                    name = .y, 
                                    ofv = as.numeric(.x$result),
                                    result = purrr::zap())) %>% 
    purrr::transpose() %>%
    purrr::simplify_all() %>% 
    tibble::as_tibble() 
}

get_resmod_idvs <- function(path){
  idvs <- list.files(path, "^resmod_") %>% sub(".*resmod_", "", .)
  return(idvs)
}
