qa_settings <- function(){
  list(
    linearize_path = "linearize_run/scm_dir1",
    derivatives_lst_path = "linearize_run/scm_dir1/derivatives.lst",
    fullblock_lst_path = "modelfit_run/fullblock.lst",
    boxcox_lst_path = "modelfit_run/boxcox.lst",
    tdist_lst_path = "modelfit_run/tdist.lst",
    scm_path = "scm_run",
    frem_path = "frem_run",
    simeval_path = "simeval_run"
  )
}

qa_files <- function(path, model_filename, settings){
  linbase_lst <- file.path(path, sub(".([^.]*)$", "_linbase.lst", model_filename))
  return(
    list(
      linearize = list(
        # non-linear model used to get derivatives for linearization
        derivatives_ext = file.path(path, ext_file(settings$derivatives_lst_path)),
        derivatives_lst = file.path(path, settings$derivatives_lst_path),
        # linearized model
        linbase_ext = ext_file(linbase_lst),
        linbase_phi = phi_file(linbase_lst),
        linbase_tab = sub_file_ext(linbase_lst, "dta")
      ),
      parvar = list(
        fullblock_ext = file.path(path, ext_file(settings$fullblock_lst_path)),
        fullblock_sim = file.path(path, sub_file_ext(settings$fullblock_lst_path, "sim")),
        boxcox_ext = file.path(path, ext_file(settings$boxcox_lst_path)),
        boxcox_sim = file.path(path, sub_file_ext(settings$boxcox_lst_path, "sim")),
        tdist_ext = file.path(path, ext_file(settings$tdist_lst_path)),
        tdist_sim = file.path(path, sub_file_ext(settings$tdist_lst_path, "sim"))
      ),
      scm = list(
        raw_results_csv = file.path(path, settings$scm_path, "raw_results_scm.csv")
      ),
      frem = list(
        path = file.path(path, settings$frem_path),
        m2_raw_results_csv = file.path(path, settings$frem_path, "model2_modelfit_dir1", "raw_results.csv"),
        m4_raw_results_csv = file.path(path, settings$frem_path, "model4_modelfit_dir1", "raw_results.csv")
      ),
      simeval = list(
        original_table = file.path(path, settings$simeval_path, "m1", "original_res_table.dta"),
        simulation_tables = purrr::map_chr(1:5, ~file.path(path, settings$simeval_path, "m1", paste0("sim_res_table-", .x, ".dta")))
      )
    ) %>% purrr::map_depth(2, normalizePath, mustWork = FALSE)
  )
}