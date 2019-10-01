qa_settings <- function(){
  list(
    linearize_path = "linearize_run/scm_dir1",
    derivatives_lst_path = "linearize_run/scm_dir1/derivatives.lst",
    scm_path = "scm_run"
  )
}

qa_files <- function(path, model_filename, settings){
  linebase_lst <- file.path(path, sub(".([^.]*)$", "_linbase.lst", model_filename))
  return(
    list(
      linearize = list(
        # non-linear model used to get derivatives for linearization
        derivatives_ext = file.path(path, ext_file(settings$derivatives_lst_path)),
        # linearized model
        linbase_ext = ext_file(linebase_lst),
        linebase_phi = phi_file(linebase_lst)
      ),
      scm = list(
        raw_results_csv = file.path(path, settings$scm_path, "raw_results_scm.csv")
      )
    )
  )
}