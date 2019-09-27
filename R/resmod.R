read_resmod_table <- function(path){
  if(!file.exists(path)) rlang::cnd_signal(cnd_file_not_found(path))
  file_content <- readr::read_lines(path)
  header <- scan(text = file_content[1], what = character(), sep = ",", quiet = TRUE)
  if(! identical(header, c("Iteration", "DVID","Model", "dOFV", "Parameters"))) 
    rlang::cnd_signal(cnd_unexpected_file_format(path))
  purrr::map(file_content[-1], ~scan(text = .x, what = character(), sep = ",", quiet = TRUE)) %>% 
    purrr::map_dfr(~list(iteration=.x[1], dvid=.x[2], model=.x[3], dofv=.x[4], prms = list(.x[-c(1:4)]))) %>% 
    dplyr::mutate(dofv = as.numeric(dofv),
                  dvid = factor(dvid),
                  model = factor(model))
}