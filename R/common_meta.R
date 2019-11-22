#' Read the PsN metadata. To be used as an object
#' @export
metadata <- function(directory='.') {
    yaml::read_yaml(file.path(directory, 'meta.yaml'))
}

#' @export
rplots_level <- function(meta) {
    level <- meta$common_options$rplots
    if (level < 1) {
        1
    } else {
        level
    }
}

#' @export
model_path <- function(meta) {
    meta$model_files[1]
}