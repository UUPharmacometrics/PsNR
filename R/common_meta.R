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

#' The prefix is what comes before the run number and the extension
#' i.e. 'run' for 'run1.mod'
#' @export
model_prefix <- function(meta) {
    no_ext <- tools::file_path_sans_ext(basename(model_path(meta)))
    gsub("\\d+$", "", no_ext)
}

#' @export
model_runno <- function(meta) {
    no_ext <- tools::file_path_sans_ext(basename(model_path(meta)))
    runno <- regmatches(no_ext, regexpr("\\d+", no_ext))
    if (length(runno) == 0) {
        runno <- ""
    }
    runno
}
