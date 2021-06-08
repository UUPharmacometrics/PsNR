#' @export
plot_structural_vpc <- function(obs,sim,shift_tab,idv) {
  p <- vpc::vpc (sim = sim,
                 obs = obs,
                 obs_cols = list(
                   dv = "DV",                             # these column names are the default,
                   idv = toupper(idv)),                         # update these if different.
                 sim_cols = list(
                   dv = "DV",
                   idv = toupper(idv)),
                 bins = "percentiles",
                 n_bins = 10
  )+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
