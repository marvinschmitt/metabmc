#' Obtain meta_uncertainty_fit object given observed and simulated posterior model probability.
#'
#' @param pmp_obs vector of observed model probability.
#' @param pmp_sim dataframe containing index of true model in the first column and the pmp corresponding to each models.
#' @param n_posterior_draws number of posterior draws after warm-up
#' @param eps small number to be added / subtracted when posterior model probability takes 0 / 1
#' @return meta_uncertainty object
#' @export
level_3 <- function(pmp_obs, pmp_sim, n_posterior_draws=1000, eps=1e-10, verbosity=1){
  # setting of verbosity
  if (verbosity != 2){
    suppressor <- suppress_mwo
  }
  else {
    suppressor <- function(exp){exp}
  }

  supress_all({

  pmp_sim$pmp <- with(pmp_sim, as.matrix(pmp_sim[, colnames(pmp_sim)[3:ncol(pmp_sim)]]))
  ## if pmp include zero, adjust values
  pmp_sim$pmp <- avoid_error_by_zero_one(pmp_sim$pmp, eps)

  print("Fitting meta model")
  meta_model_posteriors <- get_meta_model_posteriors(pmp_sim, n_posterior_draws = n_posterior_draws, suppressor)
  meta_model_param <- extract_meta_model_param(meta_model_posteriors)

  # Predictive mixture
  mixture_function <- create_mixture_function(pmp_obs, meta_model_param)

  out <- list(pmp_sim = pmp_sim, simulated_data_matrix = NULL, meta_model_param = meta_model_param, pmp_obs = pmp_obs, mixture_function = mixture_function)
  class(out) <- "metabmcfit"
  }, verbosity)
  return(out)
}
