level_3 <- function(pmp_obs, pmp_sim, n_posterior_draws=1000, eps=1e-10){
  # given observed pmp and simulated pmp, do level 3.
  pmp_sim$pmp <- with(pmp_sim, as.matrix(pmp_sim[, colnames(pmp_sim)[3:ncol(pmp_sim)]]))
  ## if pmp include zero, adjust values
  pmp_sim$pmp <- avoid_error_by_zero_one(pmp_sim$pmp, eps)

  print("Start fitting meta model")
  meta_model_posteriors <- get_meta_model_posteriors(pmp_sim, n_posterior_draws = n_posterior_draws)
  meta_model_param <- extract_meta_model_param(meta_model_posteriors)

  # Predictive mixture
  print("Start obtaining predictive mixture distribution")
  mixture_function <- create_mixture_function(pmp_obs, meta_model_param)

  out <- list(pmp_sim = pmp_sim, simulated_data_matrix = NULL, meta_model_param = meta_model_param, pmp_obs = pmp_obs, mixture_function = mixture_function)
  class(out) <- "meta_uncertainty_fit"
  return(out)
}
