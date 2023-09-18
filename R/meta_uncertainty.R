#' Fit Meta Uncertainty model
#' @param ... brmsfit object
#' @param formula_list List of brmsformula
#' @param prior_list List of vector of prior
#' @param family_list List of brmsfamily
#' @param data Data to fit Meta Uncertainty
#' @param n_sim Number of simulation to obtain posterior model probability
#' @param n_posterior_draws Number of posterior draws
#' @param warmup Number of warmup
#' @param prior_model_prob Prior distribution from which true model is drawn
#' @param eps Small number to be added / subtracted when posterior model probability takes 0 / 1
#' @param brms_arg_list Additional argument to be passed to brms::brm when fitting model
#'
#' @returns meta_uncertainty object which is to be passed to plotting function for inspection of uncertainty in posterior model probability.
#'
#' @export
meta_uncertainty <- function(..., formula_list, prior_list, family_list = c(gaussian(), gaussian(), gaussian()), data = NULL, n_sim=100, n_posterior_draws=1000, warmup = 5000, prior_model_prob="uniform", eps = 1e-10, brms_arg_list = NULL){
  brmsfit_list <- list(...)

  # validate argument and create brmsfit object using formula_list, prior_list, familiy_list and additional arguments
  if (length(brmsfit_list) == 0){
    message("Start fitting pmp model with observed data")
    brmsfit_list <- create_brmsfit_list(formula_list, prior_list, family_list, data, n_sim)
  }
  n_model <- length(brmsfit_list)

  # sample true model and return data frame to store pmp from simulation
  pmp_sim <- sample_true_model(prior_model_prob, n_model, n_sim)

  # Level2: Simulation of model implied data
  message(paste0("Start obtaining pmp with ", n_sim, " simulations"))
  post_prob_from_sim_out <- post_prob_from_sim(brmsfit_list, pmp_sim, n_model, n_sim, warmup)
  pmp_sim <- post_prob_from_sim_out$pmp_sim
  simulated_data_matrix <- post_prob_from_sim_out$simulated_data_matrix

  # Level3: Fit meta_model
  ## create nested pmp for "meta_model_posteriors"
  pmp_sim$pmp <- with(pmp_sim, as.matrix(pmp_sim[, colnames(pmp_sim)[3:ncol(pmp_sim)]]))
  ## if pmp include zero or 1, adjust values
  pmp_sim$pmp <- avoid_error_by_zero_one(pmp_sim$pmp, eps)

  message("Start fitting meta model")
  meta_model_posteriors <- get_meta_model_posteriors(pmp_sim, n_posterior_draws = n_posterior_draws)
  meta_model_param <- extract_meta_model_param(meta_model_posteriors)

  # Predictive mixture
  ## calculate observed pmp
  message("Start obtaining pmp with observed data")
  invisible(capture.output(pmp_obs <- brms::do_call(brms::post_prob, brmsfit_list)))
  message("Start obtaining predictive mixture distribution")
  mixture_function <- create_mixture_function(pmp_obs, meta_model_param)

  out <- list(pmp_sim = pmp_sim, simulated_data_matrix = simulated_data_matrix, meta_model_param = meta_model_param, pmp_obs = pmp_obs, mixture_function = mixture_function)
  class(out) <- "meta_uncertainty_fit"
  return(out)
}
