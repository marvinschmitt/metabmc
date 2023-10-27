#' Fit meta model for posterior model probabilities and predictive mixture model
#'
#' Fit meta model for posterior model probabilities and predictive mixture model.
#' All-in-one function to simulate posterior model probabilities given model specification and prior, estimate density
#' over simulated posterior model probabilities (meta models), and estimate predictive mixture model combining meta models
#' and observed posterior model probabilities. Currently only three model candidate is supported.
#' After fitting the models, results can be visualized using following three functions;
#' \code{\link{plot_simulated_pmp}}, \code{\link{plot_meta_model_density}}, and \code{\link{plot_predictive_mixture}}.
#'
#' @param ... brmsfit object
#' @param formula_list List of brmsformula
#' @param prior_list List of vector of prior
#' @param family_list List of brmsfamily
#' @param data Data to fit
#' @param n_sim Number of simulation to obtain posterior model probability
#' @param n_posterior_draws Number of posterior draws
#' @param warmup Number of warmup
#' @param prior_model_prob Prior distribution from which true model is drawn
#' @param eps Small number to be added / subtracted when posterior model probability takes 0 / 1
#' @param brms_arg_list Additional argument to be passed to brms::brm when fitting model
#'
#' @returns metabmc object which is to be passed to plotting function for inspection of uncertainty in posterior model probability.
#'
#' @examples
#' \dontrun{
#' # brmsfit objects as arguments
#' fit1 <- brm(y ~ (1 | g) + x, data = d)
#' fit2 <- brm(y ~ x, data = d)
#' fit3 <- brm(y ~ (x | g), data = d)
#' metabmc_fit <- metabmc(fit1, fit2, fit3, n_sim=10)
#'
#' # Using list of formula, data and other arguments
#' m1 <- bf(y ~ (1 | g) + x)
#' m2 <- bf(y ~ x)
#' m3 <- bf(y ~ (x | g))
#'
#' metabmc_fit <- metabmc(list(m1, m2, m3), data=d, n_sim=10)
#' }
#' @export
metabmc <- function(..., formula_list, prior_list, family_list = rep(NULL, 3), data = NULL, n_sim=20, n_posterior_draws=1000, warmup = 5000, prior_model_prob="uniform", eps = 1e-3, brms_arg_list = NULL, verbosity=1){
  brmsfit_list <- list(...)

  # setting of verbosity
  if (verbosity != 2){
    suppressor <- suppress_mwo
  }
  else {
    suppressor <- function(exp){exp}
  }

  supress_all({

  # validate argument and create brmsfit object using formula_list, prior_list, familiy_list and additional arguments
  cat("Step 1 / 4: Fitting model with observed data.\n")
  if (length(brmsfit_list) == 0){
    brmsfit_list <- create_brmsfit_list(formula_list, prior_list, family_list, data, brms_arg_list, verbosity)
  }
  n_model <- length(brmsfit_list)

  # sample true model and return data frame to store pmp from simulation
  pmp_sim <- sample_true_model(prior_model_prob, n_model, n_sim)

  # Level2: Simulation of model implied data and obtain post model probability
  cat(paste0("Step 2 / 4: Obtaining pmp with ", n_sim, " simulations.\n"))
  simulated_data_matrix <- simulate_data(brmsfit_list, pmp_sim, n_model, n_sim, warmup, suppressor)
  pmp_sim <- post_prob_from_sim(brmsfit_list, pmp_sim, n_model, n_sim, simulated_data_matrix, suppressor)
  # Level3: Fit meta_model
  ## if pmp include zero or 1, adjust values
  pmp_sim$pmp <- avoid_error_by_zero_one(pmp_sim$pmp, eps)
  cat("Step 3 / 4: Fitting meta model.\n")
  meta_model_posteriors <- get_meta_model_posteriors(pmp_sim, n_posterior_draws, suppressor, verbosity)
  meta_model_param <- extract_meta_model_param(meta_model_posteriors)

  # Predictive mixture
  cat("Step 4 / 4: Obtaining pmp with observed data.\n")
  suppressor(
  pmp_obs <- brms::do_call(brms::post_prob, brmsfit_list))
  if(any(is.na(pmp_obs))){
    stop("Posterior model probability takes NA. Try rerunning with more samples.\n")
  }
  mixture_function <- create_mixture_function(pmp_obs, meta_model_param)

  out <- list(pmp_sim = pmp_sim, simulated_data_matrix = simulated_data_matrix, meta_model_param = meta_model_param, pmp_obs = pmp_obs, mixture_function = mixture_function)
  class(out) <- "metabmcfit"

  }, verbosity)
  return(out)
}
