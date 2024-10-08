# Sample true model and create data frame to store pmp from simulations.
# @param prior_model_prob Prior distribution from which true model is drawn
# @param n_model number of model
# @param n_sim number of simulation
# @return data.frame containing row number (k), true model index and n_model columns of NA. Number of row is equal to n_sim.
sample_true_model <- function(prior_model_prob=c(...), n_model=3, n_sim=3){
  # validation of prior_model_prob
  if (! is.character(prior_model_prob)) {
    if (length(prior_model_prob) != n_model | !all(prior_model_prob < 1)
        | !all(prior_model_prob > 0) | !sum(prior_model_prob) == 1) {
      stop("prior_model_prob is not valid.")
    }
  }
  else{
    prior_model_prob <- rep(1/n_model, n_model)
  }

  # Preparation
  pmp_sim <- data.frame(
    k = 1:n_sim,
    true_model_idx = rep(0, n_sim)
  )

  # Sampling true model
  counter <- 0
  while(length(unique(pmp_sim$true_model_idx)) != n_model){
    pmp_sim$true_model_idx <- base::sample(1:n_model, size=n_sim, replace=TRUE, prob=prior_model_prob)
    counter <- counter + 1
    if (counter == 1000){
      stop("Sampling of true model exceed maximum number of trial. Check n_sim and prior_model_prob.")
    }
  }

  for (j in 1:n_model){
    col_name <- paste0("pmp", j)
    pmp_sim[col_name] <- NA
  }
  pmp_sim
}

# Create brmfit object when user gives list of bf formula and prior and additional argument with brmsargs
# @param formula_list list of brmsfit object
# @param prior_list list of brmsprior obejct
# @param family list of brmsfamily object
# @param data data to fit all the model
# @param brms_arg_list list of list containing additional argument to be passed when fitting model with brms.
# @return list of brmsfit object
create_brmsfit_list <- function(formula_list=list(), prior_list=NULL, family_list=NULL, data=NULL, brms_arg_list=NULL, verbosity=1){
  n_model = length(formula_list)
  brmsfit_list <- list()
  supress_all({
  pb <- utils::txtProgressBar(min = 0, max = n_model, style = 3, width = 50, char = "=")
  for (j in 1:n_model){
    brms::validate_prior(prior_list[[j]], formula_list[[j]], data, family_list[[j]])
    brms_arg_list[[j]]$formula <- formula_list[[j]]
    brms_arg_list[[j]]$prior <- prior_list[[j]]
    brms_arg_list[[j]]$data <- data
    brms_arg_list[[j]]$family <- family_list[[j]]
    brmsfit_list[[j]] <- brms::do_call(brms::brm, brms_arg_list[[j]])
    utils::setTxtProgressBar(pb, j)
  }
  close(pb)
  }, verbosity)
  return(brmsfit_list)
}

# Simulate data y given prior.
# @param brmsfit_list
# @pmp_sim data.frame to contain simulated pmp
# ...
# @return n_sim by nrow(data) matrix containig simulated 'resp' given prior and predetermined variables.
simulate_data <- function(brmsfit_list=list(), pmp_sim=NULL, n_model=3, n_sim=3, warmup=5000, suppressor=suppress_mwo){
    # simulate "formula$resp" from model given prior and pre-determined variables
    resp <- brmsfit_list[[1]]$formula$resp
    dat <- brmsfit_list[[1]]$data
    simulated_data_matrix <- matrix(data = NA, nrow = n_sim, ncol = nrow(dat))
    suppressor(
    for (j in 1:n_model){
      n_sim_model_j <- unname(table(pmp_sim$true_model_idx)[j])
      prior_predictive_fit <- stats::update(brmsfit_list[[j]],
                                            chains = 1,
                                            sample_prior = "only",
                                            iter = warmup + n_sim_model_j,
                                            warmup = warmup,
                                            refresh = 0)
      simulated_quantities <- brms::posterior_predict(prior_predictive_fit, ndraws = n_sim_model_j, refresh=0, silent=2)
      simulated_data_matrix[which(pmp_sim$true_model_idx == j), ] <- simulated_quantities
    })
    simulated_data_matrix
}

# Obtain simulated pmp given simulated data
# @param brmsfit_list
# @param pmp_sim
# @param n_model
# @param n_sim
# @param simulated_data_matrix Obtaind with simulate_data()
# @return pmp_sim: the matrix with NA filled with simulated pmp. Nested pmp is added to the matrix for later use.
post_prob_from_sim <- function(brmsfit_list=list(), pmp_sim=NULL, n_model=3, n_sim=3, simulated_data_matrix=NULL, suppressor=suppress_mwo, verbosity=1){
    # Level 2: Obtain post model probability from simulated data
  supress_all({
    resp <- brmsfit_list[[1]]$formula$resp
    simulated_data <- brmsfit_list[[1]]$data
    pb <- utils::txtProgressBar(min = 0, max = n_sim, style = 3, width = 50, char = "=")
    for (k in 1:n_sim){
      true_model_idx = pmp_sim$true_model_idx[k]
      ## combine simulated data and predetermined data
      simulated_data[, resp] <- simulated_data_matrix[k, ]
      fit_sim <- list()
      suppressor(
      for (j in 1:n_model){
        fit_sim[[j]] <- stats::update(brmsfit_list[[j]],
                                      newdata = simulated_data,
                                      save_pars = brms::save_pars(all = TRUE),
                                      refresh = 0)
      })
      suppressor(
      pmp <- brms::do_call(brms::post_prob, fit_sim)
      )

      if(any(is.na(pmp))){
        stop("Posterior model probability takes NA. Try rerunning with more samples.")
      }

      for (j in 1:n_model){
        col_name <- paste0("pmp", j)
        pmp_sim[k, col_name] <- pmp[j]
      }
      utils::setTxtProgressBar(pb, k)
    }
    close(pb)
    ## create nested pmp for "meta_model_posteriors"
    pmp_sim$pmp <- with(pmp_sim, as.matrix(pmp_sim[, colnames(pmp_sim)[3:ncol(pmp_sim)]]))

  }, verbosity)

    pmp_sim
}

# Extract meta model parameter
# @param meta_model_posteriors obtained from get_meta_model_posteriors
# @return data.frame containing meta model parameters
extract_meta_model_param <- function(meta_model_posteriors=NULL){
  # only three models
  meta_model_param <- data.frame(
    true_model_idx = 1:3
  )
  meta_model_param$mu <- rep(list(NA), nrow(meta_model_param))
  meta_model_param$Sigma <- rep(list(NA), nrow(meta_model_param))
  for (j in 1:3){
    row <- which(meta_model_param$true_model_idx==j)
    mu <- meta_model_posteriors[[paste0("true_model_", j)]]$mu
    Sigma <- meta_model_posteriors[[paste0("true_model_", j)]]$Sigma
    meta_model_param$mu[row] <- list(apply(mu, 2, mean))
    meta_model_param$Sigma[row] <- list(apply(Sigma, c(2, 3), mean))
  }
  meta_model_param
}
# Create predictive mixture model
# @param pmp_obs vector of observed pmps.
# @param meta_model_param obtaind with extract_meta_model_param()
# @return data.frame containing meta model parameters
create_mixture_function <- function(pmp_obs=c(...), meta_model_param=NULL){
  # only three models with logistic normal.
  mixture_function <- purrr::partial(logistic_normal_mixture,
                                     theta = pmp_obs,
                                     mu_list = list(
                                       meta_model_param$mu[meta_model_param$true_model_idx==1][[1]],
                                       meta_model_param$mu[meta_model_param$true_model_idx==2][[1]],
                                       meta_model_param$mu[meta_model_param$true_model_idx==3][[1]]
                                     ),
                                     Sigma_list = list(
                                       meta_model_param$Sigma[meta_model_param$true_model_idx==1][[1]],
                                       meta_model_param$Sigma[meta_model_param$true_model_idx==2][[1]],
                                       meta_model_param$Sigma[meta_model_param$true_model_idx==3][[1]]
                                     )
  )
  return(mixture_function)
}

# Avoid simulated pmp to take zero or one to avoid error
# @param nested_pmp last n_model row of pmp_sim
# @param eps small number to be added / subtracted
# @return nested_pmp with correction in zero and one
avoid_error_by_zero_one <- function(nested_pmp=NULL, eps=1e-3){
  # add small number to all data and normalize it when pmp include 0 (hence not able to fit logistic normal)
  if (any(nested_pmp == 0)){
    nested_pmp <- nested_pmp + eps
    nested_pmp <- t(apply(nested_pmp, 1, function(row) row / sum(row)))
  }

  if (any(nested_pmp == 1)){
    nested_pmp <- nested_pmp - eps
    nested_pmp <- t(apply(nested_pmp, 1, function(row) row / sum(row)))
  }
  nested_pmp
}

# Check given object is "metabmcfit"
# @param metabmcfit object to be tested
# @return TRUE if it is metabmcfit object FALSE otherwise
is.metabmcfit <- function(metabmcfit){
  class(metabmcfit) == "metabmcfit"
}

# Get prep object
# @inheritParams brms::posterior_predict
# @return prep object
get_prep <- function(
    object, newdata = NULL, re_formula = NULL, re.form = NULL,
    transform = NULL, resp = NULL, negative_rt = FALSE,
    ndraws = NULL, draw_ids = NULL, sort = FALSE, ntrys = 5,
    cores = NULL, ...
) {
  cl <- match.call()
  if ("re.form" %in% names(cl)) {
    re_formula <- re.form
  }
  object <- brms::restructure(object)
  prep <- brms::prepare_predictions(
    object, newdata = newdata, re_formula = re_formula, resp = resp,
    ndraws = ndraws, draw_ids = draw_ids, check_response = FALSE, ...
  )
  return(prep)
}

# Mute message/warrings from brms and Stan
# @param exp code to be muted
# @return expr run muted
suppress_mwo <- function(expr=NULL){
  suppressMessages(
    suppressWarnings(
      invisible(capture.output(expr, type = "output"))
    )
  )
}

# Wrapper for suppress_mwo when verbosity = 0
supress_all <- function(exp=NULL, verbosity=1){
  if (verbosity == 0){
    suppress_mwo(exp)
  }
  else {
    exp
  }
}


