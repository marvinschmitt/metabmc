#' Fit meta models and return posterior draws
#'
#' @param data simulated data
#' @param n_posterior_draws number of posterior draws after warm-up
#'
#' @return List of posterior draws for each model
#' @export
get_meta_model_posteriors <- function(data=NULL, n_posterior_draws=100, suppressor=suppress_mwo) {

  pb <- utils::txtProgressBar(min = 0, max = 3, style = 3, width = 50, char = "=")
  meta_model_fit_list <- list()
  for (j in 1:3){
    suppressor({
    meta_model_fit_list[[j]] <- brms::brm(
      pmp ~ 1,
      data = dplyr::filter(data, true_model_idx == j),
      family = brms::brmsfamily("logistic_normal"),
      backend = "cmdstanr",
      chains = 2,
      cores = 2,
      iter = 1000 + n_posterior_draws,
      warmup = 1000,
      refresh = 0)
    })
    utils::setTxtProgressBar(pb, j)
  }

  new_data = data.frame(x = c(1:3))

  prep_1 = get_prep(meta_model_fit_list[[1]], newdata = new_data)
  prep_2 = get_prep(meta_model_fit_list[[2]], newdata = new_data)
  prep_3 = get_prep(meta_model_fit_list[[3]], newdata = new_data)

  mu_post_1 = brms:::get_Mu(prep_1, 1)
  Sigma_post_1 = brms:::get_Sigma(prep_1, 1, cor_name = "lncor")

  mu_post_2 = brms:::get_Mu(prep_2, 1)
  Sigma_post_2 = brms:::get_Sigma(prep_2, 1, cor_name = "lncor")

  mu_post_3 = brms:::get_Mu(prep_3, 1)
  Sigma_post_3 = brms:::get_Sigma(prep_3, 1, cor_name = "lncor")


  meta_model_posteriors = list(
    "true_model_1" = list("mu" = mu_post_1,
                          "Sigma" = Sigma_post_1),
    "true_model_2" = list("mu" = mu_post_2,
                          "Sigma" = Sigma_post_2),
    "true_model_3" = list("mu" = mu_post_3,
                          "Sigma" = Sigma_post_3)
  )

  close(pb)

  return(meta_model_posteriors)
}
