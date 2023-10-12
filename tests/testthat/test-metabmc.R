test_that("Fit works given pre-fitted brms object", {
  dat <- data.frame(y = rnorm(10), x = rnorm(10), g = rep(1:5, 2))
  f1 <- brms::bf(y ~ x)
  f2 <- brms::bf(y ~ 1)
  fam1 <- gaussian(); fam2 <- brms::student()
  brms_arg_list_1 <- list(formula = f1, data = dat, family = fam1, backend = "mock", mock_fit = "1", rename = FALSE)
  brms_arg_list_2 <- list(formula = f2, data = dat, family = fam2, backend = "mock", mock_fit = "2", rename = FALSE)
  brmsfit_1 <- brms::do_call(brms::brm, brms_arg_list_1)
  brmsfit_2 <- brms::do_call(brms::brm, brms_arg_list_2)

  pmp_sim_1 <- data.frame(
    k = 1:3,
    true_model_idx = c(2, 1, 3),
    pmp1 = rep(NA, 3),
    pmp2 = rep(NA, 3),
    pmp3 = rep(NA, 3)
  )
  simulated_data_matrix <- matrix(c(rep(1,10), rep(2,10), rep(3,10)), ncol=10, byrow=TRUE)
  mockery::stub(metabmc, "sample_true_model", pmp_sim_1)
  mockery::stub(metabmc, "simulate_data", simulated_data_matrix)

  pmp_sim_2 <- pmp_sim_1
  pmp_sim_2[1, c("pmp1", "pmp2", "pmp3")] <- c(0.5, 0.3, 0.2)
  pmp_sim_2[2, c("pmp1", "pmp2", "pmp3")] <- c(0.3, 0.2, 0.5)
  pmp_sim_2[3, c("pmp1", "pmp2", "pmp3")] <- c(0.4, 0.4, 0.2)

  mockery::stub(metabmc, "post_prob_from_sim", pmp_sim_2)
  mockery::stub(metabmc, "get_meta_model_posteriors", "mock_meta_model_posterior")
  mockery::stub(metabmc, "extract_meta_model_param", "mock_meta_model_parameter")
  mockery::stub(metabmc, "brms::do_call", c(0.2, 0.2, 0.6))
  mockery::stub(metabmc, "create_mixture_function", "mock_mixture_function")
  expected_out <- list(pmp_sim = pmp_sim_2, simulated_data_matrix = simulated_data_matrix, meta_model_param = "mock_meta_model_parameter", pmp_obs = c(0.2, 0.2, 0.6), mixture_function = "mock_mixture_function")
  class(expected_out) <- "metabmcfit"
  expect_equal(metabmc(f1, f2, f2), expected_out)
})

test_that("Fit works given formula, family, data, et cetra", {
  dat <- data.frame(y = rnorm(10), x = rnorm(10), z = rnorm(10))
  f1 <- brms::bf(y ~ x)
  f2 <- brms::bf(y ~ z)
  fam1 <- gaussian(); fam2 <- brms::student()
  prior <- brms::prior("normal(0,1)")
  pmp_sim_1 <- data.frame(
    k = 1:3,
    true_model_idx = c(2, 1, 3),
    pmp1 = rep(NA, 3),
    pmp2 = rep(NA, 3),
    pmp3 = rep(NA, 3)
  )
  simulated_data_matrix <- matrix(c(rep(1,10), rep(2,10), rep(3,10)), ncol=10, byrow=TRUE)
  mockery::stub(metabmc, "sample_true_model", pmp_sim_1)
  mockery::stub(metabmc, "simulate_data", simulated_data_matrix)

  pmp_sim_2 <- pmp_sim_1
  pmp_sim_2[1, c("pmp1", "pmp2", "pmp3")] <- c(0.5, 0.3, 0.2)
  pmp_sim_2[2, c("pmp1", "pmp2", "pmp3")] <- c(0.3, 0.2, 0.5)
  pmp_sim_2[3, c("pmp1", "pmp2", "pmp3")] <- c(0.4, 0.4, 0.2)

  mockery::stub(metabmc, "post_prob_from_sim", pmp_sim_2)
  mockery::stub(metabmc, "get_meta_model_posteriors", "mock_meta_model_posterior")
  mockery::stub(metabmc, "extract_meta_model_param", "mock_meta_model_parameter")
  mockery::stub(metabmc, "brms::do_call", c(0.2, 0.2, 0.6))
  mockery::stub(metabmc, "create_mixture_function", "mock_mixture_function")
  expected_out <- list(pmp_sim = pmp_sim_2, simulated_data_matrix = simulated_data_matrix, meta_model_param = "mock_meta_model_parameter", pmp_obs = c(0.2, 0.2, 0.6), mixture_function = "mock_mixture_function")
  class(expected_out) <- "metabmcfit"
  out <- metabmc(formula_list = list(f1, f2, f2), prior_list = list(prior, prior, prior), family_list = list(fam1, fam2, fam2), data=dat, brms_arg_list = list(list(backend = "mock", mock_fit = "1", rename = FALSE), list(backend = "mock", mock_fit = "2", rename = FALSE), list(backend = "mock", mock_fit = "3", rename = FALSE)))
  expect_equal(out, expected_out)
})
