# sample_true_model
test_that("validation for prior model probability works", {
  expect_error(sample_true_model(c(0.1, 0.5, 0.8), 3, 10), "prior_model_prob is not valid.")
  expect_error(sample_true_model(c(0.1, 0.5, 0.8), 4, 10), "prior_model_prob is not valid.")
  expect_error(sample_true_model(c(0.1, 0.5), 4, 10), "prior_model_prob is not valid.")
  expect_error(sample_true_model(c(0.5, 0.5, 0), 3, 10), "prior_model_prob is not valid.")
  expect_error(sample_true_model(c(0, 1, 0), 3, 10), "prior_model_prob is not valid.")
})

test_that("correct model probability is returned", {
  out <- sample_true_model(c(rep(1/3, 3)), 3, 3)
  expect_equal(out$k, 1:3)
  expect_equal(out$pmp1, c(NA, NA, NA))
  expect_equal(out$pmp2, c(NA, NA, NA))
  expect_equal(out$pmp3, c(NA, NA, NA))
  expect_equal(length(unique(out$true_model_idx)), 3)
})

test_that("infinite roop is avoided", {
  expect_error(sample_true_model(c(0.999, 0.0005, 0.0005), 3, 3), "Sampling of true model exceed maximum number of trial. Check n_sim and prior_model_prob.")
})

# create_brmsfit_list
test_that("brms object is made given formula, prior, et cetera", {
  dat <- data.frame(y = rnorm(10), x = rnorm(10), z = rnorm(10))
  f1 <- brms::bf(y ~ x)
  f2 <- brms::bf(y ~ z)
  fam1 <- gaussian()
  fam2 <- brms::student()
  prior <- brms::prior("normal(0,1)")
  brms_arg_list_1 <- list(formula = f1, data = dat, family = fam1, prior = prior, backend = "mock", mock_fit = "1", rename = FALSE, silent=2)
  brms_arg_list_2 <- list(formula = f2, data = dat, family = fam2, prior = prior, backend = "mock", mock_fit = "2", rename = FALSE, silent=2)
  brmsfit_1 <- brms::do_call(brms::brm, brms_arg_list_1)
  brmsfit_2 <- brms::do_call(brms::brm, brms_arg_list_2)
  expected_out <- list(brmsfit_1, brmsfit_2)
  attr(expected_out[[1]]$data, 'data_name') <- ".x6" # not crucial so force the test to pass
  attr(expected_out[[2]]$data, 'data_name') <- ".x6"
  out <- create_brmsfit_list(formula_list = list(f1, f2), prior_list = list(prior, prior), family_list = list(fam1, fam2), data = dat, brms_arg_list = list(list(backend="mock", mock_fit = "1", rename = FALSE), list(backend="mock", mock_fit = "2", rename = FALSE)), verbosity=0)
  expect_equal(out, expected_out)
})

# avoid_error_by_zero_one
test_that("zero in pmp is avoided.", {
  nested_pmp <- matrix(c(0.5, 0.5, 0, 0.5, 0.5, 0), ncol=3)
  eps <- 0.001
  expected_row <- c(0.5 + eps, 0.5 + eps, eps)
  expected_row <- expected_row / sum(expected_row)
  expected_out <- matrix(c(expected_row, expected_row), ncol=3)
  expect_equal(avoid_error_by_zero_one(nested_pmp, 0.001), expected_out)
})

test_that("simulate_data works", {
  dat <- data.frame(y = rnorm(10), x = rnorm(10), z = rnorm(10))
  f1 <- brms::bf(y ~ x)
  f2 <- brms::bf(y ~ z)
  fam1 <- gaussian()
  fam2 <- brms::student()
  brms_arg_list_1 <- list(formula = f1, data = dat, family = fam1, backend = "mock", mock_fit = "1", rename = FALSE)
  brms_arg_list_2 <- list(formula = f2, data = dat, family = fam2, backend = "mock", mock_fit = "2", rename = FALSE)
  brmsfit_1 <- brms::do_call(brms::brm, brms_arg_list_1)
  brmsfit_2 <- brms::do_call(brms::brm, brms_arg_list_2)
  brmsfit_list <- list(brmsfit_1, brmsfit_2, brmsfit_2) # three models
  n_sim <- 3
  pmp_sim <- data.frame(
    k = 1:n_sim,
    true_model_idx = 1:3
  )
  for (j in 1:3){
    col_name <- paste0("pmp", j)
    pmp_sim[col_name] <- NA
  }
  m <- mockery::mock(rep(1,10), rep(2,10), rep(3,10))
  mockery::stub(simulate_data, "stats::update", 1)
  mockery::stub(simulate_data, "brms::posterior_predict", m)
  expected_out <- matrix(c(rep(1,10), rep(2,10), rep(3,10)), ncol=10, byrow=TRUE)
  expect_equal(simulate_data(brmsfit_list, pmp_sim, 3, n_sim, 0, suppress_mwo), expected_out)
})

test_that("post_prob_from_sim works", {
  simulate_data_matrix <- matrix(c(rep(1,10), rep(2,10), rep(3,10)), ncol=10, byrow=TRUE)
  dat <- data.frame(y = rnorm(10), x = rnorm(10), z = rnorm(10))
  f1 <- brms::bf(y ~ x)
  f2 <- brms::bf(y ~ z)
  fam1 <- gaussian()
  fam2 <- brms::student()
  brms_arg_list_1 <- list(formula = f1, data = dat, family = fam1, backend = "mock", mock_fit = "1", rename = FALSE)
  brms_arg_list_2 <- list(formula = f2, data = dat, family = fam2, backend = "mock", mock_fit = "2", rename = FALSE)
  brmsfit_1 <- brms::do_call(brms::brm, brms_arg_list_1)
  brmsfit_2 <- brms::do_call(brms::brm, brms_arg_list_2)
  brmsfit_list <- list(brmsfit_1, brmsfit_2, brmsfit_2) # three models
  n_sim <- 3
  pmp_sim_1 <- data.frame(
    k = 1:n_sim,
    true_model_idx = 1:3
  )
  for (j in 1:3){
    col_name <- paste0("pmp", j)
    pmp_sim_1[col_name] <- NA
  }
  m <- mockery::mock(c(0.5, 0.3, 0.2), c(0.3, 0.2, 0.5), c(0.4, 0.4, 0.2))
  mockery::stub(post_prob_from_sim, "stats::update", 1)
  mockery::stub(post_prob_from_sim, "brms::do_call", m)
  pmp_sim_2 <- pmp_sim_1
  pmp_sim_2[1, c("pmp1", "pmp2", "pmp3")] <- c(0.5, 0.3, 0.2)
  pmp_sim_2[2, c("pmp1", "pmp2", "pmp3")] <- c(0.3, 0.2, 0.5)
  pmp_sim_2[3, c("pmp1", "pmp2", "pmp3")] <- c(0.4, 0.4, 0.2)
  pmp_sim_2$pmp <- with(pmp_sim_2, as.matrix(pmp_sim_2[, colnames(pmp_sim_2)[3:ncol(pmp_sim_2)]]))
  expect_equal(post_prob_from_sim(brmsfit_list, pmp_sim_1, 3, n_sim, simulate_data_matrix, suppress_mwo, verbosity=0), pmp_sim_2)
})
