test_that("level_3 works", {
  pmp_pbs <- c(0.4, 0.4, 0.2)
  pmp_sim_1 <- data.frame(
    k = 1:3,
    true_model_idx = c(2, 1, 3),
    pmp1 = rep(NA, 3),
    pmp2 = rep(NA, 3),
    pmp3 = rep(NA, 3)
  )
  pmp_sim_1[1, c("pmp1", "pmp2", "pmp3")] <- c(0.5, 0.3, 0.2)
  pmp_sim_1[2, c("pmp1", "pmp2", "pmp3")] <- c(0.3, 0.2, 0.5)
  pmp_sim_1[3, c("pmp1", "pmp2", "pmp3")] <- c(0.4, 0.4, 0.2)
  pmp_sim_2 <- pmp_sim_1
  pmp_sim_2$pmp <- with(pmp_sim_1, as.matrix(pmp_sim_1[, colnames(pmp_sim_1)[3:ncol(pmp_sim_1)]]))

  mockery::stub(level_3, "get_meta_model_posteriors", "mock_meta_model_posterior")
  mockery::stub(level_3, "extract_meta_model_param", "mock_meta_model_parameter")
  mockery::stub(level_3, "create_mixture_function", "mock_mixture_function")

  expected_out <- list(pmp_sim = pmp_sim_2, simulated_data_matrix = NULL, meta_model_param = "mock_meta_model_parameter", pmp_obs = pmp_pbs, mixture_function = "mock_mixture_function")
  class(expected_out) <- "meta_uncertainty_fit"
  out <- level_3(pmp_pbs, pmp_sim_1)
  expect_equal(out, expected_out)
})
