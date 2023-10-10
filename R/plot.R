#' Plot simulated posterior model probability in ternary plot.
#' @param meta_uncertainty_fit meta_uncertainty_fit object created by \code{\link{meta_uncertainty}}. Only the fit with three models is supported.
#' @returns Ternary plot showing simulated posterior model probability from three true model.
#' @export
plot_simulated_pmp <- function(meta_uncertainty_fit){
  stopifnot(is.meta_uncertainty_fit(meta_uncertainty_fit))

  pmp_sim <- meta_uncertainty_fit$pmp_sim
  stopifnot(ncol(pmp_sim) == 6)
  pmp_sim$true_model <- factor(pmp_sim$true_model_idx, levels = c(1,2,3),
                               labels = c(latex2exp::TeX("$M_*=M_1$"),
                                          latex2exp::TeX("$M_*=M_2$"),
                                          latex2exp::TeX("$M_*=M_3$")
                               )
  )

  ggplot(pmp_sim, aes(pmp = ggsimplex::make_list_column(pmp1, pmp2, pmp3))) +
    coord_fixed(ratio = 1, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)) +
    theme_void() +
    ggsimplex::geom_simplex_point(size=1) +
    ggsimplex::geom_simplex_canvas() +
    facet_grid(~ true_model, labeller = label_parsed) +
    theme(strip.text = element_text(size = 18))
}

#' Plot simulated posterior model probability in ternary plot.
#' @param meta_uncertainty_fit meta_uncertainty_fit object created by \code{\link{meta_uncertainty}}.  Only the fit with three models is supported.
#' @returns Ternary plot
#' @export
plot_meta_model_density <- function(meta_uncertainty_fit){
  stopifnot(is.meta_uncertainty_fit(meta_uncertainty_fit))
  pmp_sim <- meta_uncertainty_fit$pmp_sim
  stopifnot(ncol(pmp_sim) == 6)
  pmp_sim$true_model <- factor(pmp_sim$true_model_idx, levels = c(1,2,3),
                               labels = c(latex2exp::TeX("$M_*=M_1$"),
                                          latex2exp::TeX("$M_*=M_2$"),
                                          latex2exp::TeX("$M_*=M_3$")
                               )
  )
  meta_model_param <- meta_uncertainty_fit$meta_model_param
  meta_model_param$true_model <- factor(meta_model_param$true_model_idx, levels = c(1,2,3),
                                        labels = c(latex2exp::TeX("$M_*=M_1$"),
                                                   latex2exp::TeX("$M_*=M_2$"),
                                                   latex2exp::TeX("$M_*=M_3$")
                                        )
  )

  ggplot() +
    coord_fixed(ratio = 1, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)) +
    theme_void() +
    ggsimplex::geom_simplex_canvas(fontsize=11)  +
    ggsimplex::stat_simplex_density(
      data = meta_model_param,
      fun = brms::dlogistic_normal,
      args = alist(mu = mu, Sigma = Sigma),
      col_scale = 4
    ) +
    ggsimplex::geom_simplex_point(data = pmp_sim,
                                  aes(pmp=ggsimplex::make_list_column(pmp1, pmp2, pmp3)),
                                  alpha = 1.0) +
    facet_grid(cols=vars(true_model), labeller = label_parsed) +
    theme(strip.text = element_text(size = 18))
}
#' Plot simulated posterior model probability in ternary plot.
#' @param meta_uncertainty_fit meta_uncertainty_fit object created by \code{\link{meta_uncertainty}}.  Only the fit with three models is supported.
#' @returns Ternary plot
#' @export
plot_predictive_mixture <- function(meta_uncertainty_fit){
  stopifnot(is.meta_uncertainty_fit(meta_uncertainty_fit))
  pmp_obs <- meta_uncertainty_fit$pmp_obs
  stopifnot(length(pmp_obs) == 3)

  pmp_obs_cartesian <- simplex_3d_to_cartesian_2d(pmp_obs)
  pmp_obs_df <- data.frame(pmp1 = pmp_obs[1],
                           pmp2 = pmp_obs[2],
                           pmp3 = pmp_obs[3]
  )
  mixture_function <- meta_uncertainty_fit$mixture_function

  ggplot() +
    coord_fixed(ratio = 1, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)) +
    theme_void() +
    ggsimplex::stat_simplex_density(
      data = data.frame(dummy=0),
      fun = mixture_function,
      args = alist(dummy=dummy),
      col_scale = 4
    ) +
    ggsimplex::geom_simplex_point(data = pmp_obs_df, aes(pmp = ggsimplex::make_list_column(pmp1, pmp2, pmp3)),
                                  size = 1.7, shape=21, colour = "white", alpha = 1.0) +
    ggsimplex::geom_simplex_point(data = pmp_obs_df, aes(pmp = ggsimplex::make_list_column(pmp1, pmp2, pmp3)),
                                  size = 1.5, shape=16, colour = "magenta", alpha = 1.0) +
    annotate(
      geom = "curve", x = 0.1, y = 0.5, xend = pmp_obs_cartesian[1]-0.02, yend = pmp_obs_cartesian[2]+0.02,
      curvature = .3, arrow = arrow(length = unit(2, "mm"))
    ) +
    annotate(geom = "text", x = 0.1, y = 0.51, label = latex2exp::TeX(r'($\mathring{\pi})'), hjust = "center", vjust="bottom", size=10) +
    ggsimplex::geom_simplex_canvas(fontsize=20)
}

simplex_3d_to_cartesian_2d <- function(simplex_pos){
  return(simplex_pos %*% matrix(c(0, 0, 1, 0, 0.5, sqrt(3)/2), byrow=TRUE, ncol=2))
}
