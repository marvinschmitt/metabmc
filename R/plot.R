#' Plot simulated posterior model probability.
#' @param metabmcfit metabmcfit object created by \code{\link{metabmc}}. Only the fit with three models is supported.
#' @returns Three triangle plot showing simulated posterior model probability from three true models.
#' @examples \dontrun{
#' plot_simulated_pmp(metabmc_fit)
#' }
#' @export
plot_simulated_pmp <- function(metabmcfit){
  stopifnot(is.metabmcfit(metabmcfit))

  pmp_sim <- metabmcfit$pmp_sim
  stopifnot(ncol(pmp_sim) == 6)
  pmp_sim$true_model <- factor(pmp_sim$true_model_idx, levels = c(1,2,3),
                               labels = c(latex2exp::TeX("$M_*=M_1$"),
                                          latex2exp::TeX("$M_*=M_2$"),
                                          latex2exp::TeX("$M_*=M_3$")
                               )
  )

  ggplot2::ggplot(pmp_sim, ggplot2::aes(pmp = ggsimplex::make_list_column(pmp1, pmp2, pmp3))) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)) +
    ggplot2::theme_void() +
    ggsimplex::geom_simplex_point(size=1) +
    ggsimplex::geom_simplex_canvas() +
    ggplot2::facet_grid(~ true_model, labeller = ggplot2::label_parsed) +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 18))
}

#' Plot density of metamodels
#' @param metabmcfit metabmcfit object created by \code{\link{metabmc}}.  Only the fit with three models is supported.
#' @returns Three triangle plot of density of meta models (Distribution of posterior model probability)
#' @examples \dontrun{
#' plot_meta_model_density(metabmc_fit)
#' }
#' @export
plot_meta_model_density <- function(metabmcfit){
  stopifnot(is.metabmcfit(metabmcfit))
  pmp_sim <- metabmcfit$pmp_sim
  stopifnot(ncol(pmp_sim) == 6)
  pmp_sim$true_model <- factor(pmp_sim$true_model_idx, levels = c(1,2,3),
                               labels = c(latex2exp::TeX("$M_*=M_1$"),
                                          latex2exp::TeX("$M_*=M_2$"),
                                          latex2exp::TeX("$M_*=M_3$")
                               )
  )
  meta_model_param <- metabmcfit$meta_model_param
  meta_model_param$true_model <- factor(meta_model_param$true_model_idx, levels = c(1,2,3),
                                        labels = c(latex2exp::TeX("$M_*=M_1$"),
                                                   latex2exp::TeX("$M_*=M_2$"),
                                                   latex2exp::TeX("$M_*=M_3$")
                                        )
  )

  ggplot2::ggplot() +
    ggplot2::coord_fixed(ratio = 1, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)) +
    ggplot2::theme_void() +
    ggsimplex::geom_simplex_canvas(fontsize=11)  +
    ggsimplex::stat_simplex_density(
      data = meta_model_param,
      fun = brms::dlogistic_normal,
      args = alist(mu = mu, Sigma = Sigma),
      col_scale = 4
    ) +
    ggsimplex::geom_simplex_point(data = pmp_sim,
                                  ggplot2::aes(pmp=ggsimplex::make_list_column(pmp1, pmp2, pmp3)),
                                  alpha = 1.0) +
    ggplot2::facet_grid(cols=vars(true_model), labeller = label_parsed) +
    ggplot2::theme(strip.text = element_text(size = 18))
}

#' Plot predictive mixture model
#' @param metabmcfit metabmcfit object created by \code{\link{metabmc}}. Only the fit with three models is supported.
#' @returns Single ternary plot of density of predictive mixture model along with observed PMPs.
#' @examples \dontrun{
#' plot_predictive_mixture(metabmc_fit)
#' }
#' @export
plot_predictive_mixture <- function(metabmcfit){
  stopifnot(is.metabmcfit(metabmcfit))
  pmp_obs <- metabmcfit$pmp_obs
  stopifnot(length(pmp_obs) == 3)

  pmp_obs_cartesian <- simplex_3d_to_cartesian_2d(pmp_obs)
  pmp_obs_df <- data.frame(pmp1 = pmp_obs[1],
                           pmp2 = pmp_obs[2],
                           pmp3 = pmp_obs[3]
  )
  mixture_function <- metabmcfit$mixture_function

  ggplot2::ggplot() +
    ggplot2::coord_fixed(ratio = 1, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)) +
    ggplot2::theme_void() +
    ggsimplex::stat_simplex_density(
      data = data.frame(dummy=0),
      fun = mixture_function,
      args = alist(dummy=dummy),
      col_scale = 4
    ) +
    ggsimplex::geom_simplex_point(data = pmp_obs_df, ggplot2::aes(pmp = ggsimplex::make_list_column(pmp1, pmp2, pmp3)),
                                  size = 1.7, shape=21, colour = "white", alpha = 1.0) +
    ggsimplex::geom_simplex_point(data = pmp_obs_df, ggplot2::aes(pmp = ggsimplex::make_list_column(pmp1, pmp2, pmp3)),
                                  size = 1.5, shape=16, colour = "magenta", alpha = 1.0) +
    ggplot2::annotate(
      geom = "curve", x = 0.1, y = 0.5, xend = pmp_obs_cartesian[1]-0.02, yend = pmp_obs_cartesian[2]+0.02,
      curvature = .3, arrow = ggplot2::arrow(length = unit(2, "mm"))
    ) +
    ggplot2::annotate(geom = "text", x = 0.1, y = 0.51, label = latex2exp::TeX(r"($\mathring{\pi}$)", output = "character"), hjust = "center", vjust="bottom", size=10, parse = TRUE
) +
    ggsimplex::geom_simplex_canvas(fontsize=20)
}

# Convert position in 3D simplex to position in cartesian coordinate
# @param simplex_pos vector with size 3
# @returns vector with size 2
simplex_3d_to_cartesian_2d <- function(simplex_pos){
  return(simplex_pos %*% matrix(c(0, 0, 1, 0, 0.5, sqrt(3)/2), byrow=TRUE, ncol=2))
}
