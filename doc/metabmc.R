params <-
list(EVAL = FALSE)

## ---- SETTINGS-knitr, include=FALSE-----------------------------------------------------
stopifnot(require(knitr))
options(width = 90)
opts_chunk$set(
  comment = NA,
  message = FALSE,
  warning = FALSE,
  dev = "jpeg",
  dpi = 100,
  fig.asp = 0.8,
  fig.width = 5,
  out.width = "60%",
  fig.align = "center"
)
library(metabmc)
library(brms)
library(ggplot2)
ggplot2::theme_set(theme_default())
data("LakeHuron")
LakeHuron <- LakeHuron - mean(LakeHuron)

## ----echo=TRUE--------------------------------------------------------------------------
plot(LakeHuron, type='l')

## ---- results='hide'--------------------------------------------------------------------
m1 <- bf(x ~ 0 + arma(p=1, q=0))
m2 <- bf(x ~ 0 + arma(p=0, q=1))
m3 <- bf(x ~ 0 + arma(p=1, q=1))

prior1 <- c(set_prior("normal(0, .2)", class = "ar"),
           set_prior("normal(0, .2)", class = "sigma"))

prior2 <- c(set_prior("normal(0, .2)", class = "ma"),
           set_prior("normal(0, .2)", class = "sigma"))

prior3 <- c(set_prior("normal(0, .2)", class = "ar"),
           set_prior("normal(0, .2)", class = "ma"),
           set_prior("normal(0, .2)", class = "sigma"))

fit1 <- brm(m1, data=LakeHuron, prior = prior1, save_pars = save_pars(all = TRUE))
fit2 <- brm(m2, data=LakeHuron, prior = prior2, save_pars = save_pars(all = TRUE))
fit3 <- brm(m3, data=LakeHuron, prior = prior3, save_pars = save_pars(all = TRUE))

## ---- results='hide'--------------------------------------------------------------------
metabmc_fit <- metabmc(fit1, fit2, fit3, n_sim=20)

## ----eval=FALSE-------------------------------------------------------------------------
#  metabmc_fit <- metabmc(list(m1, m2, m3), list(prior1, prior2, prior3), data = LakeHuron, n_sim=20)

## ---------------------------------------------------------------------------------------
plot_simulated_pmp(metabmc_fit)

## ---------------------------------------------------------------------------------------
plot_meta_model_density(metabmc_fit)

## ---- warning=FALSE---------------------------------------------------------------------
plot_predictive_mixture(metabmc_fit)

