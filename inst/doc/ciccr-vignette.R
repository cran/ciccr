## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ciccr)

## -----------------------------------------------------------------------------
  y = ACS$topincome
  t = ACS$baplus
  x = ACS$age

## -----------------------------------------------------------------------------
  x = splines::bs(x, df = 6)

## -----------------------------------------------------------------------------
  results_case = avg_retro_logit(y, t, x, 'case')
  results_case$est
  results_case$se

## -----------------------------------------------------------------------------
  results_control = avg_retro_logit(y, t, x, 'control')
  results_control$est
  results_control$se

## -----------------------------------------------------------------------------
results = cicc(y, t, x, 0.2, 0.95)

## -----------------------------------------------------------------------------
 # point estimates
 results$est
 # standard errors
 results$se
 # confidence intervals 
 results$ci
 # grid points from 0 to p_upper 
 results$pseq

## -----------------------------------------------------------------------------
 # point estimate
 exp(results$est)
 # confidence interval estimate
 exp(results$ci)

## -----------------------------------------------------------------------------
  yaxis_limit = c(min(exp(results$est)),(max(exp(results$ci))+0.25*(max(exp(results$ci))-min(exp(results$est)))))
  plot(results$pseq, exp(results$est), type = "l", lty = "solid", col = "blue", xlab = "Pr(Y*=1)",ylab = "exp(change in log probability)", xlim = c(0,max(results$pseq)), ylim = yaxis_limit)
  lines(results$pseq, exp(results$ci), type = "l", lty = "dashed", col = "red")

## -----------------------------------------------------------------------------
logit = stats::glm(y~t+x, family=stats::binomial("logit"))
est_logit = stats::coef(logit)
ci_logit = stats::confint(logit, level = 0.9)
# point estimate
exp(est_logit)
# confidence interval
exp(ci_logit)


