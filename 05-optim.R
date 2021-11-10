library(ggplot2)
library(fst)

fn_dumb <- function(x) 4*sin(x) + (x-4)^2

(x <- seq(-100, 100, .1))
(y <- fn_dumb(x))
qplot(x, y, geom = 'line')

optim(par = c(x = 0), fn_dumb)
optim(par = c(x = 0), fn_dumb, method = 'BFGS')
optimise(fn_dumb, interval = c(-100, 100))
(x_min <- x[which.min(y)])
fn_dumb(x_min)
fn_dumb(4.473414)
fn_dumb(4.473409)

#### Least Squares Optimisation ####
df <- read_fst('data/taiwan_real_estate2.fst')

sum_of_squares <- function(coeffs, x = 1:10, y = 4 + 1.5*x) {
  intercept = coeffs[[1]]
  slope = coeffs[[2]]
  
  sum((y - (intercept + slope*x))^2)
}

optim(
  par = c(intercept = 0, slope = 1),
  sum_of_squares,
  method = 'BFGS'
)
## Cool

ols_args <- optim(
  par = c(intercept = 10, slope = 1),
  sum_of_squares,
  x = df$n_convenience,
  y = df$price_twd_msq,
  method = 'BFGS',
  control = list(trace = 6L, REPORT = 1L)
)
# The BFGS method performs better
## fun gif here: https://github.com/jiupinjia/Visualize-Optimization-Algorithms#:~:text=Broyden%20Fletcher%20Goldfarb%20Shanno%20(BFGS).
ols_args
ols_args$par

lm(price_twd_msq ~ n_convenience, df)
## Match!