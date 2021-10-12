# QQ plots from scratch
library(ggplot2)
library(dplyr)

x <- rpois(100, 13)
x <- rpois(100, 3)
x <- runif(100)
x <- rchisq(100, 2)
xs <- scale(x)
qplot(x, geom = 'density')
qplot(xs, geom = 'density')

X <- tibble(
  p = seq.int(.01, 1, by = .01),
  theoretical = qnorm(p),
  sample = quantile(xs, p)
)
X

ggplot(X, aes(x = theoretical, y = sample)) +
  geom_point() +
  coord_fixed() +
  geom_abline(lty = 'dashed')
