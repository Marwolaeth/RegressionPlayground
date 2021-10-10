# Q-Q plots from scratch
library(ggplot2)
library(dplyr)

x <- rpois(100, 13)
x <- rpois(100, 3)
x <- runif(100)
xs <- scale(x)
qplot(x, geom = 'density')
qplot(xs, geom = 'density')

X <- tibble(
  p = seq.int(.01, 1, by = .01),
  theoretical = qnorm(p),
  empyrical = quantile(xs, p)
)
X

ggplot(X, aes(x = theoretical, y = empyrical)) +
  geom_point() +
  coord_fixed() +
  geom_line(aes(y = theoretical), lty = 'dashed')
