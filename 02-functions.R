rt4 <- function(x) x^.25

# A list of inverse functions
inverse_list <- list(
  'I' = I,
  'log10' = \(x) 10^x,
  'log2'  = \(x) 2^x,
  'sqrt'  = \(x) x^2,
  'rt4'   = \(x) x^4
)

lm_transform = function(
  equation,
  data,
  transform = c('I', 'log10'),
  n_predictions = 100
) {
  stopifnot(length(equation) == 3)
  stopifnot(!length(setdiff(transform, c('I', 'log10', 'sqrt', 'log2', 'rt4'))))
  
  require(broom)
  require(dplyr)
  
  # transform <- match.arg(transform)
  y_transform <- transform[1]
  x_transform <- last(transform)
  y_inverse   <- inverse_list[[y_transform]]
  xvar <- last(as.character(equation[[3]]))
  yvar <- as.character(equation[[2]])
  xvar_transformed <- paste0(x_transform, '(', xvar, ')')
  yvar_transformed <- paste0(y_transform, '(', yvar, ')')
  x <- data[[xvar]]
  
  equation <- equation %>%
    as.character() %>%
    gsub(pattern = yvar, replacement = yvar_transformed, .) %>%
    gsub(pattern = xvar, replacement = xvar_transformed, .) %>%
    `[`(c(2, 1, 3)) %>%
    paste(collapse = ' ') %>%
    as.formula()
  
  mod <- lm(equation, data = data)
  
  explanatory_data <- tibble(
    x = seq(from = min(x), to = max(x), length.out = n_predictions)
  ) %>%
    setNames(xvar)
  
  prediction_data <- explanatory_data %>%
    mutate(
      y_transformed = predict(mod, newdata = explanatory_data),
      y = y_inverse(y_transformed)
    ) %>%
    setNames(c(xvar, yvar_transformed, yvar))
  
  list(
    'model' = mod,
    'coefficients' = tidy(mod),
    'quality'      = glance(mod),
    'predictions'  = prediction_data
  )
}

seq_range <- function(x, na.rm = TRUE, ...) {
  seq.int(from = min(x, na.rm = na.rm), to = max(x, na.rm = na.rm), ...)
}

# by user20650 from StackOverflow
## https://stackoverflow.com/questions/45873483/ggpairs-plot-with-heatmap-of-correlation-values/53685979#53685979
cor_heatmap <- function(
  data,
  mapping,
  method = 'p',
  use = 'pairwise',
  cor_min = -1,
  ...
) {
  
  # grab data
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  # calculate correlation
  corr <- cor(x, y, method = method, use = use)
  
  # calculate colour based on correlation value
  # Here I have set a correlation of minus one to blue, 
  # zero to white, and one to red 
  # Change this to suit: possibly extend to add as an argument of `my_fn`
  colFn <- colorRampPalette(c('blue', 'white', 'red'), interpolate = 'spline')
  fill <- colFn(1000)[findInterval(corr, seq(cor_min, 1, length = 1000))]
  
  ggally_cor(data = data, mapping = mapping, colour = '#666666', ...) + 
    theme_void() +
    theme(panel.background = element_rect(fill = fill))
}
