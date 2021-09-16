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
