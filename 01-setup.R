required_packages <- c(
  # Reading data
  'readr',
  'fst',
  # Data tidying and plotting
  'tidyr',
  'dplyr',
  'broom',
  'ggplot2',
  'GGally',
  'ggfortify',
  # Additional regression functions
  'glmnet',
  'infer',
  # stats::step(),
  'StepReg',
  'olsrr'
)
(missing_packages <- setdiff(required_packages, installed.packages()))
if (length(missing_packages)) install.packages(missing_packages)
rm(required_packages, missing_packages)
