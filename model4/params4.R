set.seed(0)

# Model parameters
params <- list(
  # Data sources
  type = 4,  # Default/latest: 3
  refresh = FALSE, # Consolidate data using GSheets data?
  # How many levels to include in the chain?
  level = 4,  # Default: 4
  # Minimum products per category allowed (default: 10)
  min_per_cat = 10,  # Default: 10
  # Test sheet settings
  train_ts = FALSE, # Default: FALSE
  # Tokenize options
  dropnumwords = FALSE, # Default: FALSE
  minchar = 1, # Default: 1
  splitcharnums = TRUE, # Default: FALSE
  add_units = FALSE, # Adds regex cols for units
  # word2vec options
  w2v.epochs = 16, # Default: 16
  w2v.window = 6, # Default: 6
  w2v.vecs = 100,  # Default: 100
  # modeling options
  models = 1, # How many models to train and select the best?
  sample_p = 1, # Percentage of the data to use (default: 1)
  train_p = 1, # Test size for tuning parameters (default: 0.7)
  nfolds = 5, # Cross-validation folds (default: 5)
  exclude_algos = NULL, # Exclude algos (default: c("StackedEnsemble","DeepLearning"))
  include_algos = "DRF", # Include algos
  save = TRUE, # Save results into CSV
  export = FALSE
) 
