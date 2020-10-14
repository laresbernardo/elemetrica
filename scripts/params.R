# Model parameters
params <- list(
  # Data sources
  type = 3,  # Default/latest: 3
  # How many levels to include in the chain?
  level = 4,  # Default: 4
  # Minimum products per category allowed (default: 10)
  min_per_cat = 10,  # Default: 10
  # Tokenize options
  dropnumwords = FALSE, # Default: FALSE
  minchar = 1, # Default: 1
  splitcharnums = FALSE, # Default: FALSE
  # word2vec options
  w2v.epochs = 16, # Default: 16
  w2v.window = 6, # Default: 6
  w2v.vecs = 50,  # Default: 100
  # modeling options
  models = 1, # How many models to train and select the best?
  sample_p = 1, # Percentage of the data to use (default: 1)
  train_p = 0.7, # Test size for tuning parameters (default: 0.7)
  exclude_algos = NULL, # Exclude algos (default: c("StackedEnsemble","DeepLearning"))
  include_algos = "DRF", # Include algos
  save = TRUE # Save results into CSV
) 
