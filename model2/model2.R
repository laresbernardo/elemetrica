source("scripts/funs.R")

params <- list(
  # How many levels to include in the chain?
  level = 4, 
  # Minimum products per category allowed (default: 10)
  min_per_cat = 10, 
  # Tokenize options
  dropnumwords = FALSE,
  minchar = 1,
  # word2vec options
  w2v.epochs = 16,
  w2v.window = 6,
  w2v.vecs = 100, 
  # modeling options
  models = 1, # How many models to train and select the best?
  sample_p = 0.7, # Percentage of the data to use (default: 1)
  train_p = 0.7, # Test size for tuning parameters (default: 0.7)
  exclude_algos = NULL, # Exclude algos (default: c("StackedEnsemble","DeepLearning"))
  include_algos = "DRF", # Include algos
  save = TRUE # Save results into CSV
) 

# Read the data
cats <- read_data()
# Prepare dataset
df <- prepare_data(cats, 
                   level = params$level, 
                   sample_p = params$sample_p,
                   minimum = params$min_per_cat)
# Print summary and most frequents
params[["data"]] <- summary_data(df, cats)
print(params$data)

# Tokenize: Break labels into sequence of words
words <- h2o_tokenize(df$product, 
                      dropnumwords = params$dropnumwords, 
                      minchar = params$minchar, 
                      stop_words = STOP_WORDS)

# Create word2vector model
w2v.model <- h2o_word2vec(words, 
                          epochs = params$w2v.epochs, 
                          window_size = params$w2v.window, 
                          vec_size = params$w2v.vecs) 

# Prepare training & validation data (keep only labels made of known words)
data <- df %>% bind_cols(as.data.frame(w2v.model$vectors)) %>% filter(!is.na(.data$C1))

# Train model
modelx <- h2o_automl(
  data, 
  y = "category", 
  ignore = c("product", "chain", "source"),
  max_models = params$models,
  split = params$train_p,
  exclude_algos = params$exclude_algos,
  include_algos = params$include_algos,
  thresh = 1000, 
  start_clean = FALSE,
  plots = FALSE)

# Select model object
model <- modelx
# Show and save performance results
params[["model_name"]] <- model$model_name
params[["results"]] <- model$metrics$metrics
print(model$model@model$cross_validation_metrics_summary[,1:2])

# (Brag) show results!
save_log(params, save = params$save, print = TRUE) 


#############################################
# TEST ZONE
#############################################

# # Cumulative predictions accuracy
# mplot_topcats(model$scores_test$tag,
#               model$scores_test$score,
#               multis = subset(model$scores_test, select = -c(tag, score)),
#               model_name = model$model_name)
# 
# # Metrics by label (test)
# model$metrics$metrics_tags %>% clean_label("tag")
# 
# # Check some labeled results
# model$scores_test %>% 
#   mutate(correct = tag == score,
#          product = model$datasets$test$product) %>%
#   select(correct, product, tag, score) %>% 
#   group_by(correct) %>% sample_n(10) %>% 
#   clean_label(c("product","tag","score"))
# 
# # Manual testings
# sample_n(df, 10) %>% clean_label(c("product","category"))
# h2o_word2vec.predict("tubo pvc", w2v.model$model, model$model, clean = TRUE)
# h2o_word2vec.predict("tubo cpvc", w2v.model$model, model$model, clean = TRUE)
# h2o_word2vec.predict("extension electrica", w2v.model$model, model$model, clean = TRUE)
# h2o_word2vec.predict("botas de seguridad", w2v.model$model, model$model, clean = TRUE)
# h2o_word2vec.predict("martillo", w2v.model$model, model$model, clean = TRUE)
# h2o_word2vec.predict("lija", w2v.model$model, model$model, clean = TRUE)
# h2o_word2vec.predict("tijera", w2v.model$model, model$model, clean = TRUE)
# h2o_word2vec.predict("cerradura", w2v.model$model, model$model, clean = TRUE)
# 
# model$scores_test %>%
#   mutate(correct = tag == score) %>%
#   freqs(correct, tag, score) %>% 
#   clean_label(c("tag","score")) %>% 
#   head(20)
#
# # Clusters
# lares::clusterVisualK(data, 3) # {~20%}
