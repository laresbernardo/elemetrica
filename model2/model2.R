# Global functions
source("scripts/funs.R")
# Define model parameters
source("scripts/params.R")

# Read the data
cats <- read_data(type = params$type)
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
                      splitcharnums = params$splitcharnums,
                      stop_words = STOP_WORDS)

# Create word2vector model
w2v.model <- h2o_word2vec(words, 
                          epochs = params$w2v.epochs, 
                          window_size = params$w2v.window, 
                          vec_size = params$w2v.vecs) 

# Prepare training & validation data (keep only labels made of known words)
data <- df %>% bind_cols(as.data.frame(w2v.model$vectors)) %>% filter(!is.na(.data$C1))
head(data)

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
  plots = FALSE
); stop <- toc("h2o_automl", quiet = TRUE)

# Select model object
model <- modelx
# Show and save performance results
scores <- clean_predict(model$scores_test %>% rename(predict = score, label = tag), top = 1)
params[["model_name"]] <- model$model_name
params[["results"]] <- model$metrics$metrics
params[["train_time"]] <- round(stop$time/60, 2)
params[["acc_cv"]] <- model$model@model$cross_validation_metrics_summary$mean[1]
params[["err_cv"]] <- model$model@model$cross_validation_metrics_summary$mean[7]
params[["mean_prob"]] <- mean(scores$probability)
print(model$model@model$cross_validation_metrics_summary[,1:2])

# (Brag) show results!
save_log(params, save = params$save, print = TRUE) 

############ EXPORT MODELS
export_results(
  model, thresh = 1000, subdir = "MOJOs",
  which = c("txt", "mojo"))
export_results(
  w2v.model$model, thresh = 1000, subdir = "MOJOs",
  which = c("mojo"))

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
# h2o_word2vec.predict("tubo pvc", w2v.model$model, model$model, params = params, clean = TRUE)
# h2o_word2vec.predict("tubo cpvc", w2v.model$model, model$model, params = params, clean = TRUE)
# h2o_word2vec.predict("extension electrica", w2v.model$model, model$model, params = params, clean = TRUE)
# h2o_word2vec.predict("botas de seguridad", w2v.model$model, model$model, params = params, clean = TRUE)
# h2o_word2vec.predict("martillo", w2v.model$model, model$model, params = params, clean = TRUE)
# h2o_word2vec.predict("lija", w2v.model$model, model$model, params = params, clean = TRUE)
# h2o_word2vec.predict("tijera", w2v.model$model, model$model, params = params, clean = TRUE)
# h2o_word2vec.predict("cerradura", w2v.model$model, model$model, params = params, clean = TRUE)
# 
# model$scores_test %>%
#   mutate(correct = tag == score) %>%
#   freqs(correct, tag, score) %>% 
#   clean_label(c("tag","score")) %>% 
#   head(20)
#
# # Clusters
# lares::clusterVisualK(data, 3) # {~20%}
