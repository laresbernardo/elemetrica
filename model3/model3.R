# Global functions
source("scripts/funs.R")
# Define model parameters
source("scripts/params3.R")

# Read the data
cats <- read_data(type = params$type)
freqs(cats, source)

# Prepare dataset
df <- prepare_data(cats, 
                   level = params$level, 
                   sample_p = params$sample_p,
                   minimum = params$min_per_cat,
                   add_units = params$add_units)

# Print summary and most frequents
params[["data"]] <- summary_data(df, cats, level = params$level)
print(params$data)

# Clean labels text
txt <- clean_product(df$product, 
                     stop_words = STOP_WORDS,
                     minchar = params$minchar,
                     dropnumwords = params$dropnumwords,
                     splitcharnums = params$splitcharnums)

# Create word2vector model
w2v <- word2vec(txt, 
                type = "cbow", # skip-gram (slower, better for infrequent words) vs cbow (fast)
                window = params$w2v.window, # for skip-gram usually around 10, for cbow around 5
                dim = params$w2v.vecs, # usually more is better, but not always
                stopwords = STOP_WORDS,
                min_count = 3,
                iter = params$w2v.epochs)

# Prepare training & validation data (keep only labels made of known words)
data <- df %>% 
  bind_cols(
  suppressWarnings(doc2vec(w2v, txt)) %>% as_tibble() %>%
  magrittr::set_names(paste0("C", 1:w2v$control$dim))) %>%
  filter(!is.na(.data$C1))
print(head(data))

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
scores <- model$scores_test %>% 
  rename(predict = score, label = tag) %>%
  clean_predict(top = 1) %>%
  mutate(product = model$datasets$test$product) %>%
  select(id, product, label, category, probability) %>%
  clean_label() %>% as_tibble() %>% 
  mutate(correct = label == category)
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
if (params$export) {
  export_results(
    model, thresh = 1000, subdir = "MOJOs",
    which = c("txt", "mojo"))
  write.word2vec(w2v, "W2V/w2v.bin", type = "bin", encoding = "UTF-8")
  write.word2vec(w2v, "W2V/w2v.txt", type = "txt", encoding = "UTF-8")
}
