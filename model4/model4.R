# Global functions
source("scripts/funs.R")
# Define model parameters
source("model4/params4.R")

# Read the data
cats <- read_data(type = params$type, refresh = params$refresh)
freqs(cats, source)

# # Most frequent n-grams
# ngram <- ngrams(cats$product, ngram = 2:4, stop_words = c(1:100, "x"), exclude = "\\/")
# ngram %>%
#   ggplot(aes(x = reorder(comb, n), y = n, fill = ngram)) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ngram, ncol=unique(.data$ngram), scales = "free") +
#   labs(y = NULL, x = NULL, title = "Most Frequent N-Grams") +
#   coord_flip() + theme_lares()

# Prepare dataset
df <- prepare_data(cats, 
                   level = params$level, 
                   sample_p = params$sample_p,
                   minimum = params$min_per_cat,
                   add_units = params$add_units)
# Identify test sheet products
weights_df <- test_sheets(refresh = params$refresh)
ts_sheet <- test_sheets_data(df, weights_df, min = 5, max = 40)
df$ts_sheet <- paste(df$country, df$product) %in% paste(ts_sheet$country, ts_sheet$product)

# Print summary and most frequents
params[["data"]] <- summary_data(df, cats, level = params$level, train_ts = params$train_ts)
print(params$data)

# Clean labels text
txt <- clean_product(df$product, 
                     stop_words = STOP_WORDS,
                     minchar = params$minchar,
                     dropnumwords = params$dropnumwords,
                     splitcharnums = params$splitcharnums)

# # Most frequent words
# words <- lares::textTokenizer(txt, remove_numbers = params$dropnumwords); words

# Create word2vector model
w2v <- word2vec(txt, 
                type = "cbow", # skip-gram (slower, better for infrequent words) vs cbow (fast)
                window = params$w2v.window, # for skip-gram usually around 10, for cbow around 5
                dim = params$w2v.vecs, # usually more is better, but not always
                stopwords = STOP_WORDS,
                min_count = 3,
                iter = params$w2v.epochs)

# Prepare training & validation data (keep only labels made of known words)
vctrs <- suppressWarnings(doc2vec(w2v, txt)) %>% 
  as_tibble() %>% magrittr::set_names(paste0("C", 1:w2v$control$dim))
data <- df %>% bind_cols(vctrs) %>% filter(!is.na(.data$C1))
print(head(data))
dim(vctrs)

# Train model (manual)
processed <- model_preprocess(
  data, y = "category", 
  split = params$train_p, 
  thresh = 1000)
training_frame <- processed$data
if (!params$train_ts) 
  training_frame <- training_frame[!processed$data$ts_sheet,]
# h2o::h2o.xgboost / h2o::h2o.randomForest
tic("h2o.randomForest", quiet = TRUE)
manual <- h2o.randomForest(
  training_frame = quiet(as.h2o(
    select(training_frame, -product, -chain, -source, -ts_sheet))),
  y = "tag",
  #stopping_metric = "misclassification",
  stopping_rounds = 5,
  ntrees = 50,
  keep_cross_validation_models = FALSE,
  nfolds = params$nfolds
  ); stop <- toc("h2o.randomForest", quiet = TRUE)
modelx <- h2o_results(
  manual, 
  test = if (params$train_p == 1) processed$data else processed$data[-processed$train_index,], 
  train = if (params$train_p == 1) processed$data else processed$data[processed$train_index,], 
  model_type = processed$model_type,
  plots = FALSE)

# # Train model (automl)
# modelx <- h2o_automl(
#   data, 
#   y = "category", 
#   ignore = c("product", "chain", "source", "ts_sheet"),
#   max_models = params$models,
#   split = params$train_p,
#   nfolds = params$nfolds,
#   exclude_algos = params$exclude_algos,
#   include_algos = params$include_algos,
#   thresh = 1000, 
#   start_clean = FALSE,
#   plots = FALSE
# ); stop <- toc("h2o_automl", quiet = TRUE)

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
print(model$metrics$cv_metrics)
print(model$metrics$hit_ratio)
ggplot(scores, aes(x=probability, fill = correct)) + 
  geom_density(alpha = 0.5) + theme_lares(pal=1)

# (Brag) show results!
save_log(params, save = params$save, print = TRUE, dir = paste0("model", params$type))

############ EXPORT MODELS
if (params$export) {
  export_results(
    model, thresh = 1000, subdir = "H2O",
    which = c("txt", "mojo"))
  write.word2vec(w2v, "W2V/w2v.bin", type = "bin", encoding = "UTF-8")
  write.word2vec(w2v, "W2V/w2v.txt", type = "txt", encoding = "UTF-8")
}

############ TEST SHEET
modelS <- h2o_results(
  manual, 
  train = training_frame,
  test = processed$data %>% filter(ts_sheet), 
  model_type = processed$model_type,
  plots = FALSE)

# Select model object
model <- modelS
# Show and save performance results
scores <- model$scores_test %>% 
  rename(predict = score, label = tag) %>%
  clean_predict(top = 1) %>%
  mutate(product = model$datasets$test$product) %>%
  select(id, product, label, category, probability) %>%
  clean_label() %>% as_tibble() %>% 
  mutate(correct = label == category)
model$metrics$metrics
model$model@model$cross_validation_metrics_summary$mean[1]
model$model@model$cross_validation_metrics_summary$mean[7]
mean(scores$probability)
print(model$metrics$cv_metrics)
print(model$metrics$hit_ratio)
ggplot(scores, aes(x=probability, fill = correct)) + 
  geom_density(alpha = 0.5) + theme_lares(pal=1)
#mplot_conf(scores$label, scores$category, abc = FALSE)
writeGS(scores, "Sábana de Pruebas", "V1", email = "laresbernardo@gmail.com")
