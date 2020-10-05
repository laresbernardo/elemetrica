source("funs.R")

level <- 2

# Read the data
cats <- read_data()
# Prepare dataset
df <- prepare_data(cats, level = level, minimum = 5)
freqs(df, category)

# Tokenize: Break labels into sequence of words
words <- h2o_tokenize(df$product, dropnumwords = TRUE, minchar = 1, stop_words = STOP_WORDS)

# Create word2vector model
w2v.model <- h2o_word2vec(words, 
                          epochs = 16, 
                          window_size = 6, 
                          vec_size = 10) 

# Prepare training & validation data (keep only labels made of known words)
data <- df %>% bind_cols(as.data.frame(w2v.model$vectors)) %>% filter(!is.na(C1))

# Train model
model2 <- h2o_automl(
  data, 
  y = "category", 
  ignore = "product", 
  max_models = 1,
  split = 1,
  thresh = 500, start_clean = FALSE,
  exclude_algos = c("StackedEnsemble", "XGBoost", "GLM"),
  plots = FALSE)

# Select model object to measure
model <- model2

# Cumulative predictions accuracy
mplot_topcats(model$scores_test$tag,
              model$scores_test$score,
              multis = subset(model$scores_test, select = -c(tag, score)),
              model_name = model$model_name)

# Metrics by label (test)
model$metrics$metrics_tags %>% clean_label("tag")

# Check some labeled results
model$scores_test %>% 
  mutate(correct = tag == score,
         product = model$datasets$test$product) %>%
  select(correct, product, tag, score) %>% 
  group_by(correct) %>% sample_n(10) %>% 
  clean_label(c("product","tag","score"))

# Manual testings
sample_n(df, 10) %>% clean_label(c("product","category"))
h2o_word2vec.predict("tubo pvc", w2v.model$model, model$model, clean = TRUE)
h2o_word2vec.predict("extension electrica", w2v.model$model, model$model, clean = TRUE)
h2o_word2vec.predict("botas de seguridad", w2v.model$model, model$model, clean = TRUE)
h2o_word2vec.predict("martillo", w2v.model$model, model$model, clean = TRUE)
h2o_word2vec.predict("lija", w2v.model$model, model$model, clean = TRUE)
h2o_word2vec.predict("tijera", w2v.model$model, model$model, clean = TRUE)
h2o_word2vec.predict("cerradura", w2v.model$model, model$model, clean = TRUE)

model$scores_test %>%
  mutate(correct = tag == score) %>%
  freqs(correct, tag, score) %>% 
  clean_label(c("tag","score")) %>% 
  head(20)

# # Clusters
# lares::clusterVisualK(data, 3) # {~20%}
