library(h2o)
library(openxlsx)
library(dplyr)
library(lares)
library(ggplot2)

h2o.init()

# Read raw data
xlsx <- read.xlsx("data_raw/Catalogo Disensa MEX-ZMMRET02_110920.XLSX")

# Define what we'll use
cats <- xlsx %>%
  cleanNames() %>%
  select(texto_breve_de_material, x28, x30, x32) %>%
  rename(product = texto_breve_de_material,
         level1 = x28,
         level2 = x30,
         level3 = x32) %>%
  mutate(chain = paste(level1, level2, level3, sep = " > ")) %>%
  as_tibble()

# Check frequencies
freqs(cats, chain) # 262 labels
freqs(cats, level1) # 6 labels
freqs(cats, level2) # 48 labels
freqs(cats, level3) # 132 labels

# Prepare dataset
df <- data.frame(category = cleanText(cats$level3, spaces = "."), 
                 label = cleanText(cats$product)) %>% 
  filter(!is.na(category)) %>%
  categ_reducer(category, nmin = 10) %>%
  mutate(category = as.factor(category))

# Define stopwords
STOP_WORDS = c("para","de","y")

tokenize <- function(sentences, stop.words = STOP_WORDS) {
  tokenized <- h2o.tokenize(sentences, "\\\\W+")
  # convert to lower case
  tokenized <- h2o.tolower(tokenized)
  # # remove short words (less than 2 characters)
  # tokenized <- h2o.nchar(tokenized)
  # tokenized <- tokenized[is.na(tokenized) || tokenized >= 2,]
  # # remove words that contain numbers
  # tokenized <- tokenized[h2o.grep("[0-9]", tokenized, invert = TRUE, output.logical = TRUE),]
  # remove stop words
  tokenized[is.na(tokenized) || (! tokenized %in% STOP_WORDS),]
}

# `predict` conflicts with generic fn defined in R.stats
.predict <- function(label, w2v, gbm) {
  words <- tokenize(as.character(as.h2o(label)))
  label.vec <- h2o.transform(w2v, words, aggregate_method = "AVERAGE")
  result <- as.data.frame(h2o.predict(gbm, label.vec))
  result$label <- label
  return(result)
}

# Create h2o dataframe
df.h2o <- as.h2o(df)

# Break job titles into sequence of words
words <- tokenize(df.h2o$label)

# Build word2vec model
w2v.model <- h2o.word2vec(words, sent_sample_rate = 0, epochs = 10)

# Calculate a vector for each job title
df.h2o.vecs <- h2o.transform(w2v.model, words, aggregate_method = "AVERAGE")

# Prepare training & validation data (keep only labels made of known words)
valid.df.h2o <- !is.na(df.h2o.vecs$C1)
data <- as.data.frame(h2o.cbind(df.h2o[valid.df.h2o,], df.h2o.vecs[valid.df.h2o, ]))

# Train model
model3 <- h2o_automl(data, category, 
                     ignore = "label", max_models = 1,
                     thresh = 500, start_clean = FALSE,
                     exclude_algos = "StackedEnsemble")
# Test metrics: 
# AUC = 0.86161
# ACC = 0.4266

# Metrics by label (test)
model3$metrics$metrics_tags
# tag                               n     p   AUC order   ACC   PRC   TPR   TNR
# 1 herramientas.automotriz        1488 24.2  0.929     1 0.838 0.613 0.898 0.819
# 2 llaves                          398  6.47 0.871     2 0.939 0.524 0.595 0.963
# 3 brocas                          292  4.75 0.949     3 0.984 0.859 0.791 0.994
# 4 accesorios                      290  4.71 0.777     4 0.905 0.175 0.272 0.936
# 5 equipo.de.proteccion.personal   271  4.41 0.928     5 0.949 0.448 0.631 0.964
# 6 codos                           181  2.94 0.909     6 0.949 0.287 0.503 0.962
# 7 cortadoras                      152  2.47 0.758     7 0.972 0.311 0.125 0.993
# 8 pinzas                          133  2.16 0.785     8 0.951 0.127 0.218 0.967
# 9 cincel                          132  2.15 0.919     9 0.959 0.245 0.432 0.971
# 10 herramientas.de.jardineria      119  1.93 0.834    10 0.969 0.143 0.118 0.986

# Check some labeled results
model3$scores_test %>% 
  mutate(correct = tag == score,
         label = model3$datasets$test$label) %>%
  select(correct, label, tag, score) %>% 
  group_by(correct) %>% sample_n(10)

# Cumulative predictions accuracy
mplot_topcats(model3$scores_test$tag, 
              model3$scores_test$score,
              multis = subset(model3$scores_test, select = -c(tag, score)))
