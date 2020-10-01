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
df <- data.frame(category = cleanText(cats$chain, spaces = "."), 
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
model4 <- h2o_automl(data, category, 
                     ignore = "label", max_models = 1,
                     thresh = 500, start_clean = FALSE,
                     exclude_algos = "StackedEnsemble")
# Test metrics: 
  # AUC = 0.80408
  # ACC = 0.36983

# Metrics by label (test)
model4$metrics$metrics_tags
# tag                                                      n     p    AUC order   ACC    PRC    TPR   TNR
# 1 ferreteria..automotriz..herramientas.automotriz       1471 23.8   0.927     1 0.825 0.588  0.878  0.808
# 2 ferreteria..herramientas.manuales..llaves              398  6.44  0.786     2 0.939 0.519  0.628  0.960
# 3 ferreteria..accesorios..brocas                         313  5.06  0.933     3 0.942 0.456  0.732  0.954
# 4 ferreteria..seguridad..equipo.de.proteccion.personal   234  3.78  0.910     4 0.939 0.305  0.470  0.958
# 5 ferreteria..corte.y.terminacion..cortadoras            125  2.02  0.663     5 0.958 0.102  0.136  0.975
# 6 ferreteria..accesorios..discos.de.corte                118  1.91  0.884     6 0.952 0.201  0.5    0.961
# 7 other                                                  108  1.75 NA         7 0.968 0.0926 0.0926 0.984
# 8 ferreteria..accesorios..accesorios                      99  1.6   0.745     8 0.980 0.0968 0.0303 0.995
# 9 ferreteria..almacenamientomanipulacion.de.materiale…    97  1.57  0.840     9 0.963 0.190  0.423  0.971
# 10 material.electrico..herramientas.para.electricidad.…    82  1.33  0.799    10 0.975 0.214  0.341  0.983

# Check some labeled results
model4$scores_test %>% 
  mutate(correct = tag == score,
         label = model4$datasets$test$label) %>%
  select(correct, label, tag, score) %>% 
  group_by(correct) %>% sample_n(10)

# Cumulative predictions accuracy
mplot_topcats(model4$scores_test$tag, 
              model4$scores_test$score,
              multis = subset(model4$scores_test, select = -c(tag, score)))
