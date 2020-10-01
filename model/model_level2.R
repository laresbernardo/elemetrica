library(h2o)
library(openxlsx)
library(dplyr)
library(lares)

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
df <- data.frame(category = cleanText(cats$level2, spaces = "."), 
                 label = cleanText(cats$product)) %>% 
  filter(!is.na(category)) %>%
  categ_reducer(category, nmin = 10) %>%
  mutate(category = as.factor(category))

# Define stopwords
STOP_WORDS = c("para","de")

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
model2 <- h2o_automl(data, category, 
                    ignore = "label", max_models = 1,
                    thresh = 500, start_clean = FALSE,
                    exclude_algos = "StackedEnsemble", 
                    plots = FALSE)
# Test metrics: 
#   AUC = 0.93406
#   ACC = 0.61745

# Metrics by label (test)
model2$metrics$metrics_tags
# tag                               n     p   AUC order   ACC   PRC   TPR   TNR
# 1 automotriz                     1495 24.3  0.958     1 0.892 0.735 0.870 0.899
# 2 herramientas.manuales           900 14.6  0.900     2 0.875 0.563 0.639 0.915
# 3 accesorios                      588  9.55 0.932     3 0.932 0.631 0.697 0.957
# 4 herramientas.para.albanileria   322  5.23 0.896     4 0.941 0.443 0.475 0.967
# 5 seguridad                       275  4.47 0.961     5 0.973 0.685 0.72  0.985
# 6 cerrajeria                      228  3.7  0.966     6 0.976 0.677 0.662 0.988
# 7 conector                        177  2.88 0.940     7 0.971 0.489 0.497 0.985
# 8 pinturas.y.recubrimientos       152  2.47 0.948     8 0.979 0.584 0.572 0.990
# 9 tornilleria                     137  2.23 0.978     9 0.981 0.546 0.774 0.985
# 10 apagadores.y.cajas              126  2.05 0.940    10 0.985 0.660 0.556 0.994

# Check predictions
model2$scores_test <- model2$scores_test %>% 
  mutate(correct = tag == score,
         label = model2$datasets$test$label)

# Check some labeled results
model2$scores_test %>% 
  select(correct, label, tag, score) %>% 
  group_by(correct) %>% sample_n(10)

crosstab(model2$scores_test, tag, score, prow = TRUE)
# `score x tag` automotriz herramientas.ma… accesorios herramientas.pa… seguridad cerrajeria conector pinturas.y.recu…
# 1 automotriz          1300              135         46               27        10         24       13               16
# 2 herramientas…        100              575         49               70         8         18        7               16
# 3 accesorios            16               35        410               18        16          5        7                9
# 4 herramientas…         10               51         20              153         7          5        5                4
# 5 seguridad              2               23          9               13       198         NA       NA                1
# 6 cerrajeria             3                2          3                2         3        148        1                2
# 7 tornilleria           NA                1          2                4         4          3        8                3
# 8 conector               3                5          7                1         5          1       88                1
# 9 pinturas.y.r…          2                4          4                7         4          1        1               87
# 10 jardineria             4                8          5                5         1          2        9               NA
