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
df <- data.frame(category = cleanText(cats$level1, spaces = "."), 
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
data <- h2o.cbind(df.h2o[valid.df.h2o,], df.h2o.vecs[valid.df.h2o, ])

# Train model
model1 <- h2o_automl(as.data.frame(data), category, 
                     ignore = "label", max_models = 1,
                     thresh = 10, start_clean = FALSE,
                     exclude_algos = "StackedEnsemble")
# Test metrics: 
#   AUC = 0.96776
#   ACC = 0.91088

# Check predictions
model1$scores_test <- model1$scores_test %>% 
  mutate(correct = tag == score,
         label = model1$datasets$test$label)

# Accuracy
freqs(model1$scores_test, correct)
# correct     n     p order  pcum
# 1 TRUE     5632 91.1      1  91.1
# 2 FALSE     551  8.91     2 100  

# Check some labeled results
model1$scores_test %>% 
  select(correct, label, tag, score, everything()) %>% 
  group_by(correct) %>% sample_n(10)

crosstab(model1$scores_test, score, tag)
# # A tibble: 6 x 8
# `score x tag`      ferreteria plomeria material.electrico acabados construccion acero total
# <fct>                   <int>    <int>              <int>    <int>        <int> <int> <dbl>
#   1 ferreteria               4622      110                146      107           44    46  5075
# 2 plomeria                   22      448                 10        6            1     1   488
# 3 material.electrico         21        8                313        3            1     2   348
# 4 acabados                    6        3                  2      155            2    NA   168
# 5 construccion                4       NA                  1        1           64     1    71
# 6 acero                       3       NA                 NA       NA           NA    30    33