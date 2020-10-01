library(h2o)
library(openxlsx)
library(dplyr)
library(lares)

h2o.init()

# Load data and check levels
cats <- read.xlsx("data_raw/Catalogo Disensa MEX-ZMMRET02_110920.XLSX", "base")
freqs(cats, level1) 
freqs(cats, level2)
freqs(cats, level3)

# Prepare dataset
df <- data.frame(category = cleanText(cats$level1), 
                 label = cleanText(cats$product)) %>% 
  filter(!is.na(category)) %>%
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
  result <- h2o.predict(gbm, label.vec)
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
data.split <- msplit(as.data.frame(data), 0.8)

# Build a basic GBM model
gbm.model <- h2o.gbm(x = names(df.h2o.vecs), 
                     y = "category",
                     training_frame = as.h2o(data.split$train), 
                     validation_frame = as.h2o(data.split$test))

# Run predictions
scores <- .predict(data.split$test$label, w2v.model, gbm.model) %>% 
  as.data.frame %>%
  mutate(real = as.vector(df.h2o$category),
         correct = real == as.character(predict)) %>%
  as_tibble()

# Accuracy
freqs(scores, correct)

# Check some wrong-labeled results
scores %>% filter(!correct) %>% select(label, real, predict) %>% sample_n(10)

# # Plot ROC Curves (when few categories only)
# lares::mplot_roc(scores$real, scores$predict, 
#                  scores[,-c(1, ncol(scores)-1, ncol(scores))])
