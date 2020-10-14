library(rvest)
library(dplyr)
library(lares)

# Define item
item <- "mortero"
# MercadoLibre items
url <- glued('https://listado.mercadolibre.com.mx/{gsub(" ","-",tolower(item))}')
items <- try(read_html(url) %>% html_nodes(".ui-search-item__title") %>% html_text() %>% unique())
# Predictions
if (is.vector(items)) {
  items.pred <- h2o_word2vec.predict(
    items[1:5], w2v.model$model, model$model, clean = TRUE, top = 1, params = params)
  items.pred
}
