# Global functions
source("scripts/funs.R")
# Define model parameters
source("model4/params4.R")

ec_nolabs <- readGS(
  "Catalogos - Todos los Paises", "Sin Categorizar EC", 
  email = "laresbernardo@gmail.com",
  range = "C4:D19067")
prod <- ec_nolabs %>% 
  magrittr::set_colnames(c("product", "internal")) %>%
  mutate(row_id = row_number(),
         country = "EC")

# Run predictions
new <-  h2o_word2vec.predict(
  prod$product, w2v, model$model, 
  params = params, clean = TRUE, top = 1)
freqs(new$category)
hist(new$probability)
summary(new$probability)
head(new)

# Check random products
new %>% ungroup() %>% sample_n(20) %>% arrange(desc(probability))

# Check specific inputs
sample_n(cats, 5) %>% select(product, chain4)
product_name <- "TEFLON"
h2o_word2vec.predict(product_name, w2v, model$model, params = params, clean = TRUE, top = 5)
# Search for products/categories containing product_name
df %>% clean_label("category") %>% 
  filter(grepl(product_name, paste(category, product))) %>% 
  select(source, category, product)
# Check close words or synonyms
predict(w2v, product_name, type = "nearest", top_n = 8)

# Top N Predictors
gs <-  h2o_word2vec.predict(
  prod$product, w2v, model$model, 
  params = params, clean = TRUE, top = 2) %>%
  group_by(id) %>% mutate(tied = length(unique(probability)) == 1) %>%
  left_join(select(prod, row_id:internal), by = c("id" = "row_id"))
head(gs); dim(gs)
# How many does the model is uncertain between the top labels?
gs %>% slice(1) %>% freqs(tied)
# Distributions
gs %>% ungroup() %>% mutate(range = quants(100*probability, 5, "labels")) %>% freqs(range, abc=T)

# Upload to GSheets
writeGS(gs, "Catalogos - Todos los Paises", "Modelo EC")

############ Check logs
logs <- read.csv("resultslog.csv")

# Accuracy-Coverage Curve
logs[7:13,] %>%
  ggplot(aes(x = categories_used, y = ACC, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "Accuracy-Coverage Curve",
       y = "Prediction accuracy",
       x = "Number of categories used",
       caption = "Using 70% of the database + tested with 30% untrained data") +
  theme_lares() +
  scale_y_percent()


# USE MOJO FILES TO PREDICT
temp <-  h2o_word2vec.predict(
  x = prod$product[1:100], 
  w2v = w2v.model$model, 
  model = "MOJOs/DRF_1_AutoML_20201009_163058",
  params = params, 
  clean = TRUE)
hist(temp$probability)

# JOIN RESULTS TO COMPARE
gs1 <- readGS("Elemétrica: Catálogo Disensa", "TopProducts", email = "laresbernardo@gmail.com")
gs2 <- readGS("Elemétrica: Catálogo Disensa", "TopProducts2", email = "laresbernardo@gmail.com")
gs1 %>% left_join(gs2 %>% filter(rank == 1), by = c(`Codigo Elemetrica` = "internal")) %>%
  filter(!is.na(category)) %>%
  mutate(unchanged = `Predicción` == category) %>%
  filter(!unchanged) %>%
  sample_n(20) %>%
  select(label, `Predicción`, `Certeza`, category, probability)

mplot_conf(tag = model$scores_test$tag,
           score = model$scores_test$score,
           abc = FALSE, diagonal = TRUE, top = 20)
