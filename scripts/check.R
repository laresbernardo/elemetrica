prod <- read.xlsx("data_raw/productos_disensa_30%.xlsx", "Todos los locales") %>%
  mutate(product = cleanText(`Descripción.del.Producto`),
         row_id = row_number(),
         internal = `Codigo.Elemetrica`)
nrow(prod)

# Run predictions
new <-  h2o_word2vec.predict(prod$product, w2v.model$model, model$model, clean = TRUE, top = 1)
hist(new$probability)
head(new, 10)

# Export into a CSV
write.csv(new, "data_preds/test_v5.csv")

# Check specific inputs
h2o_word2vec.predict("MORTERO 40 kg", w2v.model$model, model$model, clean = TRUE, top = 5)

df %>% clean_label("category") %>% 
  filter(grepl("morteros", category)) %>% 
  select(product, category) 

# Check random products
new %>% ungroup() %>% sample_n(20) %>% arrange(desc(probability)) 

# Visualize results
ggplot(new, aes(x = reorder(category, probability), y = probability)) +
  geom_boxplot() + theme_lares() +
  coord_flip() +
  labs(x = NULL)

############
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

lares::export_results(
  model, thresh = 1000, subdir = "MOJOs",
  which = c("txt", "mojo"))

h2o.download_mojo(w2v.model$model, path = "MOJOs", get_genmodel_jar = TRUE)
