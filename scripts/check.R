prod <- read.xlsx("data_raw/Productos.xlsx", startRow = 3)
prods <- as.vector(cleanText(prod[,4]))

# Run predictions
new <-  h2o_word2vec.predict(prods, w2v.model$model, model$model, clean = TRUE, top = 1)
new %>% ungroup() %>% sample_n(20) %>% arrange(desc(probability)) 
freqs(new, category) %>% data.frame

# Visualize results
ggplot(new, aes(x = reorder(category, probability), y = probability)) +
  geom_boxplot() + theme_lares() +
  coord_flip() +
  labs(x = NULL)

hist(new$probability)

write.csv(new, "data_preds/test2_4.csv")

freqs(new$category)