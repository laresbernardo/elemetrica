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

write.csv(new, "data_preds/test3.csv")

freqs(new$category)

############
logs <- read.csv("resultslog.csv")

# Accuracy-Coverage Curve
logs[7:14,] %>%
  ggplot(aes(x = categories_used, y = ACC, group = 1)) +
  geom_line() + geom_point() +
  ylim(0, 1) +
  labs(title = "Accuracy-Coverage Curve",
       y = "Prediction accuracy",
       x = "Number of categories used",
       caption = "Using 70% of the database + tested with 30% untrained data") +
  theme_lares()
