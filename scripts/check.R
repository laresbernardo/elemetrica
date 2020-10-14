# Global functions
source("scripts/funs.R")
# Define model parameters
source("scripts/params.R")

prod <- read.xlsx("data_raw/Productos_Candidatos_30_corregidos.xlsx", "Todos") %>%
  cleanNames() %>%
  mutate(product = cleanText(descripcion_del_producto),
         row_id = row_number(),
         internal = codigo_producto_solo_elemetrica)

# Run predictions
new <-  h2o_word2vec.predict(prod$product, w2v.model$model, model$model, 
                             params = params, clean = TRUE, top = 1)
hist(new$probability)
mean(new$probability)
head(new, 10)

# Check random products
new %>% ungroup() %>% sample_n(20) %>% arrange(desc(probability))

# Export into a CSV
write.csv(new, "data_preds/test_v5.2.csv")

# Check specific inputs
h2o_word2vec.predict("MORTERO 40 kg", w2v.model$model, model$model, params = params, clean = TRUE, top = 5)

df %>% clean_label("category") %>% 
  filter(grepl("morteros", category)) %>% 
  select(product, category) 

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


# USE MOJO FILES TO PREDICT
temp <-  h2o_word2vec.predict(
  x = prod$product[1:100], 
  w2v = w2v.model$model, 
  model = "MOJOs/DRF_1_AutoML_20201009_163058",
  params = params, 
  clean = TRUE)
hist(temp$probability)

h2o::h2o.findSynonyms(w2v.model$model, as.h2o("mortero"), 10)

# ADD INTERNAL CODES
gs <- readGS("Elemétrica: Timetable", "TopProducts")
labeled_gs <- gs %>% left_join(
  cats %>% mutate(chain4 = cleanText(cats$chain4, spaces = ".")) %>% 
    clean_label("chain4") %>%
    select(chain4, code) %>% distinct(), 
  by = c("Predicción" = "chain4"))
writeGS(select(labeled_gs, code), "Elemétrica: Timetable", "Temp")
