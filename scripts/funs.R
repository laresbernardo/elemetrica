# Nice reference:
# https://github.com/h2oai/h2o-3/blob/master/h2o-r/demos/rdemo.word2vec.craigslistjobtitles.R
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/word2vec.html

library(h2o)
library(openxlsx)
library(dplyr)
library(lares)
library(ggplot2)
h2o.init()

# Define stopwords
STOP_WORDS = c("para","de","y")

# List of all possible codes
read_codes <- function() {
  file <- "data_raw/categorizacion_regional_DISENSA.xlsx"
  read.xlsx(file, "base")
}

# Import XLSX file
read_data <- function(type = 3) {
  
  if (type == 1) {
    df <- read.xlsx("data_raw/Catalogo Disensa MEX-ZMMRET02_110920.XLSX") %>%
      cleanNames() %>%
      select(texto_breve_de_material, x28, x30, x32) %>%
      rename(product = texto_breve_de_material,
             level1 = x28,
             level2 = x30,
             level3 = x32) %>%
      mutate(chain = paste(level1, level2, level3, sep = " > "),
             level2 = paste(level1, level2, sep = " > ")) 
  }
  
  if (type == 2) {
    df1 <- read.xlsx("data_raw/Catalogo Disensa MEX-ZMMRET02_110920.XLSX") %>%
      cleanNames() %>%
      select(texto_breve_de_material,
             grupo_de_materiales, 17,
             grupo_de_material_2, x22,
             grupo_de_material_3, x24,
             grupo_de_material_4, x26) %>%
      magrittr::set_colnames(c("product",
                               "level1lab","level1",
                               "level2lab","level2",
                               "level3lab","level3",
                               "level4lab","level4")) %>%
      mutate(code = paste0(level1lab, level2lab, level3lab, level4lab)) %>%
      filter(!is.na(level1)) %>%
      mutate_all(list(as.character)) %>%
      mutate(source = 1)
    
    df2 <- read.xlsx("data_raw/Detalle de productos con EAN.xlsx", 2) %>%
      cleanNames() %>%
      select(descripcion_del_producto, categoria_disensa) %>%
      rename(product = descripcion_del_producto,
             code = categoria_disensa) %>%
      mutate(level1lab = substr(code, 1,2),
             level2lab = substr(code, 3,5),
             level3lab = substr(code, 6,8),
             level4lab = substr(code, 9,11)) %>%
      left_join(read_codes(), "code") %>%
      filter(!is.na(level1)) %>%
      select(any_of(colnames(df1))) %>%
      mutate_all(list(as.character)) %>%
      mutate(source = 2)
    
  }
  
  if (type == 3) {
    df1 <- read.xlsx("data_raw/catalogoDisensaLimpia.xlsx") %>%
      cleanNames() %>%
      rename(product = texto_breve_de_material,
             code = cod,
             level1 = name_level_1, level1lab = grupo_de_materiales,
             level2 = name_level_2, level2lab = grupo_de_material_2,
             level3 = name_level_3, level3lab = grupo_de_material_3,
             level4 = name_level_4, level4lab = grupo_de_material_4) %>%
      select(product, code, 
             starts_with("level"),
             starts_with("chain")) %>%
      mutate(source = 3)
    
    df2 <- read.xlsx("data_raw/Detalle de productos con EAN.xlsx", 2) %>%
      cleanNames() %>%
      select(descripcion_del_producto, categoria_disensa) %>%
      rename(product = descripcion_del_producto,
             code = categoria_disensa) %>%
      mutate(level1lab = substr(code, 1,2),
             level2lab = substr(code, 3,5),
             level3lab = substr(code, 6,8),
             level4lab = substr(code, 9,11)) %>%
      left_join(read_codes(), "code") %>%
      filter(!is.na(level1)) %>%
      select(any_of(colnames(df1))) %>%
      mutate_all(list(as.character)) %>%
      mutate(source = 2)
    
    df <- bind_rows(df1, df2) %>%
      mutate(
        chain1 = level1,
        chain2 = paste(level1, level2, sep = " > "),
        chain3 = paste(level1, level2, level3, sep = " > "),
        chain4 = paste(level1, level2, level3, level4, sep = " > "))
  }
  
  df <- df %>%
    arrange(code) %>%
    distinct(.keep_all = TRUE) %>%
    as_tibble() 
  
  return(df)
  
}

# Prepare XLSX data
prepare_data <- function(cats, level = 1, sample_p = 1, minimum = 10, type = 1) {
  set.seed(0)
  cats[,paste0("chain", level)] %>%
    cbind(cleanText(cats$product)) %>%
    cbind(toupper(cats$code)) %>%
    cbind(cats$source) %>%
    magrittr::set_colnames(c("category", "product", "chain", "source")) %>%
    mutate(category = cleanText(category, spaces = "."),
           product = cleanText(product, spaces = ".")) %>%
    sample_n(round(nrow(cats) * sample_p)) %>%
    filter(!is.na(category)) %>%
    categ_reducer(category, nmin = minimum, other_label = "less_than_min") %>%
    filter(category != "less_than_min") %>%
    mutate(category = as.factor(category)) %>%
    as_tibble()
}

# Print summary
summary_data <- function(df, cats) {
  list(products = nrow(df),
       categories = length(unique(cats$code)),
       categories_used = length(unique(df$category)),
       data_used = round(nrow(df)/nrow(cats), 4))
}

# Convert sentences into tokenized words
h2o_tokenize <- function(x, 
                         stop_words = "",
                         minchar = 1,
                         dropnum = FALSE,
                         dropnumwords = FALSE) {
  text <- quiet(as.h2o(x))
  tokenized <- h2o.tokenize(text, "\\\\W+")
  # convert to lower case
  tokenized <- h2o.tolower(tokenized)
  # remove short words (less than 2 characters)
  tokenized.lengths <- h2o.nchar(tokenized)
  tokenized <- tokenized[is.na(tokenized.lengths) || tokenized.lengths >= minchar,]
  # remove words with numbers
  if (dropnumwords) 
    tokenized <- tokenized[h2o.grep("[0-9]", tokenized, invert = TRUE, output.logical = TRUE),]
  # remove numbers
  if (dropnum) 
    tokenized <- h2o.gsub("[0-9]", "", tokenized)
  # remove stop words
  if (stop_words[1] != "")
    tokenized <- tokenized[is.na(tokenized) || (! tokenized %in% stop_words),]
  return(tokenized)
}

# Train word2vec model
h2o_word2vec <- function(words, aggregate_method = "AVERAGE", ...) {
  # Build word2vec model
  w2v.model <- quiet(h2o.word2vec(words, ...))
  # Calculate vectors for each label
  df.h2o.vecs <- h2o.transform(w2v.model, words, aggregate_method = aggregate_method)
  return(list(model = w2v.model, vectors = df.h2o.vecs))
}

h2o_word2vec.predict <- function(x, w2v, model, clean = FALSE, top = 5, ...) {
  words <- h2o_tokenize(as.character(as.h2o(x)), ...)
  label.vec <- h2o.transform(w2v, words, aggregate_method = "AVERAGE")
  result <- as.data.frame(quiet(h2o.predict(model, label.vec)))
  result$label <- x
  if (clean) result <- clean_predict(result, top = top)
  result$id <- 1:nrow(result)
  return(result)
}

clean_predict <- function(x, top = 5) {
  x %>% 
    select(-predict) %>%
    mutate(id = row_number()) %>%
    tidyr::gather("category", "probability", -label, -id) %>% 
    arrange(id, desc(probability)) %>%
    group_by(id) %>%
    slice(1:top) %>%
    clean_label(label = "category") %>%
    select(2,1,3,4)
}

clean_label <- function(x, label = "label") {
  for (var in label) {
    vector <- unlist(as.vector(x[,var]))
    vector <- gsub("\\.\\.", " > ", vector)
    vector <- gsub("\\.", " ", vector)
    x[,var] <- vector 
  }
  return(x)
}

save_log <- function(params, save = TRUE, print = TRUE) {
  
  df <- suppressWarnings(bind_cols(params))
  log_tab <- data.frame(time = as.character(Sys.time())) %>% cbind(df)
  
  if (print) {
    log_txt <- paste(names(log_tab), log_tab, sep = ":", collapse = "|")
    print(log_txt)
  } 
  
  if (save) {
    file <- "resultslog.csv"
    if (file.exists(file)) {
      old <- read.csv(file)
      if (log_tab$model_name %in% old$model_name)
        return(invisible(NULL))
      log_tab <- bind_rows(old, log_tab)
    }
    write.csv(log_tab, file = file, row.names = FALSE)  
  }
  
}
