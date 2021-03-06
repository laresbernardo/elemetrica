# Nice reference:
# https://github.com/h2oai/h2o-3/blob/master/h2o-r/demos/rdemo.word2vec.craigslistjobtitles.R
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/word2vec.html

library(h2o)
library(openxlsx)
library(dplyr)
library(lares)
library(ggplot2)
library(word2vec)
lares::quiet(h2o.init())

# Define stopwords
STOP_WORDS = c("de","para","con")

# List of all possible codes
read_codes <- function(type = 2, refresh = FALSE) {
  if (type == 1) {
    file <- "data_raw/categorizacion_regional_DISENSA.xlsx"
    ret <- read.xlsx(file, "base") %>% as_tibble
  }
  if (type == 2) {
    if (refresh) {
      ret <- readGS(
        "Catalogos - Todos los Paises", "CATEGORIAS",
        email = "laresbernardo@gmail.com") %>%
        select(1,2,4,6,8) %>%
        magrittr::set_colnames(c("code",paste0("level",1:4)))
      write.csv(ret, "data_raw/full_catalogue.csv", row.names = FALSE) 
    } else {
      ret <- read("data_raw/full_catalogue.csv") 
    }
  }
  return(ret)
}

# Import XLSX file
read_data <- function(type = 3, refresh = FALSE) {
  
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
      magrittr::set_colnames(., c("product",
                                  "level1lab","level1",
                                  "level2lab","level2",
                                  "level3lab","level3",
                                  "level4lab","level4")) %>%
      mutate(code = paste0(level1lab, level2lab, level3lab, level4lab)) %>%
      filter(!is.na(level1)) %>%
      mutate_all(list(as.character)) %>%
      mutate(source = "catDisMEX")
    
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
      mutate(source = "detProdEAN")
    
  }
  
  if (type == 3) {
    dfs <- list()
    dfs[[1]] <- read.xlsx("data_raw/catalogoDisensaLimpia.xlsx") %>%
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
      mutate(source = "catDisLimp")
    
    dfs[[2]] <- read.xlsx("data_raw/Detalle de productos con EAN.xlsx", 2) %>%
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
      mutate(source = "detProdEAN")
    
    temp <- lares::importxlsx("data_raw/catalogosYsinonimos.xlsx")
    for (i in 1:length(temp)) temp[[i]]$source <- names(temp)[i]
    temp <- lapply(temp, function(x) mutate_all(x, as.character)) %>% bind_rows
    dfs[[3]] <- temp %>% cleanNames() %>% select(-source) %>%
      rename(product = description,
             code = codcat,
             level1lab = l1,
             level2lab = l2,
             level3lab = l3,
             level4lab = l4,
             level1 = nl1,
             level2 = nl2,
             level3 = nl3,
             level4 = nl4) %>%
      select(any_of(colnames(df1))) %>%
      mutate_all(list(as.character)) %>%
      mutate(source = paste0("cYs_", temp$source))
    
    df <- bind_rows(dfs)
  }
  
  if (type == 4) {
    if (refresh) {
      email <- "laresbernardo@gmail.com"
      # Catalogues
      mx <- readGS("Catalogos - Todos los Paises", "Catalogo Mexico", email = email)
      mx_aksi <- readGS("Catalogos - Todos los Paises", "Catalogo AKSI MX", email = email)
      mx_surtek <- readGS("Catalogos - Todos los Paises", "Catalogo surtek MX", email = email)
      mx_truper <- readGS("Catalogos - Todos los Paises", "Catalogo truper MX", email = email)
      ec <- readGS("Catalogos - Todos los Paises", "Catalogo Ecuador", email = email)
      # Manual labels
      mx_manual <- readGS("Catalogos - Todos los Paises", "Manuales MX", email = email)
      ec_manual <- readGS("Catalogos - Todos los Paises", "Manuales EC", email = email)
      # Enlist everything
      output <- list(
        mx = select(mx, `Texto breve de material`, `Codigo Categorias`),
        mx_aksi = select(mx_aksi, Description, Cod_Cat),
        mx_surtek = select(mx_surtek, Description, Cod_Cat),
        mx_truper = select(mx_truper, Description, Cod_Cat),
        ec = select(ec, Description, Cod_Cat),
        mx_manual = select(mx_manual, `Descripción del Producto`, `Categoria Final`),
        ec_manual = select(ec_manual, `Descripción del Producto`, `Categoria Final`)
      )
      for (i in 1:length(output)) {
        colnames(output[[i]]) <- c("product", "code")
        output[[i]]$source <- names(output)[i]
      }
      df <- bind_rows(output) %>% group_by(code, product) %>% 
        mutate(rep = row_number()) %>% filter(rep == 1) %>% select(-rep) %>%
        mutate(level1lab = substr(code, 1,2),
               level2lab = substr(code, 3,5),
               level3lab = substr(code, 6,8),
               level4lab = substr(code, 9,11)) %>%
        mutate(country = substr(source, 1, 2)) %>%
        left_join(read_codes(), "code") %>%
        filter(!is.na(level1))
      # Export results
      data.table::fwrite(df, "data_raw/allcatalogues_v1.csv")  
      message("Saved imported data...")
    } else {
      df <- data.table::fread("data_raw/allcatalogues_v1.csv") %>%
        mutate(country = substr(source, 1, 2))
    }
  }
  
  df <- df %>%
    mutate(
      chain1 = level1,
      chain2 = paste(level1, level2, sep = " > "),
      chain3 = paste(level1, level2, level3, sep = " > "),
      chain4 = paste(level1, level2, level3, level4, sep = " > ")) %>%
    arrange(code) %>%
    distinct(.keep_all = TRUE) %>%
    as_tibble() 
  
  return(df)
  
}

# Prepare XLSX data
prepare_data <- function(cats, level = 4, sample_p = 1, minimum = 10, type = 1, add_units = TRUE) {
  set.seed(0)
  cats[,paste0("chain", level)] %>%
    cbind(cleanText(cats$product)) %>%
    cbind(toupper(cats$code)) %>%
    cbind(cats$source) %>%
    cbind(toupper(cats$country)) %>%
    magrittr::set_colnames(., c("category", "product", "chain", "source", "country")) %>%
    mutate(category = cleanText(category, spaces = ".")) %>%
    sample_n(round(nrow(cats) * sample_p)) %>%
    filter(!is.na(category)) %>%
    categ_reducer(category, nmin = minimum, other_label = "less_than_min") %>%
    filter(category != "less_than_min") %>%
    mutate(category = as.factor(category)) %>%
    add_units(add = add_units) %>%
    filter(category != "..", product != "") %>%
    as_tibble()
}

add_units <- function(df, add = TRUE) {
  if (add) df %>%
    mutate(un_ml = grepl("ml|mls", product),
           un_pz = grepl("pz", product),
           un_mt = grepl("mt|mts|metro|metros", product),
           un_mm = grepl("mm", product),
           un_oz = grepl("oz", product),
           un_cc = grepl("cc", product),
           un_ct = grepl("ct|cts|cm", product)) %>%
    mutate_if(is.logical, as.integer)
  else df
}

# Print summary
summary_data <- function(df, cats, level = 4, train_ts = FALSE) {
  if (!train_ts) {
    test_sheet_products <- sum(df$ts_sheet)
    df <- df[!df$ts_sheet,]
  } 
  list(products = nrow(df),
       categories = nrow(unique(cats[,paste0("level",level)])),
       categories_used = length(unique(df$category)),
       data_used = round(nrow(df)/nrow(cats), 4),
       ts_products = test_sheet_products)
}

# Clean product names before word2vec
clean_product <- function(x, 
                          stop_words = "",
                          minchar = 1,
                          dropnum = FALSE,
                          dropnumwords = FALSE,
                          splitcharnums = FALSE) {
  # Must be a string vector
  x <- as.character(x)
  # All in lower case
  x <- tolower(x)
  # Split numbers from characters
  if (splitcharnums)
    x <- sapply(stringr::str_split(
      x, "(?<=\\D)(?=\\d)|(?<=\\d)(?=\\D)"), 
      function(y) paste(y, collapse = " "))
  # Remove short words 
  n <- nchar(x)
  x <- x[is.na(n) || n >= minchar]
  # Remove words with numbers
  if (dropnumwords) x <- x[!grepl("[0-9]", x)]
  # Remove numbers
  if (dropnum) x <- gsub("[0-9]", "", x)
  # Remove stop words
  if (stop_words[1] != "")
    for (word in stop_words) x <- stringr::str_remove(x, word)
  # White spaces and UTF stuff
  x <- cleanText(x)
  return(x)
}

# Convert sentences into tokenized words
h2o_tokenize <- function(x, 
                         stop_words = "",
                         minchar = 1,
                         dropnum = FALSE,
                         dropnumwords = FALSE,
                         splitcharnums = FALSE) {
  
  x <- as.character(x)
  
  # Split numbers from characters
  if (splitcharnums)
  x <- sapply(stringr::str_split(x, "(?<=\\D)(?=\\d)|(?<=\\d)(?=\\D)"),
              function(y) paste(y, collapse = " "))
  
  # Tokenize
  text <- quiet(as.h2o(x))
  tokenized <- h2o.tokenize(text, "\\\\W+")
  
  # All in lower case
  tokenized <- h2o.tolower(tokenized)
  
  # Remove short words (less than 2 characters)
  n <- h2o.nchar(tokenized)
  tokenized <- tokenized[is.na(n) || n >= minchar,]
  
  # Remove words with numbers
  if (dropnumwords) 
    tokenized <- tokenized[h2o.grep("[0-9]", tokenized, invert = TRUE, output.logical = TRUE),]
  
  # Remove numbers
  if (dropnum) 
    tokenized <- h2o.gsub("[0-9]", "", tokenized)
  
  # Remove stop words
  if (stop_words[1] != "")
    tokenized <- tokenized[is.na(tokenized) || (!tokenized %in% stop_words),]
  
  return(tokenized)
}

# Train word2vec model
h2o_word2vec <- function(words, aggregate_method = "AVERAGE", ...) {
  set.seed(0)
  # Build word2vec model
  w2v.model <- quiet(h2o.word2vec(words, ...))
  # Calculate vectors for each label
  df.h2o.vecs <- h2o.transform(w2v.model, words, aggregate_method = aggregate_method)
  return(list(model = w2v.model, vectors = df.h2o.vecs))
}

h2o_word2vec.predict <- function(x, w2v, model, clean = FALSE, top = 1, params = list()) {
  
  is_w2v <- "word2vec_trained" %in% class(w2v)
  
  if (!is_w2v) {
    words <- h2o_tokenize(as.character(as.h2o(x)), 
                          stop_words = STOP_WORDS,
                          minchar = params$minchar,
                          dropnum = params$dropnum,
                          dropnumwords = params$dropnumwords)
    label.vec <- h2o.transform(w2v, words, aggregate_method = "AVERAGE") 
  } else {
    x <- clean_product(x)
    label.vec <- data.frame(product = x) %>% 
      add_units(add = params$add_units) %>%
      bind_cols(
      suppressWarnings(doc2vec(w2v, x)) %>% as_tibble() %>%
        magrittr::set_names(paste0("C", 1:w2v$control$dim))) %>%
      as.h2o(.)
  }
  
  if (!is.character(model)) {
    result <- as.data.frame(quiet(h2o.predict(model, label.vec)))
  } else {
    result <- h2o_predict_MOJO(label.vec, model)
  }
  
  result$label <- x
  if (clean) result <- clean_predict(result, top = top)
  return(result)
}

clean_predict <- function(x, top = 5) {
  as.data.frame(x) %>% 
    select(-predict) %>%
    mutate(id = row_number()) %>%
    tidyr::gather("category", "probability", -label, -id) %>% 
    arrange(id, desc(probability)) %>%
    group_by(id) %>%
    slice(1:top) %>%
    mutate(rank = 1:top) %>%
    clean_label(label = "category") %>%
    select(2,1,3,4, rank) %>%
    as_tibble
}

clean_label <- function(x, label = "label") {
  x <- as.data.frame(x)
  for (var in label) {
    vector <- unlist(as.vector(x[,var]))
    vector <- gsub("\\.\\.", " > ", vector)
    vector <- gsub("\\.", " ", vector)
    x[,var] <- vector 
  }
  return(x)
}

save_log <- function(params, save = TRUE, print = TRUE, dir = "") {
  
  df <- suppressWarnings(bind_cols(params))
  log_tab <- data.frame(time = as.character(Sys.time())) %>% cbind(df)
  
  if (print) {
    log_txt <- paste(names(log_tab), log_tab, sep = ":", collapse = "|")
    print(log_txt)
  } 
  
  if (save) {
    file <- paste0(dir, "/resultslog.csv")
    if (file.exists(file)) {
      old <- read.csv(file) %>% mutate_all(list(as.character))
      if (log_tab$model_name %in% old$model_name)
        return(invisible(NULL))
      log_tab <- lapply(log_tab, as.character)
      log_tab <- bind_rows(old, log_tab)
    }
    write.csv(log_tab, file = file, row.names = FALSE)  
  }
  
}

test_sheets <- function(refresh = FALSE) {
  csv <- "data_raw/test_sheet.csv"
  if (refresh) {
    filename <- "Pesos por categorias"
    mx <- readGS(filename, "Resumen Mx", email = "laresbernardo@gmail.com")[,1:4] %>% mutate(country = "MX")
    ec <- readGS(filename, "Resumen Ec", email = "laresbernardo@gmail.com")[,1:4] %>% mutate(country = "EC")
    colnames(mx) <- colnames(ec) <- c("description","value","weight","code","country")
    out <- bind_rows(mx, ec) %>%
      select(country, code, value, weight, description) %>%
      group_by(country) %>% mutate(weight = value/sum(value)) %>%
      arrange(country, desc(weight)) 
    write.csv(out, file = csv, row.names = FALSE)  
  } else {
    out <- read.csv(csv)
  }
  return(out)
}

# Create dataframe with weighted samples to evaluate models
test_sheets_data <- function(products_df, weights_df, min = 5, max = 40) {
  temp <- select(weights_df, country, code, weight) %>%
    filter(code %in% products_df$chain) %>%
    mutate(total = round(scales::rescale(weight, to = c(min, max))))
  res <- c()
  for (i in 1:nrow(temp)) {
    x <- products_df %>% 
      filter(chain == temp$code[i], country == temp$country[i]) %>% 
      sample_n(temp$total[i], replace = TRUE) %>%
      distinct(.keep_all = TRUE)
    res <- bind_rows(res, x)
  }
  res <- select(res, 5,3,2,4,1)
  message("Test sheet size: ", formatNum(nrow(res), 0))
  return(res)
}
