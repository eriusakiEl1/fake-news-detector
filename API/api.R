# api/api.R

library(plumber)
library(rvest)
library(httr)
library(stringr)
library(mongolite)
library(jsonlite)
library(textrank)
library(tokenizers)

# ---------------------------
# FILTRO CORS
# ---------------------------
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}

# ---------------------------
# EXTRAER TEXTO DE LA PÁGINA
# ---------------------------
extraer_texto_url <- function(url) {
  tryCatch({
    pagina_raw <- httr::GET(url)
    contenido <- content(pagina_raw, as = "text", encoding = "UTF-8")
    pagina <- read_html(contenido)
    
    texto <- pagina %>%
      html_elements("p") %>%
      html_text2() %>%
      paste(collapse = " ")
    
    texto <- str_squish(texto)
    
    if (nchar(texto) < 200) return(NULL)
    return(texto)
  }, error = function(e) {
    return(NULL)
  })
}

# ---------------------------
# RESUMEN INTELIGENTE (Hugging Face + fallback textrank)
# ---------------------------
resumen_inteligente <- function(texto, n = 3) {
  if (nchar(texto) < 200) return(substr(texto, 1, 400))

  texto_corto <- substr(texto, 1, 3000)

  # --- Hugging Face
  resumen <- tryCatch({
    res <- httr::POST(
      url = "https://api-inference.huggingface.co/models/facebook/bart-large-cnn",
      httr::add_headers(
        Authorization = "Bearer TU_TOKEN_AQUI",
        `Content-Type` = "application/json",
        Accept = "application/json"
      ),
      body = jsonlite::toJSON(list(inputs = texto_corto), auto_unbox = TRUE)
    )

    data <- jsonlite::fromJSON(content(res, as = "text", encoding = "UTF-8"))

    if (!is.null(data$error) || is.null(data[[1]]$summary_text)) {
      message("⚠️ HuggingFace error: ", data$error %||% "sin resumen")
      stop("modelo no disponible")
    }

    return(data[[1]]$summary_text)
  }, error = function(e) {
    message("⚠️ HuggingFace falló, usando textrank: ", e$message)

    # --- fallback: textrank
    oraciones <- tryCatch(
      unlist(tokenizers::tokenize_sentences(texto)),
      error = function(e) return(character(0))
    )

    if (length(oraciones) < 2) return(substr(texto, 1, 400))

    resumen <- tryCatch({
      resultado <- textrank_sentences(data.frame(text = oraciones))
      if (!("sentences" %in% names(resultado)) || is.null(resultado$sentences)) {
        return(substr(texto, 1, 400))
      }
      top <- resultado$sentences[order(-as.numeric(resultado$sentences$textrank)), "text"]
      paste(head(top, n), collapse = " ")
    }, error = function(e2) {
      return(substr(texto, 1, 400))
    })

    return(resumen)
  })

  return(resumen)
}


# ---------------------------
# LISTA DE PALABRAS SOSPECHOSAS
# ---------------------------
palabras_falsas <- c(
  # Español
  "falso", "engañoso", "mentira", "bulo", "desinformación", "conspiración",
  "revelado", "impactante", "escándalo", "devastador", "urgente", "increíble",
  "secreto", "censurado", "viral", "nunca", "siempre", "todo el mundo", "nadie lo sabía",
  "traición", "la verdad", "lo que no quieren que sepas", "elite", "manipulación",
  "nuevo orden mundial", "microchip", "illuminati", "teoría", "drama", "pánico",
  "control mental", "experimento", "esto cambiará tu vida",
  # Inglés
  "fake", "false", "hoax", "misleading", "disinformation", "shocking", "breaking",
  "urgent", "revealed", "exposed", "secret", "scandal", "banned", "censored",
  "you won't believe", "click here", "what happened next", "insane", "never seen before",
  "new world order", "agenda", "mind control", "conspiracy", "fraud", "lies"
)

# ---------------------------
# ANÁLISIS DEL TEXTO
# ---------------------------
analizar_texto <- function(texto) {
  texto_lower <- tolower(texto)
  coincidencias <- sapply(palabras_falsas, function(p) grepl(p, texto_lower, fixed = TRUE))
  cantidad_sospechosas <- sum(coincidencias)
  sospechoso <- cantidad_sospechosas > 0
  
  veredicto <- ifelse(sospechoso, "falsa", "verdadera")
  puntuacion <- if (sospechoso) sample(40:60, 1) else sample(80:100, 1)
  
  palabrasClave <- unique(str_extract_all(texto, "\\b\\w{6,}\\b")[[1]])
  palabrasClave <- palabrasClave[1:min(8, length(palabrasClave))]
  
  resumen <- resumen_inteligente(texto)
  
  list(
    resumen = resumen,
    texto = substr(texto, 1, 500),
    veredicto = veredicto,
    puntuacion = puntuacion,
    palabrasClave = palabrasClave,
    sospechosasDetectadas = cantidad_sospechosas,
    fecha = Sys.time()
  )
}

# ---------------------------
# BÚSQUEDA DE NOTICIAS RELACIONADAS (NewsAPI + Reddit)
# ---------------------------
buscar_noticias_relacionadas <- function(palabras, idioma = "es") {
  api_key <- "5932c59ca7294da686639873f8809cd0"  # ← REEMPLAZA con tu key real
  query <- URLencode(paste(palabras, collapse = " "))
  noticias <- list()
  
  # NewsAPI
  url_news <- paste0("https://newsapi.org/v2/everything?q=", query,
                     "&language=", idioma, "&pageSize=3&sortBy=relevancy&apiKey=", api_key)
  res_news <- tryCatch(httr::GET(url_news), error = function(e) NULL)
  
  if (!is.null(res_news) && httr::status_code(res_news) == 200) {
    data_json <- content(res_news, as = "text", encoding = "UTF-8")
    data <- tryCatch({
      parsed <- fromJSON(data_json, simplifyVector = FALSE)
      if (!is.null(parsed$articles)) parsed$articles else NULL
    }, error = function(e) NULL)
    
    if (!is.null(data)) {
      noticias <- c(noticias, lapply(data, function(a) {
        list(titulo = a$title, fuente = a$source$name, url = a$url)
      }))
    }
  }
  
  # Reddit
  url_reddit <- paste0("https://www.reddit.com/search.json?q=", query, "&limit=3")
  res_reddit <- tryCatch(httr::GET(url_reddit, httr::add_headers(`User-Agent` = "RPlumberBot/1.0")), error = function(e) NULL)
  
  if (!is.null(res_reddit) && httr::status_code(res_reddit) == 200) {
    data_json <- content(res_reddit, as = "text", encoding = "UTF-8")
    data <- tryCatch(fromJSON(data_json, simplifyVector = FALSE), error = function(e) NULL)
    
    if (!is.null(data$data$children)) {
      reddit_posts <- data$data$children
      noticias <- c(noticias, lapply(reddit_posts, function(p) {
        list(titulo = p$data$title, fuente = "Reddit", url = paste0("https://www.reddit.com", p$data$permalink))
      }))
    }
  }
  
  if (length(noticias) > 0) return(noticias)
  return(NULL)
}

# ---------------------------
# FUNCIÓN PRINCIPAL
# ---------------------------
analizar_desde_url <- function(url) {
  texto <- extraer_texto_url(url)
  
  if (is.null(texto)) {
    return(list(error = "No se pudo extraer contenido significativo de la URL."))
  }
  
  resultado <- analizar_texto(texto)
  resultado$origen <- list(fuente = "Web", url = url)
  
  idioma <- ifelse(grepl("[áéíóúñ]", texto), "es", "en")
  resultado$relacionadas <- buscar_noticias_relacionadas(resultado$palabrasClave, idioma)
  
  tryCatch({
    con <- mongo(collection = "noticias_evaluadas", db = "noticias", url = "mongodb://localhost")
    con$insert(resultado)
  }, error = function(e) {
    message("⚠️ Error guardando en MongoDB: ", e$message)
  })
  
  return(resultado)
}

# ---------------------------
# ENDPOINT
# ---------------------------
#* @post /analizar_url
#* @param url La URL de la noticia a analizar
function(url = "") {
  tryCatch({
    analizar_desde_url(url)
  }, error = function(e) {
    message("❌ Error en la API: ", e$message)
    return(list(error = "Error interno del servidor."))
  })
}
