# run.R

# Cambiar al directorio del proyecto
setwd("C:/Users/dylan/OneDrive/Documentos/Semestre8/No Estructurados/fake-news-detector")

# Iniciar plumber API
library(plumber)

# Ruta al archivo de la API
ruta_api <- "api/api.R"

# Cargar y ejecutar
r <- plumber::plumb(ruta_api)
r$run(port = 8000, host = "0.0.0.0")

