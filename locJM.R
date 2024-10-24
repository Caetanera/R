if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, readxl, writexl, ggmap, leaflet, scales, jsonlite, sf)

register_google("CHAVE")

df <- readxl::read_xlsx("CAMINHO DO ARQUIVO.xlsx")


df$DATA <- as.numeric(df$DATA)
df$DATA <- as.Date(df$DATA, origin = '1900-01-01')

df <- df %>%
  mutate(Idade = floor(as.numeric(difftime("2024-05-08", DATA, units = "days")/365.25)))

# Renomeando as colunas, se necessário
colnames(df) <- paste("col", 1:ncol(df), sep = "")
rownames(df) <- NULL
df <- df[-(1:11), ]
col_names <- df[1, ]
df <- df[-1, ]
colnames(df) <- col_names

rm(col_names)

# trocando nomes de algumas colunas 
colnames(df)[colnames(df) == "CPF/CNPJ PORTADOR"] <- "CPF"
colnames(df)[colnames(df) == "DATA NASCIMENTO/DATA CONSTITUICAO"] <- "DATA" 
colnames(df)[colnames(df) == "LOCALIDADE"] <- "CIDADE"

# selecionando apenas algumas colunas 
df <- df %>%
  select(CPF,
         SETOR,
         DATA,
         LOGRADOURO,
         NUMERO,
         COMPLEMENTO,
         BAIRRO,
         CIDADE,
         UF,
         CEP) %>%
  filter(SETOR != "SETOR GERAL")


# funçao para obter coordenadas (latitude e longitude) a partir do endereço usando a API do Google Maps
get_coordinates <- function(address) {
  geocode_result <- try(geocode(address, output = "latlon", source = "google"), silent = TRUE)
  
  
    return(geocode_result)
}


df$latitude <- NA
df$longitude <- NA

for (i in 1:nrow(df)) {
  address <- paste(df$LOGRADOURO[i], df$NUMERO[i], df$BAIRRO[i], df$CIDADE[i], df$UF[i], df$CEP[i], sep = ", ")
  coordinates <- get_coordinates(address)
  if (!is.null(coordinates)) {
    df$latitude[i] <- coordinates$lat
    df$longitude[i] <- coordinates$lon
    print(i)
  } else {
    cat("erro na geocodificação para o endereço:", address, "\n")
    print(i)
    }
}

df <- df %>%
  mutate(CJ = FALSE)

cj_alvorada <- c("CJ de Alvorada", "CJ ALVORADA", "CJ ALVORADA", NA, NA, NA, NA, NA, NA, NA, NA, -30.009471, -51.089699, TRUE)
cj_cruzeiro <- c("CJ da Cruzeiro", "CJ VILA CRUZEIRO", "CJ VILA CRUZEIRO", NA, NA, NA, NA, NA, NA, NA, NA, -30.061620, -51.218559, TRUE)
cj_lomba <- c("CJ da Lomba do Pinheiro", "CJ LOMBA DO PINHEIRO", "CJ LOMBA DO PINHEIRO", NA, NA, NA, NA, NA, NA, NA, NA, -30.107625, -51.112566, TRUE)
cj_restinga <- c("CJ da Restinga", "CJ RESTINGA", "CJ RESTINGA", NA, NA, NA, NA, NA, NA, NA, NA, -30.152761, -51.141207, TRUE)
cj_rubem_berta <- c("CJ da Rubem Berta", "CJ Rubem Berta", "CJ Rubem Berta", NA, NA, NA, NA, NA, NA, NA, NA, -30.012553, -51.123055, TRUE)
cj_viamao <- c("CJ de Viamao", "CJ VIAMAO", "CJ VIAMAO", NA, NA, NA, NA, NA, NA, NA, NA, -30.081843, -51.098310, TRUE)

df <- rbind(df, cj_alvorada, cj_cruzeiro, cj_lomba, cj_restinga, cj_rubem_berta, cj_viamao)

glimpse(df)

df$latitude <- as.double(df$latitude)
df$longitude <- as.double(df$longitude)
df$CJ <- as.logical(df$CJ)
  
  
setores_cores <- c("CJ ALVORADA" = "#DD0000", "CJ LOMBA DO PINHEIRO" = "#00AEEE", 
                   "CJ RESTINGA" = "#EEAA00", "CJ RUBEM BERTA" = "#990099", 
                   "CJ VIAMÃO" = "#000099", "CJ VILA CRUZEIRO" = "#009000")

corPorSetor <- colorFactor(palette = setores_cores, domain = df$SETOR)


#####
# colocando a mancha da enchente

caminho_para_kmz <- "CAMINHO.kmz"

temp_folder <- tempdir()
unzip(caminho_para_kmz, exdir = temp_folder)

arquivos_kml <- list.files(temp_folder, pattern = "\\.kml$", full.names = TRUE)

dados_kmz <- st_read(arquivos_kml)

dados_kmzSimples <- st_zm(dados_kmz)
#####

mapa <- leaflet(data = df) %>%
  addTiles() %>%
  addPolygons(data = dados_kmzSimples, color = "red", fillOpacity = 0.5) %>%
  addCircleMarkers(
    lng = ~longitude, 
    lat = ~latitude, 
    radius = 1, 
    fillOpacity = .5, 
    popup = ~paste("Participou ou participa do ",SETOR, "<br>","Endereço: ",LOGRADOURO," Nº", NUMERO," Bairro: ",BAIRRO)) %>% 
  addAwesomeMarkers(
    data = subset(df, CJ == TRUE),
    lng = ~longitude, 
    lat = ~latitude, 
    icon = makeAwesomeIcon(icon = "home", markerColor = "white", library = "fa"),
    popup = ~paste(CPF)
  ) 
  
mapa

write_xlsx(df, "CAMINHONOVO.xlsx")
