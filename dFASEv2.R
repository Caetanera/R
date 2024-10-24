if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, readxl, lubridate, writexl)

caminho_do_arquivo <- "C:/Users/caetn/OneDrive/Área de Trabalho/Caetano/Prog/R/Dados/MONITORAMENTO/FASE/FASEv0.xlsx" # Especificando o caminho do arquivo
planilhas <- excel_sheets(caminho_do_arquivo) # Função para obter os nomes das planilhas no arquivo
nome_da_planilha <- "MOV" # Selecionando a planilha desejada
df <- read_excel(caminho_do_arquivo, sheet = nome_da_planilha) # Função read_excel para importar a planilha desejada

df$Data_da_Movimentacao <- as.Date(df$Data_da_Movimentacao) # Função as.Date para transformar os valores em "date"
df$Data_de_Nascimento <- as.Date(df$Data_de_Nascimento)

################################################################################################
# RESULTADO 1 - Todas as idades e sem passar mais de 18 meses na FASE
################################################################################################

df_FASE <- df %>%
  distinct(Prontuario, `NRO-SEQ`, .keep_all = TRUE) %>% # Excluo os valores repetidos de acordo com o NRO-SEQ e o Prontuário
  group_by(Prontuario,`NRO-SEQ`, Data_de_Nascimento, Movimentacao, Ato_Infracional) %>% # Mantêm e agrupa os valores das colunas expecificadas
  summarize( # Utilizo funções no grupo de valores
    Data_Entrada = first(Data_da_Movimentacao[Movimentacao == "Entrada"]), 
    Data_Desligamento_ou_Fuga = first(Data_da_Movimentacao[Movimentacao %in% c("Desligamento", "Fuga")]) 
  ) %>% # Função first extrai o valor específico da coluna especificada seguindo algum critério
  ungroup() %>% # Desagrupo
  group_by(Prontuario) %>%
  fill(Data_Desligamento_ou_Fuga, .direction = "up") %>% # Coloco o valor da coluna Data_Desligamento_ou_Fuga para uma célula acima
  filter(!is.na(Data_Entrada)) %>% #Exclui as linhas com NAs
  mutate(Dias_na_FASE = as.numeric(difftime(Data_Desligamento_ou_Fuga, Data_Entrada, units = "days"))) %>% # Calcula o número de dias na fase
  select(-Movimentacao, -"NRO-SEQ") %>% # Exclui colunas desnecessarias
  mutate(
    Idade_Entrada = round(as.numeric(difftime(Data_Entrada, Data_de_Nascimento, units = "days")/365.25)),
    Idade_Desligamento_ou_fuga = round(as.numeric(difftime(Data_Desligamento_ou_Fuga, Data_de_Nascimento, units = "days")/365.25))
  ) %>%
  relocate(Ato_Infracional, .after = Dias_na_FASE) %>% # Coloca a coluna Ato Infracional na última posição do DataFrame
  relocate(Idade_Entrada, .after = Data_Entrada) %>% # Coloca a coluna Ato Infracional na última posição do DataFrame
  relocate(Idade_Desligamento_ou_fuga, .after = Data_Desligamento_ou_Fuga) %>%
  select(-Data_de_Nascimento) %>%
  filter(Dias_na_FASE > 45) %>%
  mutate(Ano_Entrada = lubridate::year(Data_Entrada)) 


df_FASEv1 <- df_FASE %>%
  mutate(
    Reincidente = if_else(
      row_number() > which.max(Dias_na_FASE > 45),
      1,
      2
    )
  ) %>%
  relocate(Dias_na_FASE, .after = Prontuário) %>%
  relocate(Reincidente, .after = Dias_na_FASE) %>%
  mutate( # filtra apenas a primeira reincidência do adolescente
    Primeira_reincidencia = ifelse(row_number() > which.max(Reincidente == 1), 2, Reincidente)
  ) %>%
  relocate(Primeira_reincidencia, .after = Reincidente) %>%
  ungroup()

# Cria lista com df por ano

list_FASEv1 <- df_FASEv1  %>%  # Cria uma lista de df com base no ano de entrada
  split(.$Ano_Entrada, drop = TRUE)

# Cria um df para armazenar as infos por ano
info_res1 <- data.frame(Ano_Entrada = double(),
                        Total = double(),
                        Reincidentes = double(),
                        Resultado = double(),
                        stringsAsFactors = FALSE)

# Cria arquivos .xlsx para cada ano com dados do df_FASEv4aL!!
for (i in seq_along(list_FASEv1)) {
  nome_arquivo <- paste0("C:/Users/caetano-garcia/Desktop/R/FASE/RESULTADOS/RESULTADOSv1",
                         list_FASEv1[[i]]$Ano_Entrada,".xlsx")
  
  # Total
  total_res1 <- n_distinct(list_FASEv1[[i]]$Prontuário)
  # Reincidentes
  rein_res1 <-  sum(list_FASEv1[[i]]$Primeira_reincidencia == 1, na.rm = TRUE)
  # Resultado
  resultado1 <- 100 * rein_res1 / total_res1  
  
  # Coloca as infos em um data frame por ano
  info_res1 <- bind_rows(info_res1, data.frame(Ano_Entrada = list_FASEv1[[i]]$Ano_Entrada[1],
                                               Total = total_res1,
                                               Reincidentes = rein_res1,
                                               Resultado = resultado1))
  
  
  # Salvando o dataframe como um arquivo .xlsx
  write_xlsx(list_FASEv1[[i]],nome_arquivo)
  
  print(paste("O total de jovens no ano de ", list_FASEv1[[i]]$Ano_Entrada, " é: ", total_res1,". 
        E o número de reincidentes é: ", rein_res1, ". Tendo como resultado: ", resultado1 ))
  
  print(paste("Arquivo", nome_arquivo, "criado com sucesso!"))
  
  # Exclui a coluna do ano de entrada do df atual 
  list_FASEv1[[i]]$Ano_Entrada <- NULL
}

# Total
total_res1 <- n_distinct(df_FASEv1$Prontuário)
# Reincidentes
rein_res1 <-  sum(df_FASEv1$Primeira_reincidencia == 1, na.rm = TRUE)
# Resultado
resultado1 <- 100 * rein_res1 / total_res1  

info_res1$Ano_Entrada <- as.character(info_res1$Ano_Entrada)
info_res1 <- bind_rows(info_res1, data.frame(Ano_Entrada = "Total",
                                             Total = total_res1,
                                             Reincidentes = rein_res1,
                                             Resultado = resultado1))

# Salvando o dataframe com os resultados como um arquivo .xlsx
# 
write_xlsx(info_res1,"C:/Users/caetano-garcia/Desktop/R/FASE/RESULTADOS/RESULTADO.1.POR.ANO.xlsx")



################################################################################################
# RESULTADO 2 - Menores de 18 e sem o critério de passar mais de 18 meses na FASE
################################################################################################


# menores de 18
df_FASEv2 <- df_FASE %>% 
  group_by(Prontuário) %>%
  filter(Idade_Entrada <= 17) %>% 
  mutate(
    Reincidente = if_else(
      row_number() > which.max(Dias_na_FASE > 45),
      1,
      2
    )
  ) %>%
  mutate( # filtra apenas a primeira reincidência do adolescente
  Primeira_reincidencia = ifelse(row_number() > which.max(Reincidente == 1), 2, Reincidente)) %>%
  relocate(Dias_na_FASE, .after = Prontuário) %>%
  relocate(Reincidente, .after = Dias_na_FASE) %>%
  relocate(Primeira_reincidencia, .after = Reincidente) %>%
  ungroup()

# Cria lista com df por ano

list_FASEv2 <- df_FASEv2  %>%  # Cria uma lista de df com base no ano de entrada
  split(.$Ano_Entrada, drop = TRUE)

# Cria um df para armazenar as infos por ano
info_res2 <- data.frame(Ano_Entrada = double(),
                        Total = double(),
                        Reincidentes = double(),
                        Resultado = double(),
                        stringsAsFactors = FALSE)

# Cria arquivos .xlsx para cada ano com dados do df_FASEv4aL!!
for (i in seq_along(list_FASEv2)) {
  nome_arquivo <- paste0("C:/Users/caetano-garcia/Desktop/R/FASE/RESULTADOS/RESULTADOSv2",
                         list_FASEv2[[i]]$Ano_Entrada,".xlsx")
  
  # Total
  total_res2 <- n_distinct(list_FASEv2[[i]]$Prontuário)
  # Reincidentes
  rein_res2 <-  sum(list_FASEv2[[i]]$Primeira_reincidencia == 1, na.rm = TRUE)
  # Resultado
  resultado2 <- 100 * rein_res2 / total_res2  
  
  # Coloca as infos em um data frame por ano
  info_res2 <- bind_rows(info_res2, data.frame(Ano_Entrada = list_FASEv2[[i]]$Ano_Entrada[1],
                                               Total = total_res2,
                                               Reincidentes = rein_res2,
                                               Resultado = resultado2))
  
  
  # Salvando o dataframe como um arquivo .xlsx
  write_xlsx(list_FASEv2[[i]],nome_arquivo)
  
  print(paste("Arquivo", nome_arquivo, "criado com sucesso!"))
  
  # Exclui a coluna do ano de entrada do df atual 
  list_FASEv2[[i]]$Ano_Entrada <- NULL
}


# Total
total_res2 <- n_distinct(df_FASEv2$Prontuário)
# Reincidentes
rein_res2 <-  sum(df_FASEv2$Primeira_reincidencia == 1, na.rm = TRUE)
# Resultado
resultado2 <- 100 * rein_res2 / total_res2  

info_res2$Ano_Entrada <- as.character(info_res2$Ano_Entrada)
info_res2 <- bind_rows(info_res2, data.frame(Ano_Entrada = "Total",
                                             Total = total_res2,
                                             Reincidentes = rein_res2,
                                             Resultado = resultado2))


# Salvando o dataframe com os resultados como um arquivo .xlsx

write_xlsx(info_res2,"C:/Users/caetano-garcia/Desktop/R/FASE/RESULTADOS/RESULTADO.2.POR.ANO.xlsx")

################################################################################################
# RESULTADO 3 - Todas as idades e com o critério de passar mais de 18 meses na FASE
################################################################################################

# Todas as idades

df_FASEv3 <- df_FASE %>% 
  arrange(Prontuário, Data_Entrada) %>%
  group_by(Prontuário) %>%
  filter(max(Dias_na_FASE) > 547 & Dias_na_FASE > 45) %>%
  mutate(
    Reincidente = if_else(
      row_number() > which.max(Dias_na_FASE > 547 & Dias_na_FASE > 45),
      1,
      2
    )
  ) %>%
  mutate( # filtra apenas a primeira reincidência do adolescente
    Primeira_reincidencia = ifelse(row_number() > which.max(Reincidente == 1), 2, Reincidente)) %>%
  relocate(Dias_na_FASE, .after = Prontuário) %>%
  relocate(Reincidente, .after = Dias_na_FASE) %>%
  relocate(Primeira_reincidencia, .after = Reincidente) %>%
  ungroup()

# Cria lista com df por ano

list_FASEv3 <- df_FASEv3  %>%  # Cria uma lista de df com base no ano de entrada
  split(.$Ano_Entrada, drop = TRUE)

# Cria um df para armazenar as infos por ano
info_res3 <- data.frame(Ano_Entrada = double(),
                        Total = double(),
                        Reincidentes = double(),
                        Resultado = double(),
                        stringsAsFactors = FALSE)

# Cria arquivos .xlsx para cada ano com dados do df_FASEv4aL!!
for (i in seq_along(list_FASEv3)) {
  nome_arquivo <- paste0("C:/Users/caetano-garcia/Desktop/R/FASE/RESULTADOS/RESULTADOSv4",
                         list_FASEv3[[i]]$Ano_Entrada,".xlsx")
  
  # Total
  total_res3 <- n_distinct(list_FASEv3[[i]]$Prontuário)
  # Reincidentes
  rein_res3 <-  sum(list_FASEv3[[i]]$Primeira_reincidencia == 1, na.rm = TRUE)
  # Resultado
  resultado3 <- 100 * rein_res3 / total_res3
  
  # Coloca as infos em um data frame por ano
  info_res3 <- bind_rows(info_res3, data.frame(Ano_Entrada = list_FASEv3[[i]]$Ano_Entrada[1],
                                               Total = total_res3,
                                               Reincidentes = rein_res3,
                                               Resultado = resultado3))
  
  
  # Salvando o dataframe como um arquivo .xlsx
  write_xlsx(list_FASEv3[[i]],nome_arquivo)
  
  print(paste("Arquivo", nome_arquivo, "criado com sucesso!"))
  
  # Exclui a coluna do ano de entrada do df atual 
  list_FASEv3[[i]]$Ano_Entrada <- NULL
}


# Total
total_res3 <- n_distinct(df_FASEv3$Prontuário)
# Reincidentes
rein_res3 <-  sum(df_FASEv3$Primeira_reincidencia == 1, na.rm = TRUE)
# Resultado
resultado3 <- 100 * rein_res3 / total_res3

info_res3$Ano_Entrada <- as.character(info_res3$Ano_Entrada)
info_res3 <- bind_rows(info_res3, data.frame(Ano_Entrada = "Total",
                                             Total = total_res3,
                                             Reincidentes = rein_res3,
                                             Resultado = resultado3))


# Salvando o dataframe com os resultados como um arquivo .xlsx
# 
write_xlsx(info_res3,"C:/Users/caetano-garcia/Desktop/R/FASE/RESULTADOS/RESULTADO.3.POR.ANO.xlsx")



################################################################################################
# RESULTADO 4 - Menores de 18 e COM o critério de passar mais de 18 meses na FASE
################################################################################################

# Os adolescentes incluídos são aqueles que estão 
df_FASEv4 <- df_FASE %>%
  group_by(Prontuário) %>%
  filter(max(Dias_na_FASE) > 547 & Dias_na_FASE > 45) %>% 
  ungroup() %>%
  filter(Idade_Entrada <= 17) %>% 
  arrange(Prontuário, Data_Entrada) %>%
  group_by(Prontuário) %>%
  mutate(
  Reincidente = if_else(
    row_number() > which.max(Dias_na_FASE > 547 & Dias_na_FASE > 45),
    1,
    2
  )) %>%
  mutate( # filtra apenas a primeira reincidência do adolescente
    Primeira_reincidencia = ifelse(row_number() > which.max(Reincidente == 1), 2, Reincidente)) %>%
  relocate(Dias_na_FASE, .after = Prontuário) %>%
  relocate(Reincidente, .after = Dias_na_FASE) %>%
  relocate(Primeira_reincidencia, .after = Reincidente) %>%
  ungroup()

# Cria lista com df por ano

list_FASEv4 <- df_FASEv4  %>%  # Cria uma lista de df com base no ano de entrada
  split(.$Ano_Entrada, drop = TRUE)

# Cria um df para armazenar as infos por ano
info_res4 <- data.frame(Ano_Entrada = double(),
                        Total = double(),
                        Reincidentes = double(),
                        Resultado = double(),
                        stringsAsFactors = FALSE)

# Cria arquivos .xlsx para cada ano com dados do df_FASEv4aL!!
for (i in seq_along(list_FASEv4)) {
  nome_arquivo <- paste0("C:/Users/caetano-garcia/Desktop/R/FASE/RESULTADOS/RESULTADOSv4",
                         list_FASEv4[[i]]$Ano_Entrada,".xlsx")
  
  # Total
  total_res4 <- n_distinct(list_FASEv4[[i]]$Prontuário)
  # Reincidentes
  rein_res4 <-  sum(list_FASEv4[[i]]$Primeira_reincidencia == 1, na.rm = TRUE)
  # Resultado
  resultado4 <- 100 * rein_res4 / total_res4
  
  # Coloca as infos em um data frame por ano
  info_res4 <- bind_rows(info_res4, data.frame(Ano_Entrada = list_FASEv4[[i]]$Ano_Entrada[1],
                                               Total = total_res4,
                                               Reincidentes = rein_res4,
                                               Resultado = resultado4))
  
  
  # Salvando o dataframe como um arquivo .xlsx
  write_xlsx(list_FASEv4[[i]],nome_arquivo)
  
  print(paste("Arquivo", nome_arquivo, "criado com sucesso!"))
  
  # Exclui a coluna do ano de entrada do df atual 
  list_FASEv4[[i]]$Ano_Entrada <- NULL
}


# Total
total_res4 <- n_distinct(df_FASEv4$Prontuário)
# Reincidentes
rein_res4 <-  sum(df_FASEv4$Primeira_reincidencia == 1, na.rm = TRUE)
# Resultado
resultado4 <- 100 * rein_res4 / total_res4

info_res4$Ano_Entrada <- as.character(info_res4$Ano_Entrada)
info_res4 <- bind_rows(info_res4, data.frame(Ano_Entrada = "Total",
                                             Total = total_res4,
                                             Reincidentes = rein_res4,
                                             Resultado = resultado4))


# Salvando o dataframe com os resultados como um arquivo .xlsx
# 
write_xlsx(info_res4,"C:/Users/caetano-garcia/Desktop/R/FASE/RESULTADOS/RESULTADO.4.POR.ANO.xlsx")

