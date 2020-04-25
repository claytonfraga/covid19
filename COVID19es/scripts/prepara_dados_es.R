library(sf)
library(tidyverse)

#Dados epidemiológicos diários
es.raw <- read.csv("./dados/covides.csv", as.is = TRUE, encoding = "UTF-8")
es.epid <- es.raw[, c(1,4,3,5,6)]
names(es.epid) <- c("dia","Suspeitos","Confirmados","Descartados","Óbitos")
es.epid$dia <- as.Date(es.epid$dia, "%Y-%m-%d")


#Proporção de casos por regiões da SESA
reg.data <- data.frame(
  reg = c("Metropolitana", "Central", "Norte", "Sul"),
  n.sus = c(501, 111, 13, 49), #atualizar valores de suspeitos diariamente
  n.con = c(104, 9, 3, 4)) #atualizar valores de confirmados diariamente
reg.data <- reg.data %>% 
  arrange(desc(reg)) %>%
  mutate(prop.sus = n.sus / sum(reg.data$n.sus) *100) %>%
  mutate(ypos.sus = cumsum(prop.sus)- 0.5*prop.sus ) %>%
  mutate(prop.con = n.con / sum(reg.data$n.con) *100) %>%
  mutate(ypos.con = cumsum(prop.con)- 0.5*prop.con )
  
#Casos por municípios - mapa
#Para obter o mapa já com a informação do tamanho da população
es_df <- read_rds(file.path('./maps/', "es.mun.epi.rds"))

#Para obter os dados epidemiológicos já atualizadps
es.cases.mun <- read.csv("./dados/es.cases.mun.csv", as.is = TRUE, encoding = "UTF-8") 

#Juntar mapa e dados epidemiológicos
es.mun.epi <- merge(es_df, es.cases.mun, by.x ='NM_MUNICIP', by.y = 'nome', all=T)
