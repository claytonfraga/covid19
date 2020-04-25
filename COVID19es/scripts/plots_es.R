library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(leaflet)
library(sf)
library(RColorBrewer)
library(tidyverse)


################################################################################
## Parametros de formatacao comum aos plots
################################################################################
plot.formatos <- theme_bw()+
  theme(axis.text= element_text(size=12, face="bold"),
        axis.title.y = element_text(size=14, face="bold"))
        
################################################################################
## Evolucao de casos suspeitos, descartados e confirmados
################################################################################

evolucao.tipos.casos <-
  es.epid %>%
  filter(!is.na(Suspeitos)) %>%
  gather(Suspeitos:Óbitos, key = Classe, value = N.casos) %>%
  mutate(Classe = factor(Classe, levels =c("Óbitos", "Confirmados", "Suspeitos","Descartados"))) %>%
  ggplot(aes(dia,N.casos)) +
  geom_col(aes(fill=Classe)) +
  scale_x_date( date_labels = "%d/%b", name="") +
  ylab("Número de casos") +
  ggtitle("Evolução dos tipos de casos") +
  plot.formatos

ggplotly(evolucao.tipos.casos)
etcwid <- ggplotly(evolucao.tipos.casos)
htmlwidgets::saveWidget(etcwid,
                        file = "evolucao.tipos.casos.html",
                        selfcontained = FALSE, libdir = "libs")


################################################################################
## Proporção de suspeitos por região
################################################################################

reg.cases.pie <- plot_ly()

reg.cases.pie <- reg.cases.pie %>% add_pie(
                  reg.data, labels = reg.data$reg, values = reg.data$prop.sus, type = 'pie',
                  textposition = 'inside',
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',
                  text = paste(reg.data$n.sus, ' casos suspeitos'),
                  domain = list(row = 0, column = 0),
                  showlegend = FALSE)

reg.cases.pie <- reg.cases.pie %>% add_pie(
                  reg.data, labels = reg.data$reg, values = reg.data$prop.con, type = 'pie',
                  textposition = 'inside',
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',
                  text = paste(reg.data$n.con, ' casos confirmados'), 
                  domain = list(row = 0, column = 1),
                  showlegend = FALSE)

reg.cases.pie <- reg.cases.pie %>% layout(title = "Proporção de casos por região (SESA)", y = 0.95, showlegend = F,
                      font = list(size = 14),                    
                      grid=list(rows=1, columns=2),
                      annotations = (list(
                        list(
                          x = 0.225, 
                          y = 0.9, 
                          font = list(size = 14), 
                          text = "Casos suspeitos", 
                          xref = "paper", 
                          yref = "paper", 
                          xanchor = "center", 
                          yanchor = "bottom", 
                          showarrow = FALSE
                        ), 
                        list(
                          x = 0.775, 
                          y = 0.9, 
                          font = list(size = 14), 
                          text = "Casos confirmados", 
                          xref = "paper", 
                          yref = "paper", 
                          xanchor = "center", 
                          yanchor = "bottom", 
                          showarrow = FALSE))),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

reg.cases.pie
htmlwidgets::saveWidget(reg.cases.pie,
                        file = "reg.cases.pie.html",
                        selfcontained = FALSE, libdir = "libs")
                        
################################################################################
## Distribuição dos casos suspeitos
################################################################################

#Mapa obtido de: https://mapas.ibge.gov.br/
#Dados da legenda
bins <- c(0, 10, 50, 100, 200, 450) #adição manual, implementar automatização no futuro
paleta <- colorBin("YlGnBu", domain = es.mun.epi$casos.suspeitos, bins = bins, na.color = "#808080")

points <- filter(es.mun.epi, es.mun.epi$casos.confirmados >= 1)
#paleta2 <- colorNumeric(palette = "Reds",  domain = points$casos.confirmados)
  
legenda <- paste(
  "Município: <b>", es.mun.epi$NM_MUNICIP, "</b><br/>", 
  "Casos suspeitos: <b>", es.mun.epi$casos.suspeitos, "</b><br/>",
  "Suspeitos/mil hab: <b>", round((es.mun.epi$casos.suspeitos) *1000 / es.mun.epi$pop , 3), "</b>",  
  sep="") %>%
  lapply(htmltools::HTML)

legenda2 <- paste(
  "Município: <b>", points$NM_MUNICIP, "</b><br/>",
  "Casos confirmados: <b>", points$casos.confirmados, "</b><br/>", 
  "Confirmados/mil hab: <b>", round((points$casos.confirmados *1000) / points$pop , 3), "</b>",  
  sep="") %>%
  lapply(htmltools::HTML)

#Código para plotar o mapa

mapa <- leaflet(es.mun.epi) %>%
  addTiles()  %>% 
  setView( lat=-19.6805834305246, lng=-40.527048939827665 , zoom=7) %>%
  addPolygons( 
    fillColor = ~paleta(casos.suspeitos), 
    stroke=TRUE, 
    fillOpacity = 0.8, 
    color="black", 
    weight=0.3,
    label = legenda,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
   addCircleMarkers(data = points, lat = ~latitude, lng = ~longitude,
                    color = 'red', popup = legenda2,
                    radius = ~sqrt(casos.confirmados)) %>%
  addLegend( pal=paleta, values=~casos.suspeitos, opacity=0.9, title = "Casos suspeitos", position = "bottomleft" )

mapa
htmlwidgets::saveWidget(mapa,
                        file = "mapa.es.html",
                        selfcontained = FALSE, libdir = "libs")
                        
