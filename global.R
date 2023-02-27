#####
## global.R:
## Painel para exibição dos dados de Tempo Médio de Permanência em internações hospitalares
## - Leitura dos dados
## - Preparação das variáveis para exibição
#####
shinyOptions(cache = cachem::cache_disk("dados/cache/"))

## Carrega pacotes
source("requ.R")
## Carrega dados estáticos do painel
#source("R/estaticospainel.R")
#source("R/monitora_adesao.R")

##Funções auxiliares
plotatmp <- function(recorte = "NO_REGIAO",dados = aih_piccolo) {
  nomrc <- names(recortes[recortes == recorte])
  dados$periodo <- as.yearmon(dados$DT_SAIDA)
  dados <- dados%>%dplyr::filter(lubridate::year(DT_INTERNACAO)>2017)%>%
    group_by(periodo,!!as.name(recorte))%>%
    summarize(tempo_medio_permanencia = round(mean(QT_DIARIAS),3))
  names(dados)[2] <- nomrc
  p <- ggplot(dados,aes(x=periodo,y=tempo_medio_permanencia,col = !!as.symbol(nomrc)))

  p <- p +
    geom_line( size = 0.5)  +
    geom_line(size = 0.5) +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_text(size = 8, colour = "grey60"),
          legend.position='bottom'
          # ,
          # legend.title = element_blank()
          )

  ggplotly(p) %>%
    plotly::layout(legend = list(title = "", orientation = "h", y=-0.1))

}
