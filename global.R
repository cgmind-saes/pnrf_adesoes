#####
## global.R:
## Painel para exibição dos dados de Tempo Médio de Permanência em internações hospitalares
## - Leitura dos dados
## - Preparação das variáveis para exibição
#####
Sys.setlocale("LC_ALL" , "pt_BR.utf8")
options(scipen=9999999)
#shinyOptions(cache = cachem::cache_disk("dados/cache/"))

## Carrega pacotes
source("requ.R")
source("R/utils.R")
subgrupos <- readRDS("dados/subgrupos.rds")
source("R/adesao_monitora.R")
data_atualiza <- system(paste("date -r",propostas[2],'"+%Y-%m-%d %H:%M:%S"'),intern=T)

data_atualiza <- format.Date(as.POSIXlt(data_atualiza,tryFormats = "%Y-%m-%d %H:%M:%S")-15400,"%a, %d de %b de %Y, %Hh%M",tz = "América/São Paulo")
## Carrega dados estáticos do painel
#source("R/estaticospainel.R")
print("Começando novamente agora com a versão atualizada do app")

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

