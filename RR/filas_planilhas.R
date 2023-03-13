#Consolida Filas

pf <- "/home/borges/pRojetos/pnrf/dados/filas/"
filas_recebidas <- list.files(pf,pattern=".*tendimento.*xls*")





le_fila_cuf <- function(x) {
  if(grepl("xlsx",x)) {
  fila <- read_xlsx(paste0(pf,x),sheet = "Ident. Fila na UF",range="A3:F1591",n_max=1500)
  } else {
    fila <- read_xls(paste0(pf,x),sheet = "Ident. Fila na UF",range="A3:F1591",n_max=1500)
  }
  fila %<>%filter(`QUANTIDADE DE SOLICITAÇÕES NA FILA ATÉ DIA 31/12/22`!="")
  fila$UF <- substr(x,1,2)
  fila
}


filas <- rbindlist(lapply(filas_recebidas ,le_fila_cuf))


modelo_atendimento_procs <-
  read_xlsx("dados/plano-atendimento-perf-cir-eletiva-vrs-4a.xlsx",sheet = "PROCEDIMENTOS",range="C2:D1264",n_max=1500)


filas%<>%left_join(modelo_atendimento_procs,
                   by = c("CÓDIGO DO PROCEDIMENTO NO SIGTAP" = "CO_PROCEDIMENTO"))


filas%<>%mutate(CO_SUBGRUPO = paste0(0,substr(`CÓDIGO DO PROCEDIMENTO NO SIGTAP`,1,3)))
subgrupos_df <- data.frame("subgrupo" = names(subgrupos),"CO_SUBGRUPO" = subgrupos)

filas%<>%left_join(subgrupos_df )


##Procedimentos com problema do ES
es_prob <- match(TRUE,{
  match(filas$`CÓDIGO DO PROCEDIMENTO NO SIGTAP`,407040102,nomatch = F) &
    match(filas$UF,"ES",nomatch = F)
})

##Linha seguinte com problema tb
es_prob <- c(es_prob,es_prob+1)

filas[es_prob,]$`Qtde de cirurgias a serem feitas no prazo pactuado` <- c(384,266)


filas[is.na(filas$`Qtde de cirurgias a serem feitas no prazo pactuado`),]$`Qtde de cirurgias a serem feitas no prazo pactuado` <- 0

filas[!is.numeric(filas$`Qtde de cirurgias a serem feitas no prazo pactuado`),]$`Qtde de cirurgias a serem feitas no prazo pactuado` <- 0

