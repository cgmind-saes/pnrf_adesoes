#Consolida Filas

atualwd <- getwd()

setwd(gsub("nrf.*$","nrf/",atualwd))
pf <- "dados/filas/"
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


#procedimentos no rol

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




##Gambis


#tabulizer::locate_areas("dados/filas/2023_03_10_Plano_atendimento_perf_cir_eletiva_vrs_4a_Distrito_Federal.pdf",pages = 1)

# colunasdf <- list(c(36.71685,107.21246,542.22199,594.95042,709.00430,794.40142))
#
#
# fila_df <- tabulizer::extract_tables("dados/filas/2023_03_10_Plano_atendimento_perf_cir_eletiva_vrs_4a_Distrito_Federal.pdf",pages = 1,
#                                      columns=colunasdf,guess=F)
#
#
# fila_df <- fila_df[[1]][8:31,c(2:6)]
#
# fila_df <- as.data.frame(fila_df)
#
# tira_esp <- function(x){
#   gsub(" ","",x)
# }
#
# tira_perc <- function(x){
#   x <- gsub("%","",x)
#   x <- gsub("\\.","",x)
#   x <- as.numeric(x)/100
# }
#
# fila_df%<>%mutate(across(3,tira_esp),across(5,as.numeric),across(3,tira_perc))
#
# names(fila_df) <- names(filas)[c(1:3,5,6)]
#
# fila_df$`QUANTIDADE DE SOLICITAÇÕES NA FILA ATÉ DIA 31/12/22` <-
#   round(fila_df$`QUANTIDADE DE SOLICITAÇÕES NA FILA ATÉ DIA 31/12/22`)
#
# fila_df%<>%mutate(UF="DF", `Redução do Tamanho da Fila (%)` = `Qtde de cirurgias a serem feitas no prazo pactuado`/`QUANTIDADE DE SOLICITAÇÕES NA FILA ATÉ DIA 31/12/22`,
#                   `CÓDIGO DO PROCEDIMENTO NO SIGTAP`=as.numeric(`CÓDIGO DO PROCEDIMENTO NO SIGTAP`))
#
#
# fila_df%<>%left_join(modelo_atendimento_procs,
#                      by = c("CÓDIGO DO PROCEDIMENTO NO SIGTAP" = "CO_PROCEDIMENTO"))
#
#
# fila_df%<>%mutate(CO_SUBGRUPO = paste0(0,substr(`CÓDIGO DO PROCEDIMENTO NO SIGTAP`,1,3)))
#
#
# fila_df%<>%left_join(subgrupos_df)
#
#
#
# filas%<>%bind_rows(fila_df)


### CE estadual
fila_ce <- read_csv2("dados/filas/ce_fila_prel.csv")

fila_ce_junta <- fila_ce[c(2,3,5)]

names(fila_ce_junta) <- names(filas)[1:3]
fila_ce_junta$`Redução do Tamanho da Fila (%)` <- 1
fila_ce_junta$UF <- "CE"
fila_ce_junta$`Qtde de cirurgias a serem feitas no prazo pactuado` <- fila_ce_junta[[3]]



fila_ce_junta%<>%left_join(modelo_atendimento_procs,
                     by = c("CÓDIGO DO PROCEDIMENTO NO SIGTAP" = "CO_PROCEDIMENTO"))


fila_ce_junta%<>%mutate(CO_SUBGRUPO = paste0(0,substr(`CÓDIGO DO PROCEDIMENTO NO SIGTAP`,1,3)))


fila_ce_junta%<>%left_join(subgrupos_df)

fila_ce_junta <- fila_ce_junta[!is.na(fila_ce_junta$NO_PROCEDIMENTO),]

source("R/lista_procs_port.R")


lista_proc_port1$CO_PROCEDIMENTO <- as.numeric(gsub("[.-]","",lista_proc_port1$CO_PROCEDIMENTO))
fila_ce_junta <- fila_ce_junta[fila_ce_junta$`CÓDIGO DO PROCEDIMENTO NO SIGTAP` %in% lista_proc_port1$CO_PROCEDIMENTO,]

filas%<>%bind_rows(fila_ce_junta)





##Totais por estado
fila_mn <- filas%>%group_by(UF)%>%
  summarize(cirurgias=sum(round(`Qtde de cirurgias a serem feitas no prazo pactuado`,0),na.rm=T),
            fila=sum(`QUANTIDADE DE SOLICITAÇÕES NA FILA ATÉ DIA 31/12/22`,na.rm=T))

fila_mn <- rbind(fila_mn,fila_mn%>%ungroup()%>%summarize(UF="Total",cirurgias=sum(cirurgias),fila=sum(fila)))


pf <- "dados/filas/"
cnesp_recebidas <- list.files(pf,pattern=".*tendimento.*xls*")





le_cnesp_cuf <- function(x) {
  if(grepl("xlsx",x)) {
    cnesp <- read_xlsx(paste0(pf,x),sheet = "Ident. CNES e Proced.",range="A3:I15000",n_max=10000)
  } else {
    cnesp <- read_xls(paste0(pf,x),sheet = "Ident. CNES e Proced.",range="A3:I15000",n_max=10000)
  }

  cnesp %<>%dplyr::filter(8 !="")
  cnesp$UF <- substr(x,1,2)
  print(cnesp$UF)
  cnesp
}

print("até aqui ok 1")
cnesp <- rbindlist(lapply(cnesp_recebidas ,le_cnesp_cuf),fill=TRUE)



cnesp%<>%left_join(modelo_atendimento_procs,
                   by = c("CÓDIGO DO PROCEDIMENTO NO SIGTAP" = "CO_PROCEDIMENTO"))

cnesp%<>%mutate(CO_SUBGRUPO = paste0(0,substr(`CÓDIGO DO PROCEDIMENTO NO SIGTAP`,1,3)))
subgrupos_df <- data.frame("subgrupo" = names(subgrupos),"CO_SUBGRUPO" = subgrupos)

cnesp%<>%left_join(subgrupos_df )

setwd(atualwd)

