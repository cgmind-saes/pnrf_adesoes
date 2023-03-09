#Consolida Filas

pf <- "dados/filas/"
filas_recebidas <- list.files(pf,pattern=".*tendimento.*xls*")





le_fila_cuf <- function(x) {
  if(grepl("xlsx",x)) {
  fila <- read_xlsx(paste0(pf,x),sheet = "Ident. Fila na UF",range="A3:F1591",n_max=1500)
  } else {
    fila <- read_xls(paste0(pf,x),sheet = "Ident. Fila na UF",range="A3:F1591",n_max=1500)
  }
  fila %<>%filter(`Prazo para reduzir o % proposto (em meses)`!="")
  fila$UF <- substr(x,1,2)
  fila
}


filas <- rbindlist(lapply(filas_recebidas ,le_fila_cuf))


filas%<>%left_join(lista_proced_base%>%mutate(across(CO_PROCEDIMENTO,as.numeric)),by = c("CÓDIGO DO PROCEDIMENTO NO SIGTAP" = "CO_PROCEDIMENTO"))

subgrupos_df <- data.frame("subgrupo" = names(subgrupos),"CO_SUBGRUPO" = subgrupos)
filas%<>%left_join(subgrupos_df )
