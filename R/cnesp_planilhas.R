#Consolida info cnes das cnesp

atualwd <- getwd()
setwd("/home/borges/pRojetos/pnrf")
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


#procedimentos no rol

modelo_atendimento_procs <-
  read_xlsx("dados/plano-atendimento-perf-cir-eletiva-vrs-4a.xlsx",sheet = "PROCEDIMENTOS",range="C2:D1264",n_max=1500)
print("até aqui ok 2")

cnesp%<>%left_join(modelo_atendimento_procs,
                   by = c("CÓDIGO DO PROCEDIMENTO NO SIGTAP" = "CO_PROCEDIMENTO"))

cnesp%<>%mutate(CO_SUBGRUPO = paste0(0,substr(`CÓDIGO DO PROCEDIMENTO NO SIGTAP`,1,3)))
subgrupos_df <- data.frame("subgrupo" = names(subgrupos),"CO_SUBGRUPO" = subgrupos)

cnesp%<>%left_join(subgrupos_df )


