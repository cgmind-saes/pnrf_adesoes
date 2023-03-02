"https://saips.saude.gov.br/relatorios/gerar-relatorio-propostas"

estados_siglas <- read.csv("https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/master/csv/estados.csv")

#Lista arquivos
propostas <- list.files(path="dados/propostas_mon/",pattern = "*.xlsx",full.names = T)


base_propostas <- read_xlsx("dados/propostas_mon/relatorio_propostas.xlsx")

names(base_propostas) <- gsub("PLANO ESTADUAL DE REDUÇÃO DE FILAS DE CIRURGIAS ELETIVAS / ","",names(base_propostas))


base_propostas$estadual <- !grepl("MUNICIPAL",base_propostas$Fundo)


base_propostas$Cadastrador <-
  gsub("De","de",str_to_title(base_propostas$Cadastrador),ignore.case = F)





# plan_fila <- read_xlsx("dados/plano-atendimento-perf-cir-eletiva-vrs-4a.xlsx",skip=2)
#
#
# plan_procs <- read_xlsx("dados/plano-atendimento-perf-cir-eletiva-vrs-4a.xlsx",skip=1,
#                         sheet = "PROCEDIMENTOS")
#
#
plan_recs <- read_xlsx("dados/plano-atendimento-perf-cir-eletiva-vrs-4a.xlsx",
                       sheet = "gm ms 90")
#
# plan_exec <- read_xlsx("dados/plano-atendimento-perf-cir-eletiva-vrs-4a.xlsx", skip = 6,
#                        sheet = "Execução")
#
# plan_gestor <- read_xlsx("dados/plano-atendimento-perf-cir-eletiva-vrs-4a.xlsx",
#                          sheet = "GESTORES")
#
#
# plan_bd_gestor <- read_xlsx("dados/plano-atendimento-perf-cir-eletiva-vrs-4a.xlsx",
#                             sheet = "bd_gestor")
#
#
# plan_bd_cnes  <- read_xlsx("dados/plano-atendimento-perf-cir-eletiva-vrs-4a.xlsx",
#                            sheet = "bd_cnes")
#
# plan_bd_proced_cirur  <- read_xlsx("dados/plano-atendimento-perf-cir-eletiva-vrs-4a.xlsx",
#                            sheet = "bd_proced_cirur")
#
#
#
# plan_fis_cnes_compf <- read_xlsx("dados/plano-atendimento-perf-cir-eletiva-vrs-4a.xlsx",skip=2,
#                                  sheet = "Ident. CNES e Proced.",n_max=2)
#








monextradrac <- base_propostas%>%dplyr::filter(estadual == T)%>%
  select(-UF)%>%
  rename(UF = `UF do Fundo`)%>%select(UF,Cadastrador)%>%
  left_join(estados_siglas,by=c("UF"="uf"))%>%
  left_join(
    plan_recs%>%select(SIGLA,5)%>%rename(`Valor do recurso`=`Proporção Per Capita`),
            by=c("UF" = "SIGLA"))




monextradrac %<>% select(-latitude,-longitude)




# download.file("https://www.gov.br/saude/pt-br/composicao/saes/saips/plano-atendimento-perf-cir-eletiva-vrs-4a.xlsx","dados/plano-atendimento-perf-cir-eletiva-vrs-4a.xlsx",
#               method="auto")
