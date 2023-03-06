"https://saips.saude.gov.br/relatorios/gerar-relatorio-propostas"

atualwd <- getwd()
setwd("/home/borges/pRojetos/pnrf")
estados_siglas <- read.csv("https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/master/csv/estados.csv")

#Lista arquivos
propostas <- list.files(path="dados/propostas_mon/",pattern = "*.xlsx",full.names = T)

if(!exists("base_propostas")){
base_propostas <- read_xlsx("dados/propostas_mon/relatorio_propostas.xlsx")


data_atualiza <- system(paste("date -r",propostas[2],'"+%Y-%m-%d %H:%M:%S"'))

names(base_propostas) <- gsub("PLANO ESTADUAL DE REDUÇÃO DE FILAS DE CIRURGIAS ELETIVAS / ","",names(base_propostas))


base_propostas$estadual <- !grepl("MUNICIPAL",base_propostas$Fundo)


base_propostas$Cadastrador <-
  gsub("De","de",str_to_title(base_propostas$Cadastrador),ignore.case = F)


}


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








monextradrac <-
  base_propostas%>%dplyr::filter(estadual == T)%>%
  select(-UF)%>%
  rename(UF = `UF do Fundo`)%>%select(UF,`Nº da Proposta`,Situação,`Nome completo do Responsável pelo Cadastro:`,
                                      `Cargo:`,`Telefone: (xx) xxxx-xxxx`,`E-mail:`)%>%
  rename(Cadastrador=`Nome completo do Responsável pelo Cadastro:`,
         Cargo=`Cargo:`,Telefone=`Telefone: (xx) xxxx-xxxx`,email=`E-mail:`)%>%
  left_join(estados_siglas,by=c("UF"="uf"))%>%
  bind_rows(
    estados_siglas%>%
      rename(UF=uf)%>%
      anti_join(base_propostas%>%dplyr::filter(estadual==T),by = c("UF"="UF do Fundo"))%>%mutate(`Nº da Proposta` = 'n.d.',
                                       Cadastrador="n.d.",
                                       Situação= "Não iniciado",
                                       Cargo ='n.d.',Telefone ='n.d.',email ='n.d.'))%>%
  select(-latitude,-longitude)%>%
  left_join(
    plan_recs%>%select(SIGLA,5)%>%rename(`Valor do recurso`=`Proporção Per Capita`),
            by=c("UF" = "SIGLA"))%>%
  mutate(Situação = case_when(Situação == "Incompleta" ~ "Em elaboração",
                              T ~ Situação))





print(nrow(monextradrac))

# download.file("https://www.gov.br/saude/pt-br/composicao/saes/saips/plano-atendimento-perf-cir-eletiva-vrs-4a.xlsx","dados/plano-atendimento-perf-cir-eletiva-vrs-4a.xlsx",
#               method="auto")

setwd(atualwd)
