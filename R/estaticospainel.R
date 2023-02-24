recortes <- c("NO_REGIAO","CO_UF_SIGLA","CO_REGIAO_SAUDE","NU_MUN_HOSP","CO_CNES")
names(recortes) <- c("Região","UF","Região Saúde","Município","Estabelecimento")

cortes <- recortes


aih_piccolo <- readRDS("dados/aih_piccolo.rds")


filtros <- c(rep("todos",8),"CO_UF_SIGLA")
names(filtros) <- c("porte","sexo","cor",
                    "fx_et","fx_et_det","capitulo","capitulo2","especialidade",
                    "recorte")
recorte <- filtros[8]
#filtros[3] <- "TMP"
filtrospadrao <- filtros

cortes <- c("NO_REGIAO","CO_UF_SIGLA","CO_REGIAO_SAUDE","DS_MUN","CO_CNES")
names(cortes) <- c("Região","Unidade Federativa","Região de Saúde","Município","Estabelecimento")

#Faixas Etárias
etariadet <- c(0,29/365,1,10,15,seq(20,80,10),130)
etariagen <- c(0,1,15,60,130)

##Faixas etarias
c_det_id <- c(0,29/365,1,10,15,seq(20,80,10))
c_grp_id <- c(0,1,15,60)
lista_grp_id <- c(c_grp_id,130)
lista_det_id <- c(c_det_id,130)
for (i in 1:(length(lista_grp_id)-1)){
  if (i == length(lista_grp_id)-1) {
    names(lista_grp_id)[i] <- paste("Mais de", lista_grp_id[i],"anos")
  } else {
    names(lista_grp_id)[i] <- paste("De",lista_grp_id[i],"a",lista_grp_id[i+1],"anos")
  }
}
lista_grp_id <- lista_grp_id[-length(lista_grp_id)]

for (i in 3:(length(lista_det_id)-1)){
  if (i == length(lista_det_id)-1) {
    names(lista_det_id)[i] <- paste("Mais de", lista_det_id[i],"anos")
  } else {
    names(lista_det_id)[i] <- paste("De",lista_det_id[i],"a",lista_det_id[i+1],"anos")
  }
}
names(lista_det_id)[1] <- "Até 29 dias de vida"
names(lista_det_id)[2] <- "De 30 dias de vida a 1 ano"
lista_det_id <- lista_det_id[-length(lista_det_id)]

lista_idades <- unique(cut(1:130,breaks=c(c_det_id,130),labels = lista_det_id))
lista_grp_idades <- unique(cut(1:130,breaks=c(c_grp_id,130) , labels = c_grp_id))
lista_fx_det <-  sprintf("De %g até %g ano(s)",c_det_id[1:(length(c_det_id)-1)],c_det_id[2:length(c_det_id)])
lista_fx_det[1:2] <- c("Do nascimento a 29 dias","de 30 dias de vida a 1 ano")


portes <- c(20,50,100,250,500,1e5)
names(portes) <- c("Pequeno Porte I","Pequeno Porte II","Médio Porte I","Médio Porte II",
                   "Grande Porte I","Grande Porte II")
c_portes <- c(0,portes)


tipos_leitos <- read_csv("dados/sigtap_leitos.csv", col_names = F)[[1]]
names(tipos_leitos) <- janitor::make_clean_names(read_csv("dados/sigtap_leitos.csv", col_names = F)[[2]])


coretnia <- c(1,2,3,4,5,99)
names(coretnia) <- c("Branca","Preta","Parda","Amarela","Indígena","Sem informação")

sexos <-  c(1,2)
names(sexos) <- c("Masculino","Feminino")

lista_capcid_base <- read_csv2("dados/tabela_cid.csv")

#source("R/paletas.R")
#source("R/regioes_de_saude.R")
#source("RR/processamento_inicial.R")

capitulos <- unique(lista_capcid_base$capitulo)
names(capitulos) <- make_clean_names(unique(lista_capcid_base$cap_nm))
diagnosticos <- as.factor(unique(lista_capcid_base)$subcat_nm)
names(diagnosticos) <- make_clean_names(unique(lista_capcid_base$subcat_nm))


regioes <- as.factor(sort(unique(aih_piccolo$NO_REGIAO)))
levels(regioes) <- str_to_title(levels(regioes))
cidades <- as.factor(unique(aih_piccolo$DS_MUN))
cidades <- cidades[order(cidades)]
names(cidades) <- levels(cidades)[levels(cidades)%in%as.character(cidades)]
estabelecimentos <- as.factor(unique(aih_piccolo$NO_FANTASIA))
estabelecimentos <- estabelecimentos[order(estabelecimentos)]
names(estabelecimentos) <- levels(estabelecimentos)[levels(estabelecimentos)%in%as.character(estabelecimentos)]

ufs <- unique(aih_piccolo$DS_UF)
ufs <- as.factor(ufs)
names(ufs) <- str_to_title(as.character(ufs))
ufsa <- sort(ufs)
levels(ufsa) <- names(ufsa)

#indicadores
indicadores <- c("TMP","TMPP","pacientes","diarias","param_tmp")

dims_geradas_ccalc_tmp <- data.frame("codigo" = 1:10,"coluna" = c("periodos","local","indicador","porte","sexo","indi2",
                                                                  "fx_et","capitulo","capitulo2","especialidade"))

filtr_a_cdc <- data.frame(filtro = names(filtros), coldc = c("porte","CO_PACIENTE_SEXO","CO_PACIENTE_RACA_COR","fx_et",
                                                             "capitulo","capitulo2","fx_et_det","NU_ESPECIALIDADE","recorte"))

##apenas para preencher vazio em erro
periodos <- unique(aih_piccolo$DT_CMPT)
