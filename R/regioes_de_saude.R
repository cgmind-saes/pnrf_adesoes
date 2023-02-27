#get_pg <- "https://sage.saude.gov.br/paineis/regiaoSaude/lista.php?output=jsonbt&=&order=asc&_=1659576808022"


#lista_regioes <- fromJSON(GET(get_pg) %>% content(as = "text", encoding = "utf-8"))

#lista_regioes <- rbindlist(lista_regioes)

#regioes_de_saude <- lista_regioes$co_colegiado

#names(regioes_de_saude) <- lista_regioes$no_colegiado

#vma <- read_csv("https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/main/csv/estados.csv")

#lista_regioes <- lista_regioes%>%left_join(vma%>%select(uf,regiao))

#names(lista_regioes)[4:6] <- c("CO_REGIAO_SAUDE","NO_REGIAO_SAUDE","NO_REGIAO")

#lista_regioes$regiao <- paste("Região",lista_regioes$regiao)
#lista_regioes <- lista_regioes%>%mutate(across(c("CO_REGIAO_SAUDE","ibge"),as.numeric))

lista_regioes <- readRDS("dados/lista_regioes.rds")

regioes_de_saude <- readRDS("dados/regioes_de_saude.rds")
