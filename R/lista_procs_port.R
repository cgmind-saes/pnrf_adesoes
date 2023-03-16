#lista de procedimentos publicada na portaria

atualwd <- getwd()

"https://www.in.gov.br/en/web/dou/-/portaria-n-237-de-8-de-marco-de-2023-469724986"
setwd(gsub("nrf.*$","nrf/",atualwd))
#portaria_lc <- "http://www.in.gov.br/en/web/dou/-/portaria-n-237-de-8-de-marco-de-2023-468776617"
#ISO8559-1
portaria_lc <- "https://www.in.gov.br/en/web/dou/-/portaria-n-237-de-8-de-marco-de-2023-469724986"



plp_loc <- "dados/lista_portaria_8_3_republicada.html"
download.file(portaria_lc,plp_loc)
pega_pedaco_tabela <- function(x) {
  rvest::html_element(xml2::read_html(plp_loc,encoding = "UTF-8"),xpath=paste0("//table[@class='dou-table'][",x,"]"))%>%
  rvest::html_table(header = F)
}

lista_proc_port1 <- rbindlist(lapply(1:8,pega_pedaco_tabela))
names(lista_proc_port1) <- c("CO_PROCEDIMENTO","no_proc_autorizado")

write_csv2(lista_proc_port1,gsub("html","csv",plp_loc))

lista_proc_port1%<>%mutate(CO_PROCEDIMENTO = as.numeric(gsub("[.-]","",CO_PROCEDIMENTO)))

write_csv2(lista_proc_port1,"dados/2023-03-13-lista_definitiva_procedimentos.csv")

setwd(atualwd)
