"https://saips.saude.gov.br/relatorios/gerar-relatorio-propostas"


#Lista arquivos
propostas <- list.files(path="dados/propostas_mon/",pattern = "*.xlsx",full.names = T)


base_propostas <- rbindlist(lapply(propostas[1],read_xlsx))

names(base_propostas) <- gsub("PLANO ESTADUAL DE REDUÇÃO DE FILAS DE CIRURGIAS ELETIVAS / ","",names(base_propostas))


base_propostas$estadual <- grepl("ESTADUAL",base_propostas$Fundo)




#dados extra do DRAC
# adet <- "dados/propostas_mon/2023-02-24-adesao-detalhe.pdf"
# #ocrmypdf -l por 2023-02-24-monitoramento-extra.pdf  2023-02-24-adesao-detalhe.pdf
# noarea <- tabulizer::locate_areas(adet)
# nearea <- tabulizer::locate_areas(adet)
# coarea <- tabulizer::locate_areas(adet)
# adesao_detalhe <- tabulizer::extract_tables(adet)
#
# adesao_detalhe[[5]] <- adesao_detalhe[[5]][,-2]
#
# adesao_detalhe[[1]] <- tabulizer::extract_tables(adet,guess=F,area=noarea)[[1]][-1:-3,]
#
# adesao_detalhe[[2]] <- tabulizer::extract_tables(adet,guess=F,area=nearea)[[1]][-1,]
#
# adesao_detalhe[[2]] %<>% as_tibble()%>%separate(7,c("a","b"),sep= " ")
#
# adesao_detalhe[[3]] <- tabulizer::extract_tables(adet,
#                                                  guess=F,area=coarea)[[1]][-1,]
#
# adesao_detalhe[[3]] <- adesao_detalhe[[3]][,-2]
#
# adesao_detalhe[[4]] <- adesao_detalhe[[4]][-1,]

# fcrenom <- function(x) {
#   x <- as_tibble(x)
#   names(x) <- c("UF","Cadastro SAIPS", "captação filas",
#                 "definição hospitais", "distribuição recurso",
#                 "data cib", "primeiro contato", "data video")
#   x
# }
#
# adesao_detalhe <- rbindlist(lapply(adesao_detalhe,fcrenom))





