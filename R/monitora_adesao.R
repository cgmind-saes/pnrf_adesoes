"https://saips.saude.gov.br/relatorios/gerar-relatorio-propostas"


#Lista arquivos
propostas <- list.files(path="dados/propostas_mon/",pattern = "*.xlsx",full.names = T)


base_propostas <- rbindlist(lapply(propostas,read_xlsx))

names(base_propostas) <- gsub("PLANO ESTADUAL DE REDUÇÃO DE FILAS DE CIRURGIAS ELETIVAS / ","",names(base_propostas))


base_propostas$estadual <- grepl("ESTADUAL",base_propostas$Fundo)




