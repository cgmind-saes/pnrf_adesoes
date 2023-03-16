# Carregar o pacote RSelenium

library(RSelenium)
# Set the path to the custom download directory
download_dir <- "/home/borges/pRojetos/pnrf/dados/autosaips"

chrome_ops <- list(
  browser.download.dir = download_dir,
  browser.download.folderList = 2L,
  browser.download.manager.showWhenStarting = FALSE,
  browser.download.manager.focusWhenStarting = FALSE,
  browser.download.manager.closeWhenDone = TRUE,
  download.default_directory = download_dir,
  download.prompt_for_download = FALSE,
  download.directory_upgrade = TRUE,
  safebrowsing.enabled = TRUE,
  browser.helperApps.neverAsk.saveToDisk = "text/html,
                                 text/html; charset=UTF-8,
                                 text/plain,
                                 text/csv,
                                 text/csv; charset=UTF-8,
                                 application/x-csv,
                                 application/csv,
                                 attachment/csv,
                                 application/excel,
                                 attachment/excel,
                                 application/text,
                                 application/json,
                                 attachment/json,
                                 application/vnd.ms-excel,
                                 application/vnd.ms-excel.addin.macroenabled.12,
                                 application/vnd.ms-excelsheet.binary.macroenabled.12,
                                 application/vnd.ms-excel.template.macroenabled.12,
                                 application/vnd.ms-excel.sheet.macroenabled.12,
                                 application/zip,
                                 application/xhtml+xml,
                                 application/xml,
                                 application/octet-stream",
  plugin.scan.plid.all = FALSE,
  browser.popups.showPopupBlocker = FALSE,
  browser.popups.maxPopupCount = 0L,
  dom.block_multiple_popups = FALSE,
  privacy.popups.disable_from_plugins = 0L)

# Start the remote driver with the custom preferences

cDriver <- rsDriver(browser = "chrome",extraCapabilities = chrome_ops)

# abrir o navegador e ir para a página de login
remDr <- cDriver[["client"]]
remDr$navigate("https://saips.saude.gov.br/autenticacao")

# preencher o formulário de login
usuario <- Sys.getenv("usuariosaips")
senha <- Sys.getenv("senhasaips")

# encontrar os campos de e-mail e senha e preenchê-los
usuario_field <- remDr$findElement(using = "name", "nu_cpf_cnpj_pessoa")
usuario_field$sendKeysToElement(list(usuario))

senha_field <- remDr$findElement(using = "name", "ds_senha")

senha_field$sendKeysToElement(list(senha))

# encontrar e clicar no botão de login
login_button <- remDr$findElement(using = "xpath", "//input[@type='submit']")
login_button$clickElement()

# armazenar os cookies em uma variável
cukis <- remDr$getAllCookies()

dados_postar <- list("BIGipServerphp_55_prod_https" =  cukis[[1]]$value, "PHPSESSID" = cukis[[2]]$value,
                     "filtro[co_seq_proposta]"="",
                     "filtro[co_uf]"="",
                     "filtro[co_municipio]"="",
                     "filtro[co_cnes]"="",
                     "filtro[nu_cnpj]"="",
                     "filtro[dt_envio_inicio]"="",
                     "filtro[dt_envio_fim]"="",
                     "filtro[dt_parecer_inicio]"="",
                     "filtro[dt_parecer_fim]"="",
                     "filtro[co_area_tecnica]"=163,
                     "filtro[co_situacao]"="",
                     "filtro[co_rede_programa]"=950,
                     "filtro[co_componente_servico]"=2278,
                     "st_requisitos"=1)


remDr$navigate("https://saips.saude.gov.br/relatorios/proposta/")

script <- paste0("
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '", "https://saips.saude.gov.br/relatorios/gerar-relatorio-propostas", "', true);
  xhr.responseType = 'blob';",
                 "xhr.onload = function(e) {",
                 "  if (this.status == 200) {",
                 "    var blob = this.response;",
                 "    var a = document.createElement('a');",
                 "    a.href = window.URL.createObjectURL(blob);",
                 "    a.download = 'relatorio_propostas.xlsx';",
                 "    a.style.display = 'none';",
                 "    document.body.appendChild(a);",
                 "    a.click();",
                 "  }",
                 "};",

  "xhr.setRequestHeader('Cookie', '",cukis[[1]]$name,"=",cukis[[1]]$value,";",cukis[[2]]$name,"=",cukis[[2]]$value,"');
  xhr.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded; charset=UTF-8');
  xhr.setRequestHeader('X-Referer','https://saips.saude.gov.br/relatorios/proposta/');
  xhr.send('filtro[co_area_tecnica]=163&filtro[co_rede_programa]=950&filtro[co_componente_servico]=2278&filtro[st_requisitos]=1&submit=true');
")
remDr$executeScript(script)


# aguardar o download do relatório
download_done <- FALSE
while (!download_done) {
  download_done <- length(list.files("~/Downloads", pattern = ".*\\.xlsx$")) > 0
  Sys.sleep(1)
}

# mover o arquivo para o diretório desejado
file_name <- list.files("~/Downloads", pattern = ".*\\.xlsx$", full.names = TRUE)
arqdef <- "dados/propostas_mon/relatorio_propostas.xlsx"
data_atualiza <- system(paste("date -r",arqdef,'"+%Y-%m-%d %H:%M:%S"'),intern=T)
file.rename(arqdef,gsub("_mon/",paste0("_mon/",data_atualiza,"-"),arqdef))
file.rename(file_name, arqdef)


# armazenar os cookies em uma variável
cukis <- remDr$getAllCookies()

# fechar o navegador

remDr$close()
cDriver$server$stop()
