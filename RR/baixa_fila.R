##Baixa fila

library(RSelenium)
baixafila <- function(uf = "BA",n_proposta = 170329) {

# Set the path to the custom download directory
download_dir <- "/home/borges/pRojetos/pnrf/dados/autosaips"

chrome_ops <-
  # list(
  #   chromeOptions =
    #list(prefs =
           list(
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
    #)
    #)

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


remDr$navigate(paste0("https://saips.saude.gov.br/proposta/cadastro/tipo/V/prop/",n_proposta))
Sys.sleep(5)
# encontrar e clicar no botão de próxima página
proxpag_button <- remDr$findElement(using = "xpath", "//input[@id='btnProximaPagina']")
proxpag_button$clickElement()
Sys.sleep(5)

# encontrar e clicar no botão de próxima página na nova visualização
proxpag_button <- remDr$findElement(using = "xpath", "//a[@href='#next']")
proxpag_button$clickElement()

Sys.sleep(10)
# encontrar e clicar no botão de visualizar o anexo
anexo_atend_button <- remDr$findElement(using = "xpath", "//*[@id='ui-accordion-requisitos-panel-0-p-1']/div[5]/div/div[2]/div/div/a")

#DF anexou INVERTIDO!!
if(uf=="DF"){
anexo_atend_button <- remDr$findElement(using="xpath","//*[@id='ui-accordion-requisitos-panel-0-p-1']/div[6]/div/div[2]/div/div/a")
}

anexo_atend_button$clickElement()

# aguardar o download do relatório

download_done <- FALSE
while (!download_done) {
  download_done <- length(list.files("~/Downloads", pattern = ".*\\.xlsx$")) > 0
  Sys.sleep(1)
}

# mover o arquivo para o diretório desejado
file_name <- list.files("~/Downloads", pattern = ".*\\.xlsx$", full.names = TRUE)
arqdef <- paste0("dados/filas/",list.files("dados/filas",pattern=paste0(uf,".*atendimento.*\\.xls")))
file.rename(file_name, arqdef)


# armazenar os cookies em uma variável
cukis <- remDr$getAllCookies()

# fechar o navegador

remDr$close()

cDriver$server$stop()

}


