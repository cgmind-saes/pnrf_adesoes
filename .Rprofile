options(repos = c(CRAN = "https://cloud.r-project.org"))
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx12228m"))
#options(download.file.method = "wininet")

Sys.umask("002")
library(utils)
#automatizado baseado em requ.R
pacotes <-  gsub(")","",read.delim("requ.R",sep = "(", header = F)[[2]])
pacotes <- pacotes[-1]

pacotesnovos <- pacotes[ !( pacotes %in% utils::installed.packages()[ , "Package" ] ) ]
#if( length(pacotesnovos) > 1) utils::install.packages( pacotesnovos )

quero_atualizar <- F

sapply(pacotes, function (x) {
  suppressPackageStartupMessages(require(x[[1]],character.only = T))})
rm(pacotes)
