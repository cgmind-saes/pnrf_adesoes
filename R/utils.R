### Obter do IBGE lista de regiões, estados e municípios

compdata <- function(valor) {
  as.Date(paste0(valor,"15"),"%Y%m%d")
}

library(rjson)
filter <- dplyr::filter
#municipios <- "https://servicodados.ibge.gov.br/api/v1/localidades/municipios"

br_mun <-  fromJSON(file = ifelse(grepl("relatorios/.*",getwd()),
                                        "../../dados/municipios.json",
                                        "dados/municipios.json"))

achata <- function(x){as.data.frame(br_mun[[x]])}

br_mun <- data.table::rbindlist(lapply(1:length(br_mun),achata))

br_mun$idt <- substr(br_mun$id,1,6)



##funções úteis
fnx  <- function(x){formatC(as.numeric(x), format="f", big.mark=".", decimal.mark = ",", digits = 1)}
fni <- function(x){formatC(as.numeric(x), format="d", big.mark=".", decimal.mark = ",",digits = 0)}



# pop_estadual <- datasus::ibge_projpop_bruf()
#
# pop_estadual <- pop_estadual[c("Unidade da Federação","Total")]
#
# pop_estadual  %<>%filter(`Unidade da Federação` != "TOTAL")
# pop_estadual %<>% separate(`Unidade da Federação`,3,into=c("n_UF","UF"))

pop_estadual <- readRDS( ifelse(grepl("relatorios/.*",getwd()),
                                "../../dados/popestadual.rds",
                                "dados/popestadual.rds"))

minnarm <- function(x) {
  min(x,na.rm=T)
}

maxnarm <- function(x) {
  max(x,na.rm=T)
}

meannarm <- function(x) {
  mean(x,na.rm=T)
}

mediannarm <- function(x) {
  median(x,na.rm=T)
}

formatareais <- function(x) {
  paste("R$",format(x,big.mark=".",decimal.mark=",",digits = 2,scientific = F))
}

formatavirg <- function(x) {
  format(x,digits=2,big.mark=".",decimal.mark=",",scientific = F)
}


#Funções auxiliares


##Leitura e gravação - carga
###Leitura e gravação de array em fst
read_fst_array <- function(file_name) {

  ft <- fst::read_fst(file_name)  # single column data.frame
  metaf <- paste0(file_name, ".meta")
  if(file.exists(metaf)) {
    meta_data <- readRDS(metaf)  # retrieve dim

    m <- ft[[1]]
    attr(m, "dim") <- meta_data$dim
    dimensiones <- length(meta_data$dim)
    meta_data <- meta_data[2:(dimensiones+1)]
    # lapply(1:dimensiones,function(d,f) {
    #   dimnames(f)[[d]] <- meta_data[[d]]
    # })
    dimnames(m) <- meta_data

    m} else {
      ft
    }
}

write_fst_array <- function(m, file_name) {

  # store and remove dims attribute
  dim <- attr(m, "dim")

  meta_data <- list(
    dim = dim
  )
  for (i in 1:length(dim)){
    meta_data[[i+1]] <- dimnames(m)[[i]]
  }

  # serialize tale and meta data
  attr(m, "dim") <- NULL
  fst::write_fst(data.frame(Data = m), file_name)
  saveRDS(meta_data, paste0(file_name, ".meta"))
}


###Filtrar e formatar pedaços de tb_aih
lepedaco_filtra <- function(narq) {
  a <- ifelse(narq == 0,1,0)
  pedaco2 <- read_csv2(
    paste0("dados/sepTB_AIH/TB_AIH-",sprintf("%03d",narq),".csv"),
    quote="\"",
    skip = a,
    col_names = nom_cols,
    col_types = tipodados,
    num_threads = 8,
    progress = T,
    col_select = all_of(c(colunas_neces,"CO_IDENT","CO_MODALIDADE_INTERNACAO","ST_SITUACAO"))
  )
  pedaco2 %<>% mutate(across(c(CO_IDENT,CO_MODALIDADE_INTERNACAO),as.numeric))
  pedaco2 %<>% filter(ST_SITUACAO == 0,CO_IDENT != 5, CO_MODALIDADE_INTERNACAO == 2)
  pedaco2 %<>% select(all_of(colunas_neces))
  pedaco2
}



#Formatos e adequação de colunas e listas de filtros
### Formatar para data um número YYYYmmdd
paradata <- function(num) {
  if (!is.null(dim(num))) {
    num <- apply(num,1, function(x){
      if(x < 1e7){x <- x*100+1}
      x})
  }else if (length(num)>1){
    num <- sapply(t(num), function(x){if(x < 1e7)
    {x <- x*100+1}
      x
    })
  }
}

#comtodos <- function(x) {x <- c(x,"Todas/os")}

todesadd <- function(x) {c(subdims[[x]],"todos")}
todesaddl <- function(x) {
  if(is.numeric(x)){
    d <- c(x,9e6)
    names(d)[length(d)] <- "todos"
  } else {
    d <- c(subdims[[x]],"todos")
    names(d)[length(d)] <- "todos"
  }
  d
}

lu <- function(x) {length(unique(x))}

damos <- function(x,tam = 6e6) {
  sample(x,tam, replace = T)
}


recortes <- c("NO_REGIAO","CO_UF_SIGLA","CO_REGIAO_SAUDE","DS_MUN","CO_CNES")
names(recortes) <- c("Região","UF","Região Saúde","Município","Estabelecimento")

listar_loc <- function(recorte) {unlist(case_when(
  recorte == "DS_MUN" ~ list(cidades),
  recorte == "CO_UF_SIGLA" ~ list(ufsa),
  recorte == "NO_REGIAO" ~ list(regioes),
  recorte == "CO_REGIAO_SAUDE" ~ list(regioes_de_saude),
  recorte == "CO_CNES" ~ list(estabelecimentos)
))
}

numa_loc <- function(filatr){
  unlist(case_when(
    filatr[length(filatr)] == "DS_MUN" ~ 0,
    filatr[length(filatr)] == "CO_UF_SIGLA" ~ 1e8,
    filatr[length(filatr)] == "NO_REGIAO" ~ 1e9,
    filatr[length(filatr)] == "CO_REGIAO_SAUDE" ~ 1e6,
    filatr[length(filatr)] == "CO_CNES" ~ 1e10,
    T ~ 1e9
  ))}

#Fonte:https://stackoverflow.com/questions/31152960/display-only-months-in-daterangeinput-or-dateinput-for-a-shiny-app-r-programmin
monthsRangeInput <- function(inputId, label, start = as.Date("2018-01-01","%Y-%m-%d"), end = as.Date("2021-01-01","%Y-%m-%d"),
                             min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month",
                             minviewmode="months", # added manually
                             weekstart = 0, language = "pt-br", separator = " a ", width = NULL) {

  # If start and end are date objects, convert to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(start, "Date"))  start <- format(start, "%b/%Y")
  if (inherits(end,   "Date"))  end   <- format(end,   "%b/%Y")
  if (inherits(min,   "Date"))  min   <- format(min,   "%b/%Y")
  if (inherits(max,   "Date"))  max   <- format(max,   "%b/%Y")
  print(class(start))
  htmltools::attachDependencies(
    div(id = inputId,
        class = "shiny-date-range-input form-group shiny-input-container",
        style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),

        controlLabel(inputId, label),
        # input-daterange class is needed for dropdown behavior
        div(class = "input-daterange input-group",
            tags$input(
              class = "input-sm form-control",
              type = "text",
              `data-date-language` = language,
              `data-date-weekstart` = weekstart,
              `data-date-format` = format,
              `data-date-start-view` = startview,
              `data-date-min-view-mode` = minviewmode, # added manually
              `data-min-date` = min,
              `data-max-date` = max,
              `data-initial-date` = start
            ),
            span(class = "input-group-addon", separator),
            tags$input(
              class = "input-sm form-control",
              type = "text",
              `data-date-language` = language,
              `data-date-weekstart` = weekstart,
              `data-date-format` = format,
              `data-date-start-view` = startview,
              `data-date-min-view-mode` = minviewmode, # added manually
              `data-min-date` = min,
              `data-max-date` = max,
              `data-initial-date` = end
            )
        )
    ),
    datePickerDependency
  )
}

`%AND%` <- function(x, y) {
  if (!is.null(x) && !is.na(x))
    if (!is.null(y) && !is.na(y))
      return(y)
  return(NULL)
}

controlLabel <- function(controlName, label) {
  label %AND% tags$label(class = "control-label", `for` = controlName, label)
}

# the datePickerDependency is taken from https://github.com/rstudio/shiny/blob/master/R/input-date.R
datePickerDependency <- htmltools::htmlDependency(
  "bootstrap-datepicker", "1.6.4", c(href = "shared/datepicker"),
  script = "js/bootstrap-datepicker.min.js",
  stylesheet = "css/bootstrap-datepicker3.min.css",
  # Need to enable noConflict mode. See #1346.
  head = "<script>
 (function() {
 var datepicker = $.fn.datepicker.noConflict();
 $.fn.bsDatepicker = datepicker;
 })();
 </script>")

cria_geom <- function(x,cores = paleta7) {
  geom_line(aes(y = !!sym(as.character(x))),alpha = 1, color = sample(1:12,1), size = 1)
}

plotabanda_destaque <- function(recorte = "Uf",destaque_reg = 3,
                                indicador,
                                paramin ,
                                paramax,
#                                filatr = filtros(),
                                btab = aih_por_uf,
                                datas = as.Date(c("2018-01-31","2023-01-31"))){

  #filtros <- gsub("todos","",sapply(filatr,as.character))
  #btab <- as.data.frame(btab[,,indicador,,,,,,,,])

#  limfiltro <- numa_loc(filatr)

  # if (is.null(datas) || 0 %in% dim(datas)||length(datas)==0||is.na(datas)) {
  #   datas = as.Date(c("2018-01-31","2022-03-31"))
  # }
  btab$valor = btab[[indicador]]

  contextos <- btab%>%group_by(Data)%>%summarize(mini=min(valor,na.rm = T),
                                                     mediana=median(valor,na.rm = T),
                                                     maxi=max(valor,na.rm = T),
                                                     media= mean(valor,na.rm = T))
  contextos <- as.data.table(contextos)
  btab$local <- btab$Uf
  #destaque_reg <- limfiltro+as.numeric(destaque_reg)
  destaques <- btab%>%filter(local %in% estados_siglas[destaque_reg,]$nome, Data>datas[1],Data<datas[2])%>%select(Data,local,valor)
  destaques <- as.data.frame(destaques)

  #dim_local <- listar_loc(recorte)

  if(0 %in% dim(destaques)){
    cd <- contextos
    print("sem destaques")
  } else {
   # njun <- data.frame(local = as.numeric(dim_local),nom_local = str_to_title(as.character(dim_local)))
#    njun$local <- njun$local+limfiltro
    destaques <- destaques[!duplicated(destaques[1:2]),] #%>%mutate(local = local-limfiltro)
    print(names(destaques))
#    destaques <- destaques%>%left_join(njun)
    #select(-Uf,-indicador)
    destaques%<>%pivot_wider(names_from = local,values_from = valor)

    cd <- destaques%>%left_join(contextos)
    cd <- as_tibble(cd)
  }
  print("Em plota banda destaque")

  ###PRODUZ A PLOTAGEM
  d <- ggplot(cd,aes(x = Data))+
    geom_line(aes(y = media),color = paleta3[1], size = 0.3, linetype = "longdash")+
    geom_line(aes(y = paramax),color = paleta4[5],size= 0.2, linetype = "dashed")+
    geom_line(aes(y = paramin),color = paleta3[5],size= 0.2, linetype = "dashed")+
    geom_line(aes(y = maxi), color = paleta3[4],alpha=0.5,linetype = "dotted")+
    geom_line(aes(y = mini),alpha=0.5,color = paleta3[5], size = 0.7,linetype = "dotted")+
    geom_line(aes(y = mediana),alpha=0.5,color = paleta6[2] ,linetype = "dashed",size = 0.5)+
    geom_ribbon(aes(ymin = mini, ymax = maxi,fill = maxi>mini),alpha = 0.1, show.legend = F)+
    scale_fill_manual(values = paleta6[c(10,11,1:9)])+
    ylab("Procedimentos no mês")+
    scale_x_date(breaks = "1 year",date_labels = "%b %Y")+
    xlab("Período de competência")+
    ggtitle(paste("Produção de procedimentos na PNRF","por ",recorte,
                  "\n",format.Date(first(cd$Data),"%B/%Y"),"a",format.Date(last(cd$Data),"%B/%Y")))+
    #    expand_limits(y=0)+
    theme_minimal()
  if(nrow(destaques)==0){d <- d} else{
    for (i in 1:length(destaque_reg)) d <- d+cria_geom(names(cd)[2:(ncol(cd)-4)][i],i)}

  #hide_guides(
    ggplotly(d)
#    )
}
