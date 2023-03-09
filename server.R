server <- function(input, output, session) {
  output$regiao <- renderPlotly({

    plotatmp(input$recorte)
  })%>% bindCache(input$recorte)

  output$rol_estab <- renderReactable({
    reactable({
      dados <- aih_piccolo
      dados$periodo <- as.yearmon(dados$DT_SAIDA)
      dados <- dados%>%dplyr::filter(year(DT_INTERNACAO)>2017)%>%
        select(NO_REGIAO,CO_UF_SIGLA,CO_REGIAO_SAUDE,DS_MUN,CO_CNES,periodo,QT_DIARIAS)%>%
        group_by(periodo,CO_CNES)%>%
        summarize(diarias = sum(QT_DIARIAS), pacientes= n(),TMP=mean(QT_DIARIAS),
                  `Região` = first(NO_REGIAO),
                  UF = first(CO_UF_SIGLA),
                  `Região de Saúde` = first(CO_REGIAO_SAUDE),
                  `Município` = first(DS_MUN))

      dados %<>% pivot_wider(names_from = periodo,values_from = c(diarias,pacientes), names_vary = "slowest")
      print(names(dados))
      dados
      },
      groupBy = c("Região","UF","Região de Saúde","Município"),
      columns = list(
        `Região` = colDef(aggregate = "unique"),
        TMP = colDef(aggregate =  JS("function(values, rows) {
        let totalTMP = 0
        let totalpac = 0
        rows.forEach(function(row) {
        for (let i = 5; i< row.length;i+=2) {

          totaldiarias += row[i]
          totalpac += row[i+1]
          return totaldiarias / totalpacientes
        }})

      }"), format = colFormat(separators = F))
        ),
      bordered = F)
  }
  )

  ##Funções auxiliares UI
  trocatab <- function(nomeinput,conexao = NULL) {
    if (is.null(conexao)) {
      conexao <- nomeinput
    }
    observeEvent(input[[nomeinput]], {
      updateNavbarPage(session = session, inputId = "geral", selected = conexao)
    })}

  output$corte_regional <- renderUI({
    lista_regioes <- as.character(cortes)
    names(lista_regioes) <- names(cortes)
    selectInput(
      "corte_reg",
      label = "Corte Regional",
      width = "100%",
      choices = lista_regioes,
      selected = NULL,
      multiple = FALSE)
  })#%>%bindCache(filtros())
  #%>%bindCache(input$geral)

  output$corte_procedimento <- renderUI({
    lista_especialidade <- c("todos",tipos_leitos)


    names(lista_especialidade) <- c("",str_to_title(gsub("_"," ",names(lista_especialidade[-1]))))
    selectInput(
      "corte_especialidades",
      label = "Leitos",
      width = "100%",
      choices = lista_especialidade,
      selected = lista_especialidade[1],
      multiple = FALSE)
  })#%>%bindCache(filtros())

  output$corte_porte <- renderUI({
    lista_portes <- c("todos",portes)
    names(lista_portes) <- c("",names(portes))
    selectInput(
      "corte_portes",
      label = "Porte",
      width = "100%",
      choices = lista_portes,
      selected = lista_portes[1],
      multiple = FALSE)
  })#%>%bindCache(filtros())

  output$corte_etnia <- renderUI({

    lista_etnia <- c("todos",coretnia)
    names(lista_etnia) <- c("",names(coretnia))
    selectInput(
      "corte_cor",
      label = "Cor/Etnia",
      width = "100%",
      choices = lista_etnia,
      selected = lista_etnia[1],
      multiple = FALSE)
  })#%>%bindCache(filtros())
  output$corte_idade <- renderUI({
    lista_idade <- c_grp_id

    oks <- sprintf("De %i até %i ano(s)",lista_idade[1:(length(lista_idade)-1)],lista_idade[2:length(lista_idade)])
    oks <- c("",oks,paste("Mais de ",lista_idade[length(lista_idade)]))
    lista_idade <- c("todos",lista_idade)
    names(lista_idade) <- oks

    selectInput(
      "corte_idades",
      label = "Idade",
      width = "100%",
      choices = lista_idade,
      selected = lista_idade[1],
      multiple = FALSE)
  })#%>%bindCache(filtros())

  output$corte_subgrupo <- renderUI({
    lista_subgrupo <- subgrupos
    # names(lista_subgrupo) <- unique(tabela_subgrupo$cap_nm)
    lista_subgrupo <- c("todos",lista_subgrupo)
    names(lista_subgrupo) <- c("",names(lista_subgrupo[-1]))
    selectInput(
      "corte_subgrupo",
      label = "Subgrupo",
      width = "100%",
      choices = lista_subgrupo,
      selected =lista_subgrupo[1],
      multiple = FALSE)
  })#%>%bindCache(filtros())
  output$corte_sexo <- renderUI({
    lista_sexos <- c("todos",sexos)
    names(lista_sexos) <- c("",names(sexos))
    selectInput(
      "corte_sexos",
      label = "sexos",
      width = "100%",
      choices = lista_sexos,
      selected = lista_sexos[1],
      multiple = F)
  } )#%>%bindCache(filtros())

  observeEvent(input$corte_reg, {
    confere <- input$corte_reg
    print("atualizando alternativas")
    asele <- listar_loc(confere)

    print("atualizadas alternativas tibble")
    if (is.factor(asele)) {
      nana <- levels(asele)[!duplicated(levels(asele))]
    } else {
      nana <- names(asele)
    }

    asele <- as.numeric(asele)
    names(asele) <- nana
    print(str(asele))

    updateSelectizeInput(session = session,inputId = "destaquereg",
                         choices = asele,
                         selected = asele[5], options = list(maxItems = 4),
                         server = T)
  })


  output$linha_destaque <- renderUI({
    a <- c("todos",regioes)
    names(a) <- c("Selecione regiões",str_to_title(regioes))
    selectizeInput(
      "destaquereg",
      label = "Locais a destacar:",
      width = "100%",
      choices = a,
      selected = a[2],
      multiple = TRUE
    )
  })%>%bindCache(input$corte_reg)


  output$corte_tag <- renderUI({
    temporalidades <- c(1,3,6,12,24)
    names(temporalidades) <- c("Mês","Trimestre","Semestre","Ano","Biênio")
    selectInput(
      "corte_tag",
      label = "Tempo prévio:",
      choices = temporalidades,
      selected=1
    )
  })

  output$mesel <- renderUI({
    dateRangeInput("periodos","Período",min = as.Date("2018-01-01","%Y-%m-%d"),
                     format = "dd/mm/yyyy",startview = "month",
                   language="pt",
                     max = as.Date("2022-04-01","%Y-%m-%d"),
                     start = as.Date("2019-01-01","%Y-%m-%d"),
                     end = as.Date("2021-12-01","%Y-%m-%d"))

  })

  output$destaque_idade <- renderUI({
    a <- c("todos",lista_fx_det)
    names(a) <- c("Selecione Faixas etárias",str_to_title(lista_fx_det))
    selectizeInput(
      "destaqueidade",
      label = "Faixas etárias a filtrar:",
      width = "100%",
      choices = a,
      selected = "",
      multiple = TRUE
    )
  })


  output$destaque_procedimento <- renderUI({
    #         axa <- c("todos",procedimentos)
    #         names(axa) <- c("Selecione diagnósticos",str_to_title(procedimentos))

    selectizeInput(
      "destaqueproc",
      label = "Procedimentos a filtrar:",
      width = "100%",
      choices = procedimentos,
      selected = NULL,
      multiple = TRUE
    )
  }) %>%bindCache(input$geral,input$corte_subgrupo)

  updateSelectizeInput(session, 'destaqueproc', choices = procedimentos, server = TRUE)


  output$barra_ferramentas <- renderUI({
    tagList(
    uiOutput("corte_regional"),
    uiOutput("linha_destaque"),
    uiOutput("corte_especialidade"),
    uiOutput("corte_sexo"),
    uiOutput("corte_etnia"),
    uiOutput("corte_idade"),
    div(bs_button(icon("receipt"),button_type="default")%>%bs_attach_collapse("idadet")),
    bs_collapse(id = "idadet",content = uiOutput("destaque_idade")),
    tags$br(),
    uiOutput("corte_subgrupo"),
    div(bs_button(icon("kit-medical"),button_type="default")%>%bs_attach_collapse("procedi")),
    checkboxInput("procedimento","Proc Sec."),
    bs_collapse(id = "procedi",content = uiOutput("destaque_procedimento")),
    uiOutput("corte_porte"),
    uiOutput("mesel")
)
  })



  output$total_elab <- renderFlashCard({
    dadel <- data.frame(front = c(as.character(nrow(base_propostas%>%dplyr::filter(estadual == T,Situação %in% c("Incompleta","A Liberar")))-2),"Em elaboração"),
                        back = c(
                          #paste(as.character(nrow(base_propostas%>%dplyr::filter(estadual == F))),"inválidas"),
                          "",
                                 paste("<td width=200px height=143px>",(base_propostas%>%
                                   dplyr::filter(estadual == T,Situação == "Incompleta")%>%
                                   select(`UF do Fundo`)%>%arrange())$`UF do Fundo`,
                                  collapse = "&nbsp;</td>")))
    flashCard(dadel,frontColor = paleta5[3],
              front_text_color = "white", backColor = paleta7[11])
  })

  output$total_cib <- renderFlashCard({
    dadel <- data.frame(front = c(as.character(nrow(base_propostas%>%dplyr::filter(estadual == T,`Anexe Resolução CIB ou Colegiado de Gestão:` != "---"))),"Aprovados na CIB"),
                        back = c("",
                                 paste("<td width=200px height=143px>",(base_propostas%>%
                                                                          dplyr::filter(estadual == T,`Anexe Resolução CIB ou Colegiado de Gestão:` != "---")%>%
                                                                          select(`UF do Fundo`)%>%arrange())$`UF do Fundo`,
                                       collapse = "&nbsp;</td>")))
    flashCard(dadel,frontColor = paleta2023[8],
              front_text_color = "white", backColor = paleta7[11])
  })

  output$total_plano <- renderFlashCard({
    dadel <- data.frame(front = c(as.character(nrow(base_propostas%>%dplyr::filter(estadual == T,`Plano de atendimento - Importe planilha de cumprimento do artigo 6 da PT.\nDisponível em https://www.gov.br/saude/pt-br/composicao/saes/saips/manuais-gerais-do-sistema-saips` != "---"))),"planos enviados ao SAIPS"),
                        back = c("",
                                 paste("<td width=200px height=143px>",(base_propostas%>%
                                                                          dplyr::filter(estadual == T,`Plano de atendimento - Importe planilha de cumprimento do artigo 6 da PT.\nDisponível em https://www.gov.br/saude/pt-br/composicao/saes/saips/manuais-gerais-do-sistema-saips` != "---")%>%
                                                                          select(`UF do Fundo`)%>%arrange())$`UF do Fundo`,
                                       collapse = "&nbsp;</td>")))
    flashCard(dadel,frontColor = paleta2023[2],
              front_text_color = "white", backColor = paleta2023[2])
  })

  output$plano_analise <- renderFlashCard({
    dadel <- data.frame(front = c(as.character(nrow(base_propostas%>%dplyr::filter(estadual == T,Situação == "Em análise"))),"planos em análise"),
                        back = c(as.character(nrow(base_propostas%>%dplyr::filter(estadual == T,Situação == "Enviada para o MS" ))),"enviadas mas ainda não em análise"))
    flashCard(dadel,frontColor = paleta2023[2],
              front_text_color = "white", backColor = paleta7[11])
  })

  output$plano_ajustes <- renderFlashCard({
    dadel <- data.frame(front = c(as.character(nrow(base_propostas%>%dplyr::filter(estadual == T,Situação %in% situacoes_propostas[c(4:5,21,25)]))),"planos em ajustes"),
                        back = c(as.character(nrow(base_propostas%>%dplyr::filter(estadual == T,!(Situação %in% situacoes_propostas[c(4:5,21,25)])))),"sem plano em ajustes"))
    flashCard(dadel,frontColor = paleta7[11],
              front_text_color = "white", backColor = paleta2023[2])
  })


  output$plano_aprovado <- renderFlashCard({
    dadel <- data.frame(front = c(as.character(nrow(base_propostas%>%dplyr::filter(estadual == T,grepl("Aprovada",Situação)))),"planos aprovados"),
                        back = c(as.character(27-nrow(base_propostas%>%dplyr::filter(estadual == T,grepl("Aprovada",Situação)))),"sem plano aprovado"))
    flashCard(dadel,frontColor = paleta2023[8],
              front_text_color = "white", backColor = paleta7[11])
  })

  output$tabeladrac <- DT::renderDT({
    names(monextradrac) <- str_to_sentence(names(monextradrac))

    monextradrac%<>%rename(UF=Nome,Região=Regiao)
    monextradrac$`Valor do recurso` <- paste("R$",format(monextradrac$`Valor do recurso`,nsmall=2,decimal.mark=",",big.mark=".",scientific=F))
    monextradrac$`Nº da proposta` <- paste("Nº",format(as.numeric(monextradrac$`Nº da proposta`),decimal.mark=",",big.mark=".",scientific=F))
    print(names(monextradrac))
    print(nrow(monextradrac))
    monextradrac%>%
      select(Região,UF,Situação,`Nº da proposta`,
             `Valor do recurso`,Cadastrador,Cargo,Telefone,Email)%>%
      arrange(Cadastrador %in% c("n.d.","---"),Região,UF)

  },options = list(language =  JS("{url: '//cdn.datatables.net/plug-ins/1.10.15/i18n/Portuguese-Brasil.json'}"),
                   pageLength = 5,
                   responsive=T,columnDefs= list(list(responsivePriority=1,targets=1),
                                                  list(responsivePriority= 10001, targets= 4),
                                                 list(responsivePriority= 10000, targets= 5),
                                                  list(responsivePriority= 2, targets= 2 ),
                                                 list(responsivePriority= 10004, targets= 6),
                                                 list(responsivePriority= 10003, targets= 7),
                                                 list(responsivePriority= 10002, targets= 8),
                                                 list(responsivePriority= 3, targets= 3 ))),
                   rownames= FALSE,caption = "Fonte: SAIPS",
  extensions="Responsive",selection="single")


  output$aih_uf <- renderPlotly(
    plotabanda_destaque(indicador="qt_procedimentos",paramin=0,paramax=1e2)
  )



  output$top_filas <- DT::renderDT({

    filamostra <- filas%>%
      filter(!is.na(`PROCEDIMENTO CIRÚRGICO`))%>%
      group_by(paste(`CÓDIGO DO PROCEDIMENTO NO SIGTAP`, str_to_sentence(`PROCEDIMENTO CIRÚRGICO`), sep =" - "))%>%
      summarize(Fila = sum(round(`QUANTIDADE DE SOLICITAÇÕES NA FILA ATÉ DIA 31/12/22`,0)),
        "Cirurgias a Realizar" = sum(round(`Qtde de cirurgias a serem feitas no prazo pactuado`,0)),
        UFs = toString((unique(UF))))%>%
      arrange(desc(Fila),desc(`Cirurgias a Realizar`))%>%
      mutate(across(where(is.numeric),fni))

    names(filamostra) <- c("Procedimento","Fila","Cirurgias a Realizar","UFs")
    filamostra
  },
  options = list(language=JS("{url: '//cdn.datatables.net/plug-ins/1.10.15/i18n/Portuguese-Brasil.json'}"),
                 pageLength = 5,
                 searching = F, info = F,
                 sdom = T),
  rownames= FALSE,caption = "Procedimentos - Top 5"
  )


  output$menores_filas <- DT::renderDT({

    filamostra <- filas%>%
      group_by(paste(`CÓDIGO DO PROCEDIMENTO NO SIGTAP`, str_to_sentence(`PROCEDIMENTO CIRÚRGICO`), sep =" - "))%>%
      summarize(Fila = sum(round(`QUANTIDADE DE SOLICITAÇÕES NA FILA ATÉ DIA 31/12/22`,0)),
                "Cirurgias a Realizar" = sum(round(`Qtde de cirurgias a serem feitas no prazo pactuado`,0)),
                UFs = toString((unique(UF))), .groups = 'drop')%>%
      arrange(Fila,`Cirurgias a Realizar`)%>%
      mutate(across(where(is.numeric),fni))

    names(filamostra) <- c("Procedimento","Fila","Cirurgias a Realizar","UFs")
    filamostra
  },
  options = list(language=JS("{url: '//cdn.datatables.net/plug-ins/1.10.15/i18n/Portuguese-Brasil.json'}"),
                 pageLength = 5,
                 searching = F, info = F),
    rownames= FALSE,caption = "Procedimentos - 5 menores filas"
  )

 output$reducao_geral <- renderFlashCard({
   dadel <- data.frame(front = c(fni(round(sum(filas$`Qtde de cirurgias a serem feitas no prazo pactuado`,na.rm=T))),"Cirurgias na fila"),
                       back = c(paste0(fnx(round(100*sum(filas$`Qtde de cirurgias a serem feitas no prazo pactuado`,na.rm=T)/
                                                   sum(filas$`QUANTIDADE DE SOLICITAÇÕES NA FILA ATÉ DIA 31/12/22`,na.rm=T),1)),"%"),"Redução Média"))

   flashCard(dadel,frontColor = paleta2023[2],
             front_text_color = "white", backColor = paleta2023[2])
 })


 output$donut_subgrupo <-  renderggiraph({ #need to update from renderPlot
    proporciona <- function(x){
      round(100*prop.table(x),2)
    }
   data <- filas%>%group_by(subgrupo)%>%summarize(cirurgias=round(sum(`Qtde de cirurgias a serem feitas no prazo pactuado`),0),
                                                     fila=round(sum(`QUANTIDADE DE SOLICITAÇÕES NA FILA ATÉ DIA 31/12/22`),0))

   dataprop <- data%>%mutate(across(-subgrupo,proporciona))%>%rename(fila_prop = fila,cirurgia_prop=cirurgias)

   data%<>%left_join(dataprop)
   data%<>%filter(!is.na(subgrupo))

   p <-  ggplot() + #assign as object in environment
     geom_bar_interactive(data = data, stat = "identity",
                          color = "white",
                          aes(x = 2, y = fila_prop, fill = subgrupo, tooltip = paste0(subgrupo,"\n",fni(fila), "\n", fila_prop, "%"))) + #Add tooltip to aes()
     coord_polar(theta = "y", start = 0)+
     scale_fill_manual(values = c(paleta2023,paleta7)) +
     theme_void() +theme(
       panel.background = element_rect(fill='transparent',color=NA),
       plot.background = element_rect(fill='transparent',color=NA),
       legend.position = "none")+
     xlim(.5, 2.5)+
     ggtitle("Procedimentos por subgrupo")

   ggiraph(code = print(p),tooltip_opacity = 0.8) #call object p

 })
}

