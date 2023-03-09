ui <- navbarPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  collapsible = TRUE,
  windowTitle = "Adesão ao Programa Nacional de Redução das Filas - Planos Estaduais",
  title = div(bs_button(icon("bars"), button_type = "primary") %>%
                bs_attach_collapse("lateral_filtros"),
              tags$text("Programa Nacional de Redução das Filas - Planos Estaduais"),
              style = "color: rgb(19,81,180);
               font-weight: 600;"),
  header = absolutePanel(
    fixed = TRUE,
    style = "
      background-color: rgba(255,255,255,0.2);
      z-index: 800;
      padding: 0px;
    ",
    top = 20,
    left = "1%",
    width = "100%",
    height = "40px",

    tags$style("
            position: absolute;
            top: 1px;
            left: 30px;
             "

    ),
    bsplus::bs_collapse(
      "lateral_filtros",
      show = F,
      content = uiOutput(
        "barra_ferramentas")
    )),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "geralb.css")
  ),
  tags$style("
             .cbcontainer {
               display: inline-block;
             }

             .menorbarra {
             font-size: 0.7em;
             }
             .checkbox {
               text-align: right;
               display: inline-block;
             }

             .checkbox input {
               float: bottom;
               position: relative !important;
               margin: 5px !important;
             }

             .checkbox label {
               padding-left: 0px !important;
             }
             "),
  # footer contém o painel de créditos.
  footer = absolutePanel(
    fixed = TRUE,
    style = "
      background-color: rgba(255,255,255,0.2);
      z-index: 100;
      padding: 0px
    ",
    bottom = 2,
    right = "3vw",
    width = 460,
    height = "5vh",
    tags$table(
      tags$tr(
        tags$td(
          img(src = "ms_marca.png",
              height = "60vh")
        ),
        # tags$td(
        #   img(src = "https://grupo.pro.br/cgmind-simbolo.png",
        #       height = "20px",
        #       style = "
        #         -webkit-filter: grayscale(20%);
        #         filter: grayscale(20%)
        #   ")
        # ),
        tags$td(
          "CGMIND-SAES | ",
          span("©", style = "
              display: inline-block;
              text-align: right;
              margin: 0px;
              -moz-transform: scaleX(-1);
              -o-transform: scaleX(-1);
              -webkit-transform: scaleX(-1);
              transform: scaleX(-1);
              filter: FlipH;
              -ms-filter: 'FlipH'
            "
          ),
          "CC-BY-NC-SA 4.0",
          style = "font-size: 8px;"
        ),
        tags$td(paste("Atualizado em:",data_atualiza),style = "font-size:10px")

      )
    )
  ),
    tabsetPanel(
    id="principal",
    #style = "max-height:90vh; overflow-y: auto;",
    tabPanel(
   "Adesão e Plano inicial",
  flowLayout(theme=bslib::bs_theme(version = 4, bootswatch = "minty"),lang="pt-br",

    column(width=3,flashCardOutput("total_elab",width="200px",height="143px")),
    column(width=3,flashCardOutput("total_cib",width="200px",height="143px"),offset=1),
  column(width=3,flashCardOutput("total_plano",width="200px",height="143px"),offset=1),
    column(width=3,flashCardOutput("plano_analise",width="200px",height="143px"),offset=2),
    column(width=3,flashCardOutput("plano_ajustes",width="200px",height="143px"),offset=2),
    column(width=3,flashCardOutput("plano_aprovado",width="200px",height="143px"),offset=2),
column(12,tags$br())),
#column(12,tags$h2("Detalhe dos planos em elaboração")),
fluidRow(column(10,
       DT::DTOutput("tabeladrac"),height="auto")
,
column(1,tags$a(bs_button(icon("file-pdf")),target="_blank",href="relatorios/relatorio_adesao.pdf",
                download=paste0(system(paste("date -r",propostas[2],'"+%Y-%m-%d"'),intern=T),"-relatorio_de_adesao_pnrf.pdf"))))
),
tabPanel(
  "Filas",
  fluidRow(
    column(6,DT::DTOutput("top_filas")),
    column(6, ggiraphOutput("donut_subgrupo"))
  ),
  fluidRow(
    column(3,flashCardOutput("reducao_geral",width="200px",height="143px")),
    column(6,DT::DTOutput("menores_filas")))
),
  tabPanel(
    "Planos Estaduais",
    fluidRow(
      tags$br()
    )
  ),
  tabPanel(
    "Resultados s/ótica MS",
    fluidRow(
#      column(6,shinycssloaders::withSpinner(flashCardOutput("total_elab"))
#    )
  )),
  tabPanel(
    "Gestão",
fluidRow( fileInput("atualiza","Insira novo relatório SAIPS",      buttonLabel = "Navegar...",
                placeholder = "Nenhum arquivo selecionado",width="340px")
)
)
)
)
