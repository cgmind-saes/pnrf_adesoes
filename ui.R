ui <- navbarPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  collapsible = TRUE,
  windowTitle = "Adesão ao Programa Nacional de Redução das Filas - Planos Estaduais",
  title = div(bs_button(icon("bars"), button_type = "primary") %>%
                bs_attach_collapse("lateral_filtros"),
              tags$text("Programa Nacional de Redução das Filas - Planos Estaduais")),
  header = absolutePanel(
    fixed = TRUE,
    style = "
      background-color: rgba(255,255,255,0.2);
      z-index: 800;
      padding: 0px;
    ",
    top = 20,
    left = "2%",
    width = "100%",
    height = "40px",

    tags$style("
            position: absolute;
            top: 1px;
            left: 1px;
             "

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
    right = "2%",
    width = 360,
    height = 40,
    tags$table(
      tags$tr(
        tags$td(
          img(src = "ms_marca.png",
              height = "35px")
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
          " CC-BY-NC SA 4.0",
          style = "font-size: 10px;"
        )
      )
    )
  ),
  absolutePanel(
    fixed= T,
    top = 60,
    width = "100%",
    height = "95%",
    sidebarLayout(
#          "Indicadores",
      sidebarPanel = bsplus::bs_collapse(
        "lateral_filtros",
        show = F,
        content = sidebarPanel(
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
          uiOutput("mesel"),
          width = 12
        )
      ),
  mainPanel = tabsetPanel(
    id="principal",
    #style = "max-height:90vh; overflow-y: auto;",
    tabPanel(
   "Adesão e Plano inicial",
  fluidPage(theme=bslib::bs_theme(version = 4, bootswatch = "minty"),lang="pt-br",
    fluidRow(
    column(3,flashCardOutput("total_elab",width="200px",height="133px")),
    column(3,flashCardOutput("total_cib",width="200px",height="133px"),offset=1),
    column(3,flashCardOutput("total_plano",width="200px",height="133px"),offset=1)),
fluidRow(column(12,tags$br())),
fluidRow(
  column(3,flashCardOutput("plano_analise",width="200px",height="133px"),offset=2),
  column(3,flashCardOutput("plano_aprovado",width="200px",height="133px"),offset=1)),
fluidRow(column(12,tags$br())),
fluidRow(
  column(6,DT::DTOutput("tabeladrac"),height="300px"))
)),
  tabPanel(
    "Resultados s/ótica MS",
    fluidRow(
#      column(6,shinycssloaders::withSpinner(flashCardOutput("total_elab"))
#    )
  )),
  tabPanel(
    "Gestão",
    absolutePanel(
      width="75%",
      top=50,
      height="88%",
      right="1.5%",
      style = "z-index: 100",
      fileInput("atualiza","Insira novo relatório SAIPS")
     #shinycssloaders::withSpinner(reactableOutput("rol_estab"))
    )),
#  width = 12
),
position = "right",
fluid = TRUE)
# ,
# )
# )
 )
)
