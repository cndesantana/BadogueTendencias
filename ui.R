library(shinydashboard)
library(leaflet)

header <- dashboardHeader(
  title = "Badogue Tendencias"
)

body <- dashboardBody(
  fluidRow(
    column(width = 3,
      box(width = NULL, status = "warning",
        p(
          class = "text-muted",
          paste("Nota: É preciso subir um arquivo no formato do Stilingue contendo a Polarização de comentários dos últimos meses.",
                "É preciso escolher a região da qual se deseja estudar a tendência de IS.",
                "O IS é definido pela razão: (Positivos + Negativos)/(Positivos - Negativos)"
          ),
          fileInput('file', 'Escolha o Arquivo EXCEL', accept=c('.xlsx')),
          textAreaInput("mes",label = "Mês referente aos dados",value = ""),
          textAreaInput("grupo",label = "Grupo para o qual quer gerar a figura",value = "")
        )
      )
    ),
    column(width = 9,
       box(width = NULL, status = "warning",
           plotOutput("tendencias",width="100%", height= 600),
           downloadButton("tendenciasTS","Download")
       )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
