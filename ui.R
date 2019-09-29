
source("./src/R/libs.R")

options(spinner.size=0.5)
Produtos_categoria2 <- read_delim("Produtos-categoria.txt",
 ";", escape_double = FALSE, trim_ws = TRUE)
options(spinner.size=0.5)
htmlTemplate("index.html",
     button = actionButton(inputId = "btn-add-1", "Adicionar item"),
     FileInputjrs = fileInput("file1", "Carregue o arquivo",
        multiple = TRUE,
        accept = c("text/csv",
           "text/comma-separated-values,text/plain",
           ".csv")),
     slider = sliderInput(inputId = "TESTE", "teste",min=0 ,max=10, 1, step=0.2),
     textinpt = textInput("teste","teste","Entre com a busca"),
             ##tab1 = withSpinner(DT::dataTableOutput("table1"), type=6,color="#00b300"),
     tab2 = withSpinner(DT::dataTableOutput("table2",width="200%"), type=6,color="#00b300"),
     seletor = selectInput("seletor", "selecione", choices=unique(colnames(mtcars)), width ="40px"),
     kpi1 = withSpinner(plotlyOutput("grafico1", height = "350px"), type=6,color="#00b300"),
     kpi2 = withSpinner(plotlyOutput("grafico2", height = "350px"), type=6,color="#00b300"),
     kpi4 = withSpinner(plotlyOutput("graphinput", height = "350px"), type=6,color="#00b300"),
     kpi5 = withSpinner(plotlyOutput("graphinput2", height = "350px"), type=6,color="#00b300"),
     kpi6 = withSpinner(plotlyOutput("KPIdados", height = "350px"), type=6,color="#00b300"),
            ##kpi2 = withSpinner(plotlyOutput("reativos", height = "299px"), type=6,color="#00b300"),
     kpi3 = withSpinner(plotlyOutput("reativoss", height = "299px"), type=6,color="#00b300"),
     confianca_seletor=numericInput("conf","Confianca",value=0.5,step=0.001),
     suporte_seletor=numericInput("sup","Suporte",value=0.0001,step=0.001),
     info = infoBox("Transações analisadas", value = textOutput("qtd",container=a), icon = icon("credit-card")),
     info2 = infoBox("Recomendações geradas",value = textOutput("qtdRegras",container=a), icon = icon("shopping-bag")),
     engrenagem = icon("cogs"),
     menuitem1 = icon("shopping-bag"),
     menuitem2 = icon("shopping-cart"),
     menuitem3 = icon("question-circle"),
     colnamestab =  verbatimTextOutput("texto"),
     
     entrada1=orderInput('source', 'Produtos Recomendados', items = NULL,
           as_source = TRUE, connect = 'dest'),
     saida1 = orderInput('dest', 'Adicionar', items = NULL, placeholder = 'Drag items here...'),
     ll =verbatimTextOutput('order') ,
     ll2= verbatimTextOutput("textoselecao"),
     rmbt = actionButton("rm-prod-1","remova"),
     btnRecomend=actionButton("btnrecup","atualizar recomendacao"),
     produtosAdd =uiOutput("VarsInput") ,
     prodrecomendado2= uiOutput("VarsInput2"),
     desconto = numericInput("offR","Selecione a taxa de desconto sob o valor do produto (%)",value=5,step=1)
     )


