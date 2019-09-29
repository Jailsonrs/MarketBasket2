##------------------------##
##------------------------##
library("glue")
source("./src/R/libs.R")
source("transacoes.R")
source("./dataSummary.R")
source("./src/R/Modelo_produto.R")
source("./src/R/kpi1.R")
options(shiny.maxRequestSize = 500*1024^2)
##------------------------##
tema <- theme(
  plot.title=element_text(size=12)
  )

df <- economics %>%
select(date, psavert, uempmed) %>%
gather(key = "variable", value = "value", -date)

df3 <- data_summary(ToothGrowth, varname = "len", 
  groupnames = c("supp", "dose"))
##Convert dose to a factor variable
df3$dose = as.factor(df3$dose)

##------------------------##
## SHINY SERVER FUNCTION 
##------------------------##
function(input, output,session){


##------------------------##

## Arquivo de entrada 2017-3

##------------------------##

## OBTENDO CAMINHO DO ARQUIVO APÓS A LEITURA

  arquivo <- reactive({    
    infile <- input$file1
    if (is.null(infile)) {
      ##RETORNA NULL SE O ARQ NAO FOI CARREGADO
      return(NULL)}
      else{

      ##CASO CONTRARIO RETORNA O CAMINHO DO ARQUIVO E SALVA NO
      ##OBJETO REATIVO "arquivo"
        return(read_delim(infile$datapath,
          ";", escape_double = FALSE, trim_ws = TRUE))
      }
    })


##------------------------##

## Arquivo de saída - Regras

##------------------------##

  TabRegras <- reactive({
    if(is.null(arquivo())){return(NULL)}
    Alg(arquivo(), input$conf, input$sup)
  })


  output$VarsInput <- renderUI({
    selectInput(
      "cartAdd","Pesquise o produto desejado",
      choices=unique(TabRegras()[,1]),
      multiple = FALSE,
      selectize=TRUE
      )


  })

  output$VarsInput2 <- renderUI({
    selectInput("recprod2", "selecioe  o produto recomendado:",
      choices =TabRegras()[,4], selected = c("teste"))
  })

  observe({
    escolhas = subset(TabRegras(),as.character(TabRegras()$NomeProduto)==factor(input$cartAdd))[,4]
    updateSelectInput(session,"recprod2",
      choices=escolhas$`Nome Produto recomendado`)
  })

##-----------------------------------------------------------------------##
##-----------------------------------------------------------------------##
############################ SESSÃO DE KPI's ##############################
##-----------------------------------------------------------------------##

  output$grafico1 <-renderPlotly({
    if(is.null(arquivo())){return(NULL)}
    

    gp<-ggplotly(KPI(TabRegras())$kp1) %>%
    config(collaborate=FALSE,
     cloud=FALSE,
     displaylogo=FALSE,
     modeBarButtonsToRemove=c(
       "select2d",
       "sendDataToCloud",
       "pan2d",
       "resetScale2d",
       "hoverClosestCartesian",
       "hoverCompareCartesian",
       "lasso2d",
       "zoomIn2d",
       "zoomOut2d")
     )

    ##gp <- layout(gp, margin=list(t = 100),autosize = F)

    gp
  })

  output$grafico2 <-renderPlotly({
    if(is.null(arquivo())){return(NULL)}


    gp<- ggplotly(KPI(TabRegras())$kp2) %>%
    config(collaborate=FALSE,
     cloud=FALSE,
     displaylogo=FALSE,
     modeBarButtonsToRemove=c(
       "select2d",
       "sendDataToCloud",
       "pan2d",
       "resetScale2d",
       "hoverClosestCartesian",
       "hoverCompareCartesian",
       "lasso2d",
       "zoomIn2d",
       "zoomOut2d")
     )

  ##gp <- layout(gp, margin=list(t = 100),autosize = F)

    gp
  })

  output$graphinput <-renderPlotly({
    if(is.null(arquivo())){return(NULL)}


    gp<- ggplotly(KPIin(arquivo())$g1) %>%
    config(collaborate=FALSE,
     cloud=FALSE,
     displaylogo=FALSE,
     modeBarButtonsToRemove=c(
       "select2d",
       "sendDataToCloud",
       "pan2d",
       "resetScale2d",
       "hoverClosestCartesian",
       "hoverCompareCartesian",
       "lasso2d",
       "zoomIn2d",
       "zoomOut2d")
     )

  ##gp <- layout(gp, margin=list(t = 100),autosize = F)

    gp
  })

  output$graphinput2 <-renderPlotly({
    if(is.null(arquivo())){return(NULL)}


    gp<- ggplotly(KPIin(arquivo())$g2) %>%
    config(collaborate=FALSE,
     cloud=FALSE,
     displaylogo=FALSE,
     modeBarButtonsToRemove=c(
       "select2d",
       "sendDataToCloud",
       "pan2d",
       "resetScale2d",
       "hoverClosestCartesian",
       "hoverCompareCartesian",
       "lasso2d",
       "zoomIn2d",
       "zoomOut2d")
     )

  ##gp <- layout(gp, margin=list(t = 100),autosize = F)

    gp
  })

  output$KPIdados <-renderPlotly({
    if(is.null(arquivo())){return(NULL)}

    gp<- ggplotly(KPIdim(arquivo())) %>%
    config(collaborate=FALSE,
     cloud=FALSE,
     displaylogo=FALSE,
     modeBarButtonsToRemove=c(
       "select2d",
       "sendDataToCloud",
       "pan2d",
       "resetScale2d",
       "hoverClosestCartesian", 
       "hoverCompareCartesian",
       "lasso2d",
       "zoomIn2d",
       "zoomOut2d")
     )
  ##gp <- layout(gp, margin=list(t = 100),autosize = F)
    gp
  })

  output$reativoss <- renderPlotly({

    p <-ggplot(df, aes(x = date, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    scale_color_manual(values=c("#00ba88","#113d31"))+
    theme_minimal()+
    labs(title="Transações por ano")+
    tema

    
    gp <- ggplotly(p,width=880,height=400) %>%
    config(collaborate=FALSE,
     cloud=FALSE,
     displaylogo=FALSE,
     modeBarButtonsToRemove=c(
       "select2d",
       "sendDataToCloud",
       "pan2d",
       "resetScale2d",
       "hoverClosestCartesian",
       "hoverCompareCartesian",
       "lasso2d",
       "zoomIn2d",
       "zoomOut2d")
     )
    
    gp <- layout(gp, margin=list(t = 100),autosize = F)
    
    gp
  })

##-----------------------------------------------------------------------##

########################  TABELA DE RECOMENDAÇÃO ##########################

##-----------------------------------------------------------------------##


  output$table2 <- DT::renderDataTable({
    datatable(
      TabRegras(),
      filter="top",
      extensions = c('Buttons'), 
      width="1500px",
      options = list( dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength=dim(TabRegras())[1],              
        columnDefs = list(list(width = '500px', targets = c(2,3))),
                      ##deferRender = TRUE,
        scrollY = 400
                      ##lengthMenu = c(500, 1000, 1500, 3000,5000,10000,500000)))
        ))})




  output$qtd <- renderText({

    if(is.null(TabRegras())){
      return(NULL)
    }
    format(length(unique(arquivo()$ORCAMENTO)))
  })

  output$qtdRegras <- renderText({
    if(is.null(TabRegras())){
      return(NULL)
    }
    else{
      format(length((TabRegras()$Produto)))
    }
  })  

  objeto <- reactive({ input$dest_order })

  arquivoCompras <- reactive({ input$dest_order })
  output$order <- renderText({arquivoCompras()})
  ##observe({

    ##    toggle(id = "rpan2", condition = input$checkbox,anim = TRUE, animType = "slide", time = 0.5,
 ## selector = NULL)
  ##    })
  produtoSelect <- reactive({input$cartAdd})
  output$textoselecao <- renderText({as.character(produtoSelect())
})
  
  produtoCruzado <- reactive({
    Mydf=data.frame(df.nomeProduto=produtoSelect())
    Mydf <- left_join(Mydf,Produtos_categoria,by=c(`df.nomeProduto`="PRODUTO"))
  })

  observeEvent(input$`btn-add-1`,{shinyjs::html("prod2",sprintf("<div id=\"%s\" class=\"product\"> 
    <div class=\"product-image\">
    <img src=%s>
    </div>
    <div class=\"product-details\">
    <div class=\"product-title\">%s</div>
    <p class=\"product-description\">
    </p>
    </div>
    <div class=\"product-price\">
    12.99
    </div>
    <div class=\"product-quantity\">

    <input type=\"number\" value=\"2\" min=\"1\">
    </div>
    <div class=\"product-removal\">
    <button id=\"btn-remove\" style=\"width:100px;\" class=\"remove-product\">Remover</button>
    </div>
    <div class=\"product-line-price\">
    25.98
    </div>
    </div>",input$cartAdd,paste0("/imagens",images[1]), input$cartAdd),add=TRUE)})


  observeEvent(input$`rm-prod-1` ,{
    shinyjs::toggle(input$cartAdd)
  }
  )



  produtos<-reactive({
   TabRegras() %>%  group_by(`COD_PRODUTO_RECOMENDADO`) %>% 
   dplyr::summarise(frequencia=n()) %>% 
   arrange(desc(frequencia)) 

   dataplot %>% mutate(linha=seq(1,nrow(dataplot),by=1))%>% filter(linha <= 10) %>% 
   mutate(freq=(frequencia/sum(frequencia))*100,
     freq_acu=(cumsum(frequencia)/sum(frequencia))*100)


 })


}




