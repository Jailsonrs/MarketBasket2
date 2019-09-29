
data4 <- read_delim("./Produtos-categoria.txt", delim = ";", escape_double = FALSE, trim_ws = TRUE)

data4 <- data4 %>% 
       filter(!(str_detect(COD_PRODUTO,"S"))) %>%
           filter(!(str_detect(COD_PRODUTO,"G"))) %>%
         filter(!(str_detect(COD_PRODUTO,"00000")))  %>%
         filter(!(str_detect(CLASSIFICACAO,"CHIP"))) %>%
         filter(!(str_detect(CLASSIFICACAO,"ALMOXARIFADO"))) %>%
         filter(!(str_detect(CLASSIFICACAO,"SERVICOS")))

GetData<-read_delim('2017 - 3.csv',delim=";")
##
##data <-read_delim("./data/REC_RECOMENDA_POR_PRODUTO.txt",
##";", escape_double = FALSE, trim_ws = TRUE)


KPI <- function(SysData){

 ## Pareto por produto ##
# %>% 


dataplot <- SysData %>%  group_by(`COD_PRODUTO_RECOMENDADO`) %>% 
  dplyr::summarise(frequencia=n()) %>% 
   arrange(desc(frequencia)) 

dataplot <- dataplot %>% mutate(linha=seq(1,nrow(dataplot),by=1))%>% filter(linha <= 10) %>% 
   mutate(freq=(frequencia/sum(nrow(SysData)))*100,
         freq_acu=(cumsum(frequencia)/sum(nrow(SysData)))*100)

 
#### -------------------------

dataplot <- left_join(dataplot,data4,by=c(`COD_PRODUTO_RECOMENDADO`=
                       "COD_PRODUTO"))


g1 <- plot_ly(x=reorder(dataplot$PRODUTO,dataplot$linha,function(x) max(x)),y=dataplot$freq,
                              type = 'bar',
             marker = list(color = 'rgb(0, 77, 26)')) %>%
  add_trace(type="scatter",
            x=reorder(dataplot$PRODUTO,dataplot$linha,function(x) max(x)),
            y=dataplot$freq_acu,
            mode = 'lines',showlegend=F,
            line = list(color = 'rgb(0, 186, 136)')) %>%
  add_trace(type="scatter",
            x=reorder(dataplot$PRODUTO,dataplot$linha,function(x) max(x)),
            y=dataplot$freq_acu,marker=list(color='rgb(0, 186, 136)'),
            mode = 'point',showlegend=F) %>%
  layout(title = "",
         xaxis = list(title = "Produto",showticklabels = FALSE,
                      showgrid = FALSE,autorange = TRUE),
         yaxis = list(title = "Porcentagem"), hoverlabel = list(font=list(size=8))) 

#### Pareto por categoria 

data1 <- left_join(SysData,data4,by=c(`COD_PRODUTO_RECOMENDADO`=
                                        "COD_PRODUTO"))

dataplot2 <- data1 %>%  group_by(CLASSIFICACAO) %>% 
  dplyr::summarise(frequencia=n()) %>% 
  arrange(desc(frequencia)) %>% na.omit() 

dataplot2 <- dataplot2  %>% mutate(linha=seq(1,nrow(dataplot2),by=1))%>% filter(linha <= 10) %>% 
  mutate(freq=(frequencia/sum(nrow(SysData)))*100,
         freq_acu=(cumsum(frequencia)/sum(nrow(SysData)))*100)

#-----------------------------------------------------------------------------------------------------------

g2 <- plot_ly(x=reorder(dataplot2$CLASSIFICACAO,dataplot2$linha,function(x) max(x)),
             y=dataplot2$freq,
             type = 'bar',
             marker = list(color = 'rgb(0, 77, 26)')) %>%
  add_trace(type="scatter",
            x=reorder(dataplot2$CLASSIFICACAO,dataplot2$linha,function(x) max(x)),
            y=dataplot2$freq_acu,
            mode = 'lines',showlegend=F,
            line = list(color = 'rgb(0, 186, 136)')) %>%
  add_trace(type="scatter",
            x=reorder(dataplot2$CLASSIFICACAO,dataplot2$linha,function(x) max(x)),
            y=dataplot2$freq_acu,
            mode = 'point',showlegend=F,
            marker=list(color='rgb(0, 186, 136)')) %>%
  layout(title = "",
         xaxis = list(title = "Categorias",showticklabels = FALSE,
                      showgrid = FALSE,autorange = TRUE),
         yaxis = list(title = "Procentagem"),
         hoverlabel = list(font=list(size=8))) 

graph <- list(kp1=g1,kp2=g2)

  return(graph)

}

######## KPIs para o perfil de entrada 

KPIin <- function(GetData) {
  
  ###### Gráficos de entradas - Perfis ####
  
  ### Por categoria 
  
  plotdata3 <- GetData %>% group_by(CLASSIFICACAO) %>% 
    dplyr::summarise(frequencia=n()) %>%
    arrange(desc(frequencia)) %>%
    top_n(10,wt=frequencia) %>%
    mutate(freq_rel=((frequencia)/length(unique(GetData$ORCAMENTO)))*100,
           freq_ac=(cumsum(frequencia)/length(unique(GetData$ORCAMENTO)))*100) 
  
  
  KP1 <- plot_ly(x=reorder(plotdata3$CLASSIFICACAO,desc(plotdata3$frequencia),max),y=plotdata3$freq_rel,
               textposition = 'auto',
               type = 'bar',
               marker = list(color = 'rgb(0, 77, 26)')) %>%
    add_trace(type="scatter",
              x=reorder(plotdata3$CLASSIFICACAO,desc(plotdata3$frequencia),max),
              y=plotdata3$freq_ac,
              mode = 'lines',showlegend=F,
              line = list(color = 'rgb(0, 186, 136)')) %>%
    add_trace(type="scatter",
              x=reorder(plotdata3$CLASSIFICACAO,desc(plotdata3$frequencia),max),
              y=plotdata3$freq_ac,
              mode = 'point',showlegend=F,
              marker=list(color='rgb(0, 186, 136)')) %>%
    layout(title = "",
           xaxis = list(title = "Categorias",showticklabels = FALSE,
                        showgrid = FALSE,autorange = TRUE),
           yaxis = list(title = "Porcentagem"),
           hoverlabel = list(font=list(size=8))) 
  

  
  #### Por Loja ####
  
  dataplot4 <- GetData %>% group_by(FILIAL) %>% dplyr::summarise(frequencia=n()) %>%
    arrange(desc(frequencia)) %>% top_n(10,wt=frequencia) %>%
    mutate(frequencia_rel=(frequencia/length(unique(GetData$ORCAMENTO)))*100, 
           frequencia_ac=(cumsum(frequencia)/length(unique(GetData$ORCAMENTO)))*100) 
  
  
  KP2 <- plot_ly(y=reorder(dataplot4$FILIAL,dataplot4$frequencia,max),x=dataplot4$frequencia_rel,
               textposition = 'auto',
               type = 'bar',
               marker = list(color = 'rgb(0, 77, 26)'),orientation='h') %>%
    # add_trace(type="scatter",
    #           x=reorder(dataplot4$FILIAL,desc(dataplot4$frequencia),max),
    #           y=dataplot4$frequencia_ac,
    #           mode = 'lines',showlegend=F,
    #           line = list(color = 'rgb(0, 186, 136)')) %>%
    # add_trace(type="scatter",
    #           x=reorder(dataplot4$FILIAL,desc(dataplot4$frequencia),max),
    #           y=dataplot4$frequencia_ac,
    #           mode = 'point',showlegend=F,
    #           marker=list(color='rgb(0, 186, 136)')) %>%
    layout(title = "",
           yaxis = list(title = "Loja",showticklabels = T,
                        showgrid = FALSE,autorange = TRUE),
           xaxis = list(title = "Porcentagem",showticklabels = T,
                        showgrid = F),
           hoverlabel = list(font=list(size=8))) 
  
  Saidas=list(g1=KP1,g2=KP2)
  
  return(Saidas)
}


KPIdim <- function(Getdata){
  
  S='F'
  ID=50
  L='12'
  
  lucao <- function(S,ID,L){
    S=as.character(S)
    ID=as.double(ID)
    L=as.character(L)
    
    if(is.na(S)) {do='T1'}
    if(is.na(ID)) {do='T2'}
    if(is.na(L)){ do='T3'}
    if(is.na(S) & is.na(ID)){ do='T4'}
    if(is.na(S) & is.na(L)){ do='T5'}
    if(is.na(ID) & is.na(L)){ do='T6'}
    if(is.na(S) & is.na(L) & is.na(ID)){ do='T7'}
    if((is.na(S)==FALSE) & (is.na(L)==FALSE) & (is.na(ID)==FALSE)){ do='T8' }
    
    switch(do,
           'T1'= { ### consultar produtos por loja e idade
             x <-  Getdata  %>%
               filter(IDADE==ID & FILIAL== L) %>% group_by(COD_PRODUTO) %>% 
               dplyr::summarise(frequencia=n()) %>%
               arrange(desc(frequencia))}
           ,  
           
           'T2'= { ### consultar produtos por sexo e loja
             x <- Getdata  %>%
               filter(SEXO==S & FILIAL== L) %>%
               group_by(COD_PRODUTO) %>% dplyr::summarise(frequencia=n()) %>%
               arrange(desc(frequencia))}
           ,
           
           'T3'= { ### consultar produtos por sexo e idade
             x <- Getdata  %>%
               filter(SEXO==S & IDADE== ID) %>%
               group_by(COD_PRODUTO) %>% dplyr::summarise(frequencia=n()) %>%
               arrange(desc(frequencia))}
           ,
           
           'T4'= { ### consultar produtos apenas pela loja
             x <- Getdata  %>%
               filter(FILIAL== L) %>%
               group_by(COD_PRODUTO) %>% dplyr::summarise(frequencia=n()) %>%
               arrange(desc(frequencia))}
           ,
           
           'T5'= { ## consultar produtos apenas pela idade
             x <- Getdata %>% filter(IDADE == ID) %>%
               group_by(COD_PRODUTO) %>% dplyr::summarise(frequencia=n()) %>%
               arrange(desc(frequencia))}, 
           
           'T6'= { ## consultar produtos apenas pelo sexo
             x <- Getdata %>% filter(SEXO==S) %>%
               group_by(COD_PRODUTO) %>% dplyr::summarise(frequencia=n()) %>%
               arrange(desc(frequencia))},
           
           'T7'= { ### consulta sem restrição de sexo, idade ou loja
             x <- Getdata %>% 
               group_by(COD_PRODUTO) %>% dplyr::summarise(frequencia=n()) %>%
               arrange(desc(frequencia))}, 
           
           'T8'= { ### consultar produtos por sexo, idade e loja
             x <- Getdata %>% filter(SEXO==S & IDADE==ID & FILIAL==L) %>% 
               group_by(COD_PRODUTO) %>% dplyr::summarise(frequencia=n()) %>%
               arrange(desc(frequencia))})
  }
  
  x<-lucao(S,ID,L)
  x<- left_join(x,data4,by="COD_PRODUTO")
  
  x <- x %>% mutate(linha=seq(1,(dim(x)[1]),by=1)) %>% na.omit() %>%
    filter(linha <= 10) %>%
    mutate(frequencia_rel=(frequencia/nrow(x))*100,
           frequencia_ac=(cumsum(frequencia)/nrow(x))*100)
  
  id <- plot_ly(x=reorder(x$PRODUTO,x$linha,function(x) max(x)),
                y=x$frequencia_rel,
                textposition = 'auto',
                type = 'bar',
                marker = list(color = 'rgb(0, 77, 26)')) %>%
    add_trace(type="scatter",
              x=reorder(x$PRODUTO,x$linha,function(x) max(x)),
              y=x$frequencia_ac,
              mode = 'lines',showlegend=F,
              line = list(color = 'rgb(0, 186, 136)')) %>%
    add_trace(type="scatter",
              x=reorder(x$PRODUTO,x$linha,function(x) max(x)),
              y=x$frequencia_ac,
              mode = 'point',showlegend=F,
              marker=list(color='rgb(0, 186, 136)')) %>%
    layout(title = "",
           xaxis = list(title = "Produto",showticklabels = FALSE,
                        showgrid = FALSE,autorange = TRUE),
           yaxis = list(title = "Porcentagem"),
           hoverlabel = list(font=list(size=8))) 
  
  return(id)
  
}














