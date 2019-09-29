## Bibliotecas
library(plotly)

### Diretorio 


### Lendo dataset

#data2 <- read_delim("2017 - 3.csv",delim=";")

#data4 <- read_delim("Produtos-categoria.txt", delim = ";")

# data2= na.omit(data2)

# data2<-data2%>%filter(!(str_detect(COD_PRODUTO,"S")))%>%
#   filter(!(str_detect(COD_PRODUTO,"G")))%>%
#   filter(!(str_detect(COD_PRODUTO,"00000")))%>%
#   filter(!(str_detect(CLASSIFICACAO,"CHIP")))%>%
#   filter(!(str_detect(CLASSIFICACAO,"ALMOXARIFADO")))%>%
#   filter(!(str_detect(CLASSIFICACAO,"SERVICOS")))

# data4<-data4%>%filter(!(str_detect(COD_PRODUTO,"S")))%>%
#   filter(!(str_detect(COD_PRODUTO,"G")))%>%
#   filter(!(str_detect(COD_PRODUTO,"00000")))%>%
#   filter(!(str_detect(CLASSIFICACAO,"CHIP")))%>%
#   filter(!(str_detect(CLASSIFICACAO,"ALMOXARIFADO")))%>%
#   filter(!(str_detect(CLASSIFICACAO,"SERVICOS")))

data4 <- read_delim("./Produtos-categoria.txt", delim = ";")

KPI1<-function(SysData){
  


#### Pareto por produto ###

  dataplot <- SysData %>% group_by(`Produto recomendado`) %>% summarise(frequencia=n()) %>% 
  arrange(desc(frequencia)) %>% top_n(10,wt=frequencia) %>%
  mutate(freq=(frequencia/sum(frequencia))*100,
   freq_acu=(cumsum(frequencia)/sum(frequencia))*100)

dataplot <- left_join(dataplot,data4,by=c('Produto recomendado'=
                       'COD_PRODUTO'))


  p <- plot_ly(x=reorder(dataplot$PRODUTO,desc(dataplot$freq),max),y=dataplot$freq,
    type = 'bar',
    marker = list(color = 'rgb(0, 77, 26)')) %>%
  add_trace(type="scatter",
    x=reorder(dataplot$PRODUTO,desc(dataplot$freq),max),
    y=dataplot$freq_acu,
    mode = 'lines',showlegend=F,
    line = list(color = 'rgb(0, 186, 136)')) %>%
  add_trace(type="scatter",
    x=reorder(dataplot$PRODUTO,desc(dataplot$freq),max),
    y=dataplot$freq_acu,marker=list(color='rgb(0, 186, 136)'),
    mode = 'point',showlegend=F) %>%
  layout(title = "Diagrama de Pareto",
   xaxis = list(title = "Produto",showticklabels = FALSE,
    showgrid = FALSE,autorange = TRUE),
   yaxis = list(title = "Porcentagem")) 

  return(p)

}

### Pareto para categorias ####

# View(dataplot)
# View(x)
# h <- plot_ly(x=reorder(x=as.factor(dataplot$CLASSIFICACAO),X=dataplot$frequencia,FUN=max),y=dataplot$freq,
#              textposition = 'auto',
#              type = 'bar',
#              marker = list(color = 'rgb(0, 77, 26)')) %>%
#   add_trace(type="scatter",
#             x=reorder(x=as.factor(dataplot$CLASSIFICACAO),X=dataplot$frequencia,FUN=max),
#             y=dataplot$freq_acu,
#             mode = 'lines',showlegend=F,
#             line = list(color = 'rgb(0, 186, 136)')) %>%
#   add_trace(type="scatter",
#             x=reorder(x=as.factor(dataplot$CLASSIFICACAO),X=dataplot$frequencia,FUN=max),
#             y=dataplot$freq_acu,
#             mode = 'point',showlegend=F,
#             marker=list(color='rgb(0, 186, 136)')) %>%
#   layout(title = "Diagrama de Pareto",
#          xaxis = list(title = "Categorias",showticklabels = FALSE,
#                       showgrid = FALSE,autorange = TRUE),
#          yaxis = list(title = "Procentagem")) 

# h

# View(dataplot2)

# ###### Gráficos de entradas - Perfis ####

# data2=na.omit(data2)

# ### Por categoria 

# plotdata3 <- data2 %>% group_by(CLASSIFICACAO) %>% 
#               summarise(frequencia=n()) %>%
#               arrange(desc(frequencia)) %>%
#   top_n(10,wt=frequencia) %>%
#     mutate(freq_rel=((frequencia)/sum(frequencia))*100,
#            freq_ac=(cumsum(frequencia)/sum(frequencia))*100) 
    
# View(plotdata3)

# c <- plot_ly(x=reorder(plotdata3$CLASSIFICACAO,desc(plotdata3$frequencia),max),y=plotdata3$freq_rel,
#              textposition = 'auto',
#              type = 'bar',
#              marker = list(color = 'rgb(0, 77, 26)')) %>%
#   add_trace(type="scatter",
#             x=reorder(plotdata3$CLASSIFICACAO,desc(plotdata3$frequencia),max),
#             y=plotdata3$freq_ac,
#             mode = 'lines',showlegend=F,
#             line = list(color = 'rgb(0, 186, 136)')) %>%
#   add_trace(type="scatter",
#             x=reorder(plotdata3$CLASSIFICACAO,desc(plotdata3$frequencia),max),
#             y=plotdata3$freq_ac,
#             mode = 'point',showlegend=F,
#             marker=list(color='rgb(0, 186, 136)')) %>%
#   layout(title = "Diagrama de Pareto",
#          xaxis = list(title = "Categorias",showticklabels = FALSE,
#                       showgrid = FALSE,autorange = TRUE),
#          yaxis = list(title = "Porcentagem")) 

# c

# #### Por Loja ####

# dataplot4 <- data2 %>% group_by(FILIAL) %>% summarise(frequencia=n()) %>%
#              arrange(desc(frequencia)) %>% top_n(10,wt=frequencia) %>%
#              mutate(frequencia_rel=(frequencia/sum(frequencia))*100, 
#               frequencia_ac=(cumsum(frequencia)/sum(frequencia))*100) 
          
# View(dataplot4)

# f <- plot_ly(x=reorder(dataplot4$FILIAL,desc(dataplot4$frequencia),max),y=dataplot4$frequencia_rel,
#              textposition = 'auto',
#              type = 'bar',
#              marker = list(color = 'rgb(0, 77, 26)')) %>%
#   add_trace(type="scatter",
#             x=reorder(dataplot4$FILIAL,desc(dataplot4$frequencia),max),
#             y=dataplot4$frequencia_ac,
#             mode = 'lines',showlegend=F,
#             line = list(color = 'rgb(0, 186, 136)')) %>%
#   add_trace(type="scatter",
#             x=reorder(dataplot4$FILIAL,desc(dataplot4$frequencia),max),
#             y=dataplot4$frequencia_ac,
#             mode = 'point',showlegend=F,
#             marker=list(color='rgb(0, 186, 136)')) %>%
#   layout(title = "Diagrama de Pareto",
#          xaxis = list(title = "Lojas",showticklabels = FALSE,
#                       showgrid = FALSE,autorange = TRUE),
#          yaxis = list(title = "Porcentagem")) 

# f


# ## Por sexo, idade, loja ##

# S='F'
# ID=25
# L='05'

# lucao <- function(S,ID,L){
  
#   if(is.na(S)) {do='T1'}
#     if(is.na(ID)) {do='T2'}
#       if(is.na(L)){ do='T3'}
#         if(is.na(S) & is.na(L) & is.na(ID)){ do='T4'}
#   if((is.na(S)==FALSE) & (is.na(L)==FALSE) & (is.na(ID)==FALSE)){ do='T5' }
  
#   switch(do,
#          'T1'= {
#          x <- data2  %>%
#              filter(IDADE==ID & FILIAL== L) %>%
#            group_by(COD_PRODUTO) %>% summarise(frequencia=n()) %>%
#            arrange(desc(frequencia))}
#          , 
#          'T2'= {
#     x <- data2 %>% filter(SEXO==S & FILIAL==L) %>%
#       group_by(COD_PRODUTO) %>% summarise(frequencia=n()) %>%
#       arrange(desc(frequencia))}, 
#   'T3'= {
#     x <- data2 %>% filter(SEXO==S & IDADE==ID) %>%
#       group_by(COD_PRODUTO) %>% summarise(frequencia=n()) %>%
#       arrange(desc(frequencia))},
#   'T4'= {
#     x <- data2 %>% 
#       group_by(COD_PRODUTO) %>% summarise(frequencia=n()) %>%
#       arrange(desc(frequencia))}, 
#   'T5'= {
#     x <- data2 %>% filter(SEXO==S & IDADE==ID & FILIAL==L) %>% 
#       group_by(COD_PRODUTO) %>% summarise(frequencia=n()) %>%
#       arrange(desc(frequencia))})
#   }

# x<-lucao(S,ID,L)

# x<- left_join(x,data4,by="COD_PRODUTO")

# x <- x %>% na.omit() %>% top_n(10,wt=frequencia) %>% 
#   mutate(frequencia_rel=(frequencia/sum(frequencia))*100,
#          frequencia_ac=(cumsum(frequencia)/sum(frequencia))*100)

# View(x)

# id <- plot_ly(x=reorder(x$PRODUTO,desc(x$frequencia),max),y=x$frequencia_rel,
#               textposition = 'auto',
#               type = 'bar',
#               marker = list(color = 'rgb(0, 77, 26)')) %>%
#   add_trace(type="scatter",
#             x=reorder(x$PRODUTO,desc(x$frequencia),max),
#             y=x$frequencia_ac,
#             mode = 'lines',showlegend=F,
#             line = list(color = 'rgb(0, 186, 136)')) %>%
#   add_trace(type="scatter",
#             x=reorder(x$PRODUTO,desc(x$frequencia),max),
#             y=x$frequencia_ac,
#             mode = 'point',showlegend=F,
#             marker=list(color='rgb(0, 186, 136)')) %>%
#   layout(title = "Diagrama de Pareto",
#          xaxis = list(title = "Produto",showticklabels = FALSE,
#                       showgrid = FALSE,autorange = TRUE),
#          yaxis = list(title = "Porcentagem")) 

# id




# }