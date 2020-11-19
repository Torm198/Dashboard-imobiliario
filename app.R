library(shiny)
library(leaflet)
library(shinydashboard)
library(tidyverse)
library(plotly)





modelo_aluguel_asa_norte <- readRDS('modelos aluguel/Asa Norte.rds')
modelo_aluguel_asa_sul <- readRDS('modelos aluguel/Asa Sul.rds')
modelo_aluguel_lago_sul <- readRDS('modelos aluguel/Lago Sul.rds')

modelo_aluguel_asa_norte %>% summary()
modelo_aluguel_asa_sul %>% summary()
modelo_aluguel_lago_sul %>% summary()

##NÃO RODAR, EM CONSTRUÇÃo##
# modelo_aluguel <- list()
# 
# 
# modelo_aluguel[['Asa Norte']] <- function(area,condominio,vaga){
#     
#     est <- predict.lm(modelo_aluguel_asa_norte,
#                       
#                       newdata = data.frame( area = m2,
#                                             Condominio = condominio,
#                                             vagas = (vaga >= 1)*1
#                       ))
#     
#     transformada <- round((0.3*est+1)^(1/0.3),2)
#     
#     saida <- format(transformada,nsmall = 2,decimal.mark = ',')
#     
#     
#     return(saida)
# } 
# 
# 
# 
# modelo_aluguel[['Asa Sul']] <- function(quarto,condominio,sobra){
#     sobra <- NA # mata a variavel
#     est <- predict.lm(modelo_aluguel_asa_sul,
#                       newdata = data.frame(Quartos = quarto,
#                                            Condominio = condominio))
#     
#     transformada <- round(exp(est),2)
#     
#     saida <- format(transformada,nsmall = 2,decimal.mark = ',')
#     
#     
#     return(saida)
# }
# 
# 
# 
# modelo_aluguel[['Lago Sul']] <- function(area,quarto,banheiro){
#     est <- predict.lm(modelo_aluguel_lago_sul,
#                       newdata = data.frame(area=input$m2,
#                                            Quartos=input$quarto,
#                                            Banheiros=input$ban))
#     
#     transformada <- round(exp(est),2)
#     
#     saida <- format(transformada,nsmall = 2,decimal.mark = ',')
#     
#     
#     return(saida)
# } 

















bairros <- c("Noroeste","Asa Norte","Asa Sul","Lago Norte","Lago Sul")
opcoes <- c('Aluguel',"Venda")

analise <- readRDS('banco_shiny.RDS') %>% mutate(ID=1:n())



titulo <- dashboardHeader(title = 'Build Parcial 2')


menu_lateral <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        menuItem('Exploratória', tabName = 'exp'),
        menuItem('Estimação', tabName = 'est')
    ),
        conditionalPanel(
            "input.tabs == 'exp'",
            #menuItem('Aluguel',tabName = 'aluguel'),
            #menuItem('Compra e Venda',tabName = 'venda'),
            selectInput('bairro', 'Escolha o bairro', choices = bairros),
            radioButtons('procura', 'O que você procura?', choices = opcoes),
            sliderInput(
                'quartos',
                'Número de quartos:',
                min = 1,
                max = max(analise$quartos, na.rm = T),
                value = 4
            ),
            #sliderInput('meter','Metragem:',min = min(analise$area_util_m2,na.rm = T),max=max(analise$area_util_m2,na.rm = T),value=50)
            numericInput(
                'meter',
                'Metragem:',
                min = min(analise$area_util_m2, na.rm = T),
                max = max(analise$area_util_m2, na.rm = T),
                value = 50
            ),
            numericInput(
                'condominio',
                'Condominio:',
                min = min(analise$condominio, na.rm = T),
                max = 100000,
                value = 1000
            ),
            numericInput(
                'vagas',
                'N° de Vagas:',
                min = 1,
                max = max(analise$vagas, na.rm = T),
                value = 1
            ),
            checkboxGroupInput(
                'status',
                'status(não implementado)',
                choices = c('Em processo', 'Aprovado', 'Edital')
            )
        ),
        
        
        
        
        conditionalPanel(
            condition = "input.tabs == 'est'",
            selectInput(
                "local",
                "Localização",
                choices = list(
                    "Asa Norte" = "norte",
                    "Asa Sul" = "sul",
                    "Lago Sul" = "lago"
                ),
                selected = "norte"
            )
        ),
        conditionalPanel(
            condition = "input.local == 'norte' && input.tabs == 'est'",
            numericInput("m2", "Metragem", value = 50, min =
                             0),
            numericInput(
                "condo",
                "Valor do Condomínio R$",
                value = 100,
                min = 0
            ),
            sliderInput(
                "vaga",
                "Número de Vagas",
                min = 0,
                max = 10,
                step = 1,
                value = 0
            ),
            numericInput(
                "alug",
                "Aluguel a ser comparado R$",
                value = 1000,
                min = 0
            )
        ),
        # Wrap the file input in a conditional panel
        conditionalPanel(
            # The condition should be that the user selects
            condition = "input.local == 'sul' && input.tabs == 'est'",
            numericInput("quarto", "Número de Quartos", value =
                             2, min = 0),
            numericInput(
                "condo",
                "Valor do Condomínio R$",
                value = 100,
                min = 0
            ),
            numericInput(
                "alug",
                "Aluguel a ser comparado R$",
                value = 1000,
                min = 0
            )
        ),
        conditionalPanel(
            # The condition should be that the user selects
            condition = "input.local == 'lago' && input.tabs == 'est'",
            numericInput("m2", "Metragem", value = 50, min =
                             0),
            numericInput("quarto", "Número de Quartos", value =
                             2, min = 0),
            numericInput("ban", "Número de Banheiros", value =
                             2, min = 0),
            numericInput(
                "alug",
                "Aluguel a ser comparado R$",
                value = 1000,
                min = 0
            )
        )
    )



corpo <- dashboardBody(
    tabItems(
        tabItem('exp',
                fluidRow(
                    box(plotlyOutput('precos')),
                    box(plotlyOutput('metragem')),
                    leafletOutput('mapa')
                )),
        tabItem('est',
                infoBoxOutput('estima'))
        ))






server <- function(input, output) {
    banco_filtro <- reactive({
        filter(analise,bairro==input$bairro & str_detect(tipo_anuncio,input$procura) & quartos<=input$quartos & area_util_m2<=input$meter)
    })
    
    output$precos <- renderPlotly({
        plot_ly(data = banco_filtro(),x=~get(input$procura),type = 'histogram',bingroup=1,bargap=0.1) %>%
            layout(title= 'Distribuição dos preços',
                   xaxis=list(title=paste('Valor de',input$procura,"em Reais" )),
                   bargap=0.1
                   )
    })
    
    output$metragem <- renderPlotly({
        plot_ly(data = banco_filtro(),x=~area_util_m2,type = 'histogram',bingroup=1,bargap=0.1) %>%
            layout(title= 'Distribuição de metragem',
                   xaxis=list(title='Área Útil em metros quadrados'),
                   bargap=0.1)
            
    })
    
    
    
    data_est <- reactive({
        valor <- switch(input$local, 
                        norte = round((0.3*predict.lm(modelo_aluguel_asa_norte,newdata=data.frame(`Area Util`=input$m2,Condominio=input$condo,
                                                                              Vagas=as.numeric(input$vaga>=1),check.names = F))+1)^(1/0.3),2),
                        
                        sul = round(exp(predict.lm(modelo_aluguel_asa_sul,newdata=data.frame(Quartos=input$quarto,
                                                                             Condominio=input$condo))),2),
                        
                        lago = round(predict.lm(modelo_aluguel_lago_sul,newdata=data.frame(`Area Util`=input$m2,Quartos=input$quarto,
                                                                         Banheiros=input$ban,check.names = F)),2)
        )
    })
    
   
    
    output$estima <- renderInfoBox({
        infoBox("Preço Estimado",
                paste('R$',suppressWarnings({format(data_est(),decimal.mark = ',',big.mark = '.')}))
    )})
    
    output$mapa <-  renderLeaflet({
        suppressWarnings({leaflet(banco_filtro()) %>%
            addTiles() %>%
            addMarkers(lng = ~longitude,
                       lat = ~latitude,
                       popup = ~paste('R$',get(input$procura),'com',area_util_m2,'metros quadrados','ID',ID))})
    })

}

# Run the application 
ui <- dashboardPage(header=titulo,sidebar=menu_lateral,body=corpo)
shinyApp(ui = ui, server = server)
