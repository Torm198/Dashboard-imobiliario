library(shiny)
library(leaflet)
library(shinydashboard)
library(tidyverse)
library(plotly)


bairros <- c("Noroeste","Asa Norte","Asa Sul","Lago Norte","Lago Sul")
opcoes <- c('Aluguel',"Venda")

analise <- readRDS('banco_shiny.RDS') %>% mutate(ID=1:n())



titulo <- dashboardHeader(title = 'Imóveis do wimoveis (prototipo)')


menu_lateral <-dashboardSidebar(sidebarMenu(
    selectInput('bairro', 'Escolha o bairro', choices = bairros),
    radioButtons('procura', 'O que você procura?', choices = opcoes),
    sliderInput('quartos','Número de quartos:',min = 1,max=max(analise$quartos,na.rm = T),value=4),
    #sliderInput('meter','Metragem:',min = min(analise$area_util_m2,na.rm = T),max=max(analise$area_util_m2,na.rm = T),value=50)
    numericInput('meter','Metragem:',min = min(analise$area_util_m2,na.rm = T),max=max(analise$area_util_m2,na.rm = T),value=50),
    checkboxGroupInput('status','status(não implementado)',choices =c('Em processo','Aprovado','Edital') )
    
))


corpo <- dashboardBody(
    fluidRow(
    box(plotlyOutput('precos')),
    box(plotlyOutput('metragem'))),
    leafletOutput('mapa')
)




ui <- dashboardPage(header=titulo,sidebar=menu_lateral,body=corpo)



server <- function(input, output) {
    banco_filtro <- reactive({
        filter(analise,bairro==input$bairro & str_detect(tipo_anuncio,input$procura) & quartos<=input$quartos & area_util_m2<=input$meter)
    })
    
    output$precos <- renderPlotly({
        plot_ly(data = banco_filtro(),y=~get(input$procura),type = 'bar') %>%
            layout(title= 'Distribuição dos preços',
                   xaxis=list(title=paste('Valor de',input$procura ))
                   )
    })
    
    output$metragem <- renderPlotly({
        plot_ly(data = banco_filtro(),y=~area_util_m2,type = 'bar') %>%
            layout(title= 'Distribuição de metragem',
                   xaxis=list(title='Área Útil'))
            
    })
    
    output$mapa <-  renderLeaflet({
        suppressWarnings({leaflet(banco_filtro()) %>%
            addTiles() %>%
            addMarkers(lng = ~longitude,
                       lat = ~latitude,
                       popup = ~paste('R$',get(input$procura),'com',area_util_m2,'metros quadrados','ID',ID))})
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
