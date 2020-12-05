library(shiny)
require(leaflet)
require(shinydashboard)
require(tidyverse)
require(plotly)



#######################carregamento dos modelos########################### 

modelo_aluguel_asa_norte <- readRDS('modelos aluguel/Asa Norte.rds')
modelo_aluguel_asa_sul <- readRDS('modelos aluguel/Asa Sul.rds')
modelo_aluguel_lago_sul <- readRDS('modelos aluguel/Lago Sul.rds')

modelo_venda_apt <- readRDS('modelos venda/modapt.gz')
modelo_venda_casa <- readRDS('modelos venda/modcasa.gz')

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





############# funcao de apoio ###################


def_bairro <- function(bairro,imovel){
    
}






#####################carregamento do banco#####################





bairros <- c("Noroeste","Asa Norte","Asa Sul","Lago Norte","Lago Sul")
opcoes <- c('Aluguel',"Venda")

analise <- readRDS('banco_shiny.RDS') %>% mutate(ID=1:n())


############# Header do dashboard #####################
titulo <- dashboardHeader(title = 'Build Parcial 2')




##################Menu lateral######################

menu_lateral <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        menuItem('Exploratória', tabName = 'exp'),
        menuItem('Estimação', tabName = 'est'),
        menuItem('Imóveis da União',tabName = 'uniao')
    ),
    
    
    
        conditionalPanel(
            "input.tabs == 'exp'",
            radioButtons('procura', 'O que você procura?', choices = opcoes),
            selectInput('bairro', 'Escolha o bairro', choices = bairros),
            
            sliderInput(
                'quartos',
                'Número de quartos:',
                min = 1,
                max = max(analise$quartos, na.rm = T),
                value = 4
            ),
            # sliderInput(
            #     'meter',
            #     'Metragem:',
            #     min = min(analise$area_util_m2, na.rm = T),
            #     max = max(analise$area_util_m2, na.rm = T),
            #     value = 50
            # ),
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
            ),
            radioButtons('imovel', 'Tipo de imóvel', choices = c('Casa','Apartamento')),
            numericInput("m2", "Metragem", value = 50, min =
                             0)
            
        ),
    
    # asa norte
        conditionalPanel(
            condition = "input.tabs == 'est'",
            sliderInput(
                "vaga",
                "Número de Vagas",
                min = 0,
                max = 10,
                step = 1,
                value = 0
            )
        ),
    
    # asa sul
        conditionalPanel(
            condition = "input.local == 'sul' && input.tabs == 'est'",
            numericInput("quarto", "Número de Quartos", value =
                             2, min = 0),
            numericInput(
                "condo",
                "Valor do Condomínio R$",
                value = 100,
                min = 0
            )
        ),
    
    # lago sul
        conditionalPanel(
            condition = "input.tabs == 'est'",
            numericInput("ban", "Número de Banheiros", value =
                             2, min = 0)
            
        ),
    conditionalPanel(
        condition = "input.tabs == 'est' ",
    numericInput(
        "alug",
        "Aluguel a ser comparado R$",
        value = 1000,
        min = 0)
    ),
    
    conditionalPanel(
        condition = "input.tabs == 'uniao' ",
        checkboxGroupInput(
            'status',
            'Status:',
            choices = c('Em processo', 'Aprovado', 'Edital')
        )
    )
    
    )




################# corpo do dashboard ########################
corpo <- dashboardBody(
    tabItems(
        
        #exploratoria
        tabItem('exp',
                fluidRow(
                    box(plotlyOutput('precos')),
                    box(plotlyOutput('metragem')),
                    leafletOutput('mapa')
                )),
        
        
        #estimacao
        tabItem('est',
                fluidRow(
                    infoBoxOutput('estima',width=6),
                    infoBoxOutput('compara',width=6),
                    infoBoxOutput('metroq',width=6)
                )),
        tabItem('uniao',
                dataTableOutput('tabela'))
        ))







####################### back end ######################################
server <- function(input, output) {
    
    #exploratoria
    
    banco_filtro <- reactive({
        filter(analise,bairro==input$bairro & str_detect(tipo_anuncio,input$procura) & quartos<=input$quartos & area_util_m2<=input$meter)
    })
    
    output$precos <- renderPlotly({
        plot_ly(data = banco_filtro(),x=~get(input$procura),type = 'histogram',bingroup=1) %>%
            layout(title= 'Distribuição dos preços',
                   xaxis=list(title=paste('Valor de',input$procura,"em Reais" )),
                   bargap=0.1
                   )
    })
    
    output$metragem <- renderPlotly({
        plot_ly(data = banco_filtro(),x=~area_util_m2,type = 'histogram',bingroup=1) %>%
            layout(title= 'Distribuição de metragem',
                   xaxis=list(title='Área Útil em metros quadrados'),
                   bargap=0.1)
            
    })
    
    output$mapa <-  renderLeaflet({
        suppressWarnings({leaflet(banco_filtro()) %>%
                addTiles() %>%
                addMarkers(lng = ~longitude,
                           lat = ~latitude,
                           popup = ~paste('R$',get(input$procura),'com',area_util_m2,'metros quadrados','ID',ID))})
    })
    
    
    
    #estimacao
    
    data_est <- reactive({
        
        banco_predicao <- data.frame(`Area Util`=input$m2,
                                     Condominio=input$condo,
                                     Vagas=as.numeric(input$vaga>=1),
                                     Quartos=input$quarto,
                                     Banheiros=input$ban,
                                     area_util_m2=input$m2,
                                     vagas=input$vaga,
                                     quartos=input$quarto,
                                     banheiros=input$ban,
                                     bairro=
                                     check.names = F
                                     )
        
        
        
        valor <- switch(input$local, 
                        norte = round((0.3*predict.lm(modelo_aluguel_asa_norte,newdata=banco_predicao)+1)^(1/0.3),2),
                        
                        sul = round(exp(predict.lm(modelo_aluguel_asa_sul,newdata=banco_predicao)),2),
                        
                        lago = round(predict.lm(modelo_aluguel_lago_sul,newdata=banco_predicao),2)
        )
        
        
        
        return(valor)
    })
    
    
    data_diff <- reactive({abs(data_est()-input$alug)})

    
    
    
    
    
    output$estima <- renderInfoBox({
        infoBox(tags$p("Preço Estimado",style="font-size: 120%;",),
                tags$p(
                paste('R$',suppressWarnings({format(data_est(),decimal.mark = ',',big.mark = '.')})),
                style="font-size: 150%;"
                )
    
    
                
                )})
    
    
    output$compara <- renderInfoBox({
        infoBox(tags$p("Comparação",style="font-size: 120%;"),
                tags$p(
                paste('R$',suppressWarnings({format(data_diff(),decimal.mark = ',',big.mark = '.')})),
                style="font-size: 150%;"
                )
    
    
                
                )})

    
    
        output$metroq <- renderInfoBox({
        infoBox(tags$p("valor do aluguel/m²",style="font-size: 120%;"),
                tags$p(
                paste('R$',suppressWarnings({format(round(data_est()/input$m2,2),decimal.mark = ',',big.mark = '.')}),'/M²'),
                style="font-size: 150%;"
                )
    
    
                
                )})
    
    ###uniao#########
        
        output$tabela <- renderDataTable({data.frame(A=1,B=1,C=1)})
        
        
        
        

}

######### rodar o shiny ###############
ui <- dashboardPage(header=titulo,sidebar=menu_lateral,body=corpo)
shinyApp(ui = ui, server = server)

