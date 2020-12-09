require(shiny)
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





############# funcao de apoio ###################


def_bairro <- function(bairro,imovel){
    if_else(imovel=='casa',
            if_else(bairro=='lago',
                    'Lago Sul',
                    'Plano'
            ),
            if_else(bairro=='norte',
                    'Asa Norte',
                    'Asa Sul'))
}






#####################carregamento do banco#####################




# banco do wi imoveis
bairros <- c("Noroeste","Asa Norte","Asa Sul","Lago Norte","Lago Sul")
opcoes <- c('Aluguel',"Venda")

analise <- readRDS('banco_shiny.RDS') %>% mutate(ID=1:n())


# banco da uniao




############# Header do dashboard #####################
titulo <- dashboardHeader(title = 'Algm decide o titulo')




##################Menu lateral######################

menu_lateral <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        menuItem('Exploratória', tabName = 'exp'),
        menuItem('Estimação', tabName = 'est'),
        menuItem('Imóveis da União',tabName = 'uniao')
    ),
    
    
    # exploratoria
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
    
    
    
    # estimacao
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
        radioButtons('imovel', 'Tipo de imóvel', choices = list('Casa'= 'casa','Apartamento'='apt')),
        numericInput("m2", "Metragem", value = 50, min = 0)
        
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
        condition = "input.tabs == 'est'",
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
    
    # uniao
    conditionalPanel(
        condition = "input.tabs == 'uniao' ",
        radioButtons(
            'status',
            'Status:',
            choices = c('Em processo', 'Aprovado', 'Edital')
        ),
        radioButtons(
            'reforma',
            'Preço de reforma:',
            choices = c("R$420/m²","R$550/m²","R$650/m²")
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
                    infoBoxOutput('estima_aluguel',width=6),
                    infoBoxOutput('estima_venda',width=6),
                    infoBoxOutput('compara',width=6),
                    infoBoxOutput('metroq',width=6),
                    infoBoxOutput('caprate',width=6)
                )),
        
        #uniao
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
    
    
    #grafico para distribuicao de metragem
    output$metragem <- renderPlotly({
        plot_ly(data = banco_filtro(),x=~area_util_m2,type = 'histogram',bingroup=1) %>%
            layout(title= 'Distribuição de metragem',
                   xaxis=list(title='Área Útil em metros quadrados'),
                   bargap=0.1)
        
    })
    
    #gerar o mapa
    output$mapa <-  renderLeaflet({
        suppressWarnings({leaflet(banco_filtro()) %>%
                addTiles() %>%
                addMarkers(lng = ~longitude,
                           lat = ~latitude,
                           popup = ~paste('R$',get(input$procura),'com',area_util_m2,'metros quadrados','ID',ID))})
    })
    
    
    
    #estimacao
    
    
    
    # valores estimados
    data_est <- reactive({
        
        
        
        # todos os inputs relevantes para estimacao entram nesse data.frame
        banco_predicao <- data.frame(`Area Util`=input$m2,
                                     Condominio=input$condo,
                                     Vagas=as.numeric(input$vaga>=1),
                                     Quartos=input$quarto,
                                     Banheiros=input$ban,
                                     area_util_m2=input$m2,
                                     vagas=input$vaga,
                                     quartos=input$quarto,
                                     banheiros=input$ban,
                                     bairro=def_bairro(input$local,input$imovel),
                                     check.names = F
        )
        
        
        #estimacao do valor do aluguel
        valor_aluguel <- switch(input$local, 
                                norte = round((0.3*predict.lm(modelo_aluguel_asa_norte,newdata=banco_predicao)+1)^(1/0.3),2),
                                
                                sul = round(exp(predict.lm(modelo_aluguel_asa_sul,newdata=banco_predicao)),2),
                                
                                lago = round(predict.lm(modelo_aluguel_lago_sul,newdata=banco_predicao),2)
        )
        #estimacao do valor de venda
        valor_venda <- switch(input$imovel,
                              apt = round((-0.05*predict.lm(modelo_venda_apt,newdata=banco_predicao)+1)^(1/-0.05),2),
                              casa = round(exp(predict.lm(modelo_venda_casa,newdata=banco_predicao)),2)
        )
        
        
        return(c(valor_aluguel,valor_venda))
    })
    
    
    data_diff <- reactive({abs(data_est()[1]-input$alug)})
    
    
    
    
    
    
    output$estima_aluguel <- renderInfoBox({
        infoBox(tags$p("Aluguel Estimado",style="font-size: 120%;",),
                tags$p(
                    paste('R$',suppressWarnings({format(data_est()[1],decimal.mark = ',',big.mark = '.')})),
                    style="font-size: 150%;"
                )
                
                
                
        )})
    
    output$estima_venda <- renderInfoBox({
        infoBox(tags$p("Venda Estimado",style="font-size: 120%;",),
                tags$p(
                    paste('R$',suppressWarnings({format(data_est()[2],decimal.mark = ',',big.mark = '.')})),
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
                    paste('R$',suppressWarnings({format(round(data_est()[1]/input$m2,2),decimal.mark = ',',big.mark = '.')}),'/M²'),
                    style="font-size: 150%;"
                )
                
                
                
        )})
    
    
    output$caprate <- renderInfoBox({
        infoBox(tags$p("Cap rate",style="font-size: 120%;"),
                tags$p(
                    paste(suppressWarnings({format(round(data_est()[1]*100/data_est()[2],2),decimal.mark = ',',big.mark = '.')}),'% por mês'),
                    style="font-size: 150%;"
                )
                
                
                
        )})
    
    ###uniao#########
    
    uniao_filtro <- reactive(
        if(input$status == "Em processo"){
            if(input$reforma == "R$420/m²"){
                uniao_processo$`Preço Sugerido` <- uniao_processo$`Preço Estimado` - processo$ref1
                uniao_processo[,-c(4,5)]
            } else if (input$reforma == "R$550/m²"){
                uniao_processo$`Preço Sugerido` <- uniao_processo$`Preço Estimado` - processo$ref2
                uniao_processo[,-c(3,5)]
            } else {
                uniao_processo$`Preço Sugerido` <- uniao_processo$`Preço Estimado` - processo$ref3
                uniao_processo[,-c(3,4)]
            }
        } else if (input$status == "Aprovado"){
            if(input$reforma == "R$420/m²"){
                uniao_aprovado$`Preço Sugerido` <- uniao_aprovado$`Preço Estimado` - aprovado$ref1
                uniao_aprovado[,-c(4,5)]
            } else if (input$reforma == "R$550/m²"){
                uniao_aprovado$`Preço Sugerido` <- uniao_aprovado$`Preço Estimado` - aprovado$ref2
                uniao_aprovado[,-c(3,5)]
            } else {
                uniao_aprovado$`Preço Sugerido` <- uniao_aprovado$`Preço Estimado` - aprovado$ref3
                uniao_aprovado[,-c(3,4)]
            }
        } else {
            if (input$reforma == "R$420/m²"){
                sugerido <- uniao_edital$`Preço Estimado` - edital$ref1
                diferenca <- sugerido - edital$precouniao
                uniao_edital$`Preço Sugerido` <- sugerido
                uniao_edital$`Comparação (R$)` <- diferenca
                uniao_edital$`Comparação (%)` <- paste(round((diferenca/sugerido)*100,1),"%",sep="")
                uniao_edital[,-c(4,5)]
            } else if (input$reforma == "R$550/m²"){
                sugerido <- uniao_edital$`Preço Estimado` - edital$ref2
                diferenca <- sugerido - edital$precouniao
                uniao_edital$`Preço Sugerido` <- sugerido
                uniao_edital$`Comparação (R$)` <- diferenca
                uniao_edital$`Comparação (%)` <- paste(round((diferenca/sugerido)*100,1),"%",sep="")
                uniao_edital[,-c(3,5)]
            } else {
                sugerido <- uniao_edital$`Preço Estimado` - edital$ref3
                diferenca <- sugerido - edital$precouniao
                uniao_edital$`Preço Sugerido` <- sugerido
                uniao_edital$`Comparação (R$)` <- diferenca
                uniao_edital$`Comparação (%)` <- paste(round((diferenca/sugerido)*100,1),"%",sep="")
                uniao_edital[,-c(3,4)]
            }
        }
    )
    
    
    output$tabela <- renderDataTable(uniao_filtro())
    
    
    
    
    
}


######### rodar o shiny ###############
ui <- dashboardPage(header=titulo,sidebar=menu_lateral,body=corpo)
shinyApp(ui = ui, server = server)

