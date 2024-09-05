# Instalação e carregamento dos pacotes
library(shiny)
library(shinydashboard)
suppressPackageStartupMessages(library(tidyverse))
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(readr)
library(geobr)
library(RColorBrewer)

# Variáveis de configuração da API
url = "https://info.dengue.mat.br/api/alertcity?"

geocodes = c(3501707, 3503208, 3506706, 3507407, 3510104, 3513702, 
              3514007, 3514304, 3515608, 3516853, 3519303, 3519600, 
              3522703, 3529302, 3532058, 3532900, 3540705, 3542909, 
              3543709, 3546504, 3546900, 3547502, 3548906, 3552700, 
              3553708, 3554755)
disease = "dengue"
format = "csv"
ew_start = 1
ew_end = 32
ey_start = 2024
ey_end = 2024

# Carregar os dados para cada geocode
dados_lista = list()
for (geocode in geocodes) {
  cons1 = paste0(url,"geocode=",geocode,"&disease=",disease,"&format=",format,
                  "&ew_start=",ew_start,"&ew_end=",ew_end,
                  "&ey_start=",ey_start,"&ey_end=",ey_end)
  dados = read_csv(cons1, show_col_types=FALSE) %>% arrange(data_iniSE)
  dados_lista[[as.character(geocode)]] = dados
}

# Carregar os dados geoespaciais de todos os municípios do Brasil
municipios_brasil = read_municipality(year = 2022)

# Filtrar para o estado de São Paulo
municipios_sp = municipios_brasil %>% filter(abbrev_state == "SP")

# Mesclar com os dados dos municípios (Região Administrativa Central)
municipios_central = municipios_sp %>% filter(code_muni %in% geocodes)

# Exemplo simples de datas de início de semanas epidemiológicas (para substituir pelos dados reais)
inicio_semanas = seq(as.Date("2023-12-31"), as.Date("2024-08-25"), by = "week")

# Formatar as datas como "dia/mês/ano"
semanas_formatadas = format(inicio_semanas, "%d/%m/%Y")

#interface do aplicativo
ui = dashboardPage(
  dashboardHeader(title = "AMDENV SC"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introdução", tabName = "introducao", icon = icon("info-circle")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      selectInput("municipio", "Município:", choices = geocodes, selected = geocodes[1]),
      selectInput("semanas_inicio", "Semana Epidemiológica (Início):", choices = semanas_formatadas, selected = semanas_formatadas[1]),
      selectInput("semanas_fim", "Semana Epidemiológica (Fim):", choices = semanas_formatadas, selected = semanas_formatadas[length(semanas_formatadas)])
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "introducao",
              fluidRow(
                  box(
                    title = "Informações sobre a dengue",
                    p("A dengue é uma infecção viral transmitida pela picada do mosquito Aedes aegypti. 
                    Essa doença pode se apresentar em diversos níveis, mas sintomas comuns são:"),
                    tags$ul(
                      tags$li("Febre alta de início repentino"),
                      tags$li("Dor de cabeça"),
                      tags$li("Dores musculares e/ou articulares"),
                      tags$li("Dor atrás dos olhos")
                    ),
                    p("Com o declínio da febre (entre o terceiro e sétimo dia da doença), podem haver indicíos de pioras no indivíduo infectado. 
                      Em casos mais graves, a dengue pode evoluir para complicações sérias, como hemorragias e choque, que exigem atenção médica imediata."),
                    p("Assim, é importante estar atento a sinais de alerta e buscar atendimento médico caso os sintomas se agravem a qualquer momento.
                      Prevenir a dengue é um dever de todos: a vigilância e o controle de focos do mosquito são cruciais para o combate da dengue."),
                    width = 12
                  ),
                box(
                  title = "Informações sobre o projeto 'Análise municipal do vírus da dengue na região de São Carlos' (AMDENV SC)",
                  tabBox(
                    id = "introducao_tabs",
                    width = 12,
                    tabPanel("Metodologia", 
                             p("Esse projeto tem como objetivo responder algumas dúvidas norteadoras relacionadas ao vírus da dengue (DENV) que possam ser
                             de interesse para a população da região administrativa central do estado de São Paulo. A região administrativa central do estado 
                             de São Paulo reúne 26 municípios, sendo São Carlos e Araraquara as duas cidades-sede."),
                             p("O aplicativo foi criado por meio do pacote Shiny na linguagem R e está hospedado no serviço GitHub Pages. Os principais pacotes
                               do R que auxiliaram na construção dos gráficos e mapas foram: tidyverse, dplyr, ggplot2, geobr, leaflet, sf, plotly."),
                             p("O intervalo de tempo dos dados coletados está entre o início de 2022 e a semana mais recente do ano, no intuito de que o aplicativo
                               sirva para analisar padrões de incidência e permita ao usuário entender quais períodos do ano ele deve ter uma cautela maior com relação 
                               ao vírus. Para isso, gráficos de números de casos, taxas de incidência e temperatura/umidade foram disponilizados no aplicativo para que 
                               a progressão semanal dessas métricas pudessem ser acompanhadas."),
                             p("Espera-se que esses gráficos, em conjunto com os mapas disponibilizados sobre níveis de alerta e transmissão sustentada, ofereçam uma
                               interface de fácil entendimento para os usuários e que as dúvidas norteadoras do projeto sejam respondidas com os mesmos.")
                             
                    ),
                    tabPanel("Banco de dados", 
                             p("O conjunto de dados que alimenta os dashboards presentes nesse aplicativo é proveniente do projeto InfoDengue, 
                               um sistema de alerta para arboviroses que conta com apoio do Ministério da Saúde. Esse sistema integra múltiplas
                               fontes de dados para melhorar sua vigilância epidemiológica, como dados de temperatur, umidade (fatores que 
                               influenciam a proliferação do mosquito Aedes aegypti) e busca por sinais de prolifração de sintomas nas redes sociais."),
                             p("Os municípios incluídos nas análises desse projeto são: Américo Brasiliense, Araraquara, Boa Esperança do Sul, Borborema, Cândido Rodrigues,
                               Descalvado, Dobrada, Dourado, Fernando Prestes, Gavião Peixoto, Ibaté, Ibitinga, Itápolis, Matão, Motuca, Nova Europa, Porto Ferreira, 
                               Ribeirão Bonito, Rincão, Santa Ernestina, Santa Lúcia, Santa Rita do Passa Quatro, São Carlos, Tabatinga, Taquaritinga, Trabiju"),
                             p("As variáveis presentes no conjunto de dados são:"),
                             tags$ul(
                               tags$li("data_ini_SE : Primeiro dia da semana epidemiológica (Domingo)"),
                               tags$li("SE: Semana epidemiológica"),
                               tags$li("casos_est : Número estimado de casos por semana usando o modelo de nowcasting"),
                               tags$li("cases_est_min and cases_est_max: Intervalo de credibilidade de 95% do número estimado de casos"),
                               tags$li("casos: Número de casos notificados por semana (Os valores são atualizados retrospectivamente todas as semanas)"),
                               tags$li("p_rt1: Probabilidade de (Rt> 1). Para emitir o alerta laranja, usamos o critério p_rt1> 0,95 por 3 semanas ou mais."),
                               tags$li("p_inc100k: Taxa de incidência estimada por 100.000"),
                               tags$li("Localidade_id: Divisão submunicipal (atualmente implementada apenas no Rio de Janeiro)"),
                               tags$li("nivel: Nível de alerta (1 = verde, 2 = amarelo, 3 = laranja, 4 = vermelho), mais detalhes, consulte (Saiba mais)"),
                               tags$li("id: Índice numérico"),
                               tags$li("versao_modelo: Versão do modelo (uso interno)"),
                               tags$li("Rt: Estimativa pontual do número reprodutivo de casos, ver Saiba Mais"),
                               tags$li("pop: População estimada (IBGE)"),
                               tags$li("tempmin: Média das temperaturas mínimas diárias ao longo da semana"),
                               tags$li("tempmed: Média das temperaturas diárias ao longo da semana"),
                               tags$li("tempmax: Média das temperaturas máximas diárias ao longo da semana"),
                               tags$li("umidmin: Média da umidade relativa mínima diária do ar ao longo da semana"),
                               tags$li("umidmed: Média da umidade relativa diária do ar ao longo da semana"),
                               tags$li("umidmax: Média da umidade relativa máxima diária do ar ao longo da semana"),
                               tags$li("receptivo: Indica receptividade climática, ou seja, condições para alta capacidade vetorial. 0 = desfavorável, 1 = favorável, 2 = favorável nesta semana e na semana passada, 3 = favorável por pelo menos três semanas (suficiente para completar um ciclo de transmissão)"),
                               tags$li("transmissao: Evidência de transmissão sustentada: 0 = nenhuma evidência, 1 = possível, 2 = provável, 3 = altamente provável"),
                               tags$li("nivel_inc: Incidência estimada abaixo do limiar pré-epidemia, 1 = acima do limiar pré-epidemia, mas abaixo do limiar epidêmico, 2 = acima do limiar epidêmico"),
                               tags$li("notif_accum_year: Número acumulado de casos no ano")
                             )
                    ),
                    tabPanel("Perguntas norteadoras do projeto",
                             p("Busca-se responder as seguintes perguntas relacionadas aos casos de dengue na região administrativa central do estado de São Paulo:"),
                             tags$ul(
                               tags$li("Qual a situação atual dos casos de dengue nessa região? Quais cidades apresentam os maiores números de casos?"),
                               tags$li("Como a taxa de incidência está evoluindo?"),
                               tags$li("Quais são as condições climáticas atuais? Devemos estar atentos a períodos de chuva?"),
                               tags$li("Qual é o nível de alerta atual para minha cidade? Como posso interpretar cada nível?")
                             )
                    )
                  ) ))),
      tabItem(tabName = "dashboard",
              fluidRow(
                infoBoxOutput("populationInfoBox"),
                infoBoxOutput("casesInfoBox"),
                infoBoxOutput("incidenceRateInfoBox")
              ),
              fluidRow(
                infoBoxOutput("avgTempInfoBox"),
                infoBoxOutput("humidityInfoBox"),
                infoBoxOutput("climateReceptivityInfoBox")
              ),
              fluidRow( 
                infoBoxOutput("probRtInfoBox"),
                infoBoxOutput("alertLevelInfoBox"),
                infoBoxOutput("transmissaoInfoBox")
              ),
              fluidRow(
                box(plotlyOutput("grafico_casos_notificados", height = 500), width = 6),
                box(plotOutput("plot_taxa_incidencia", height = 500), width = 6),
                box(plotOutput("grafico_receptividade", height = 200), width = 6),
                box(plotOutput("grafico_prob_transmissao", height = 200), width = 6),
                box(plotOutput("grafico_temperatura", height = 500), width = 6), 
                box(plotOutput("grafico_umidade", height = 500), width = 6),
                box(title = "Nível de alerta", leafletOutput("mapa_nivel_alerta", height = 500), width = 6),
                box(title = "Evidência de transmissão sustentada", leafletOutput("mapa_transmissao", height = 500), width = 6),
                box(plotOutput("grafico_rt", height = 500), width = 6),
                box(plotOutput("grafico_nivel_inc", height = 500), width = 6)
              ))
    )
  )
)



# Servidor
server = function(input, output, session) {
  # Carregar e processar os dados
  dados_lista = reactive({
    dados_lista = list()
    for (geocode in geocodes) {
      cons1 = paste0(url, "geocode=", geocode, "&disease=", disease, "&format=", format,
                      "&ew_start=", ew_start, "&ew_end=", ew_end, "&ey_start=", ey_start, "&ey_end=", ey_end)
      dados = read_csv(cons1, show_col_types = FALSE) %>% arrange(data_iniSE)
      dados_lista[[as.character(geocode)]] = dados
    }
    dados_lista
  })
  
  # Atualizando o seletor de municípios
  observe({
    choices = setNames(as.character(geocodes), municipios_central$name_muni)
    updateSelectInput(session, "municipio", choices = choices)
  })
  
  # Atualizando o seletor de semanas epidemiológicas com base no município selecionado
  observe({
    dados = dados_lista()[[input$municipio]]
    semanas_disponiveis = format(dados$data_iniSE, "%d/%m/%Y")
    updateSelectInput(session, "semanas_inicio", choices = semanas_disponiveis, selected = semanas_disponiveis[1])
    updateSelectInput(session, "semanas_fim", choices = semanas_disponiveis, selected = semanas_disponiveis[length(semanas_disponiveis)])
  })
  
  # Filtrando os dados com base no município e semanas epidemiológicas selecionadas
  dados_filtrados = reactive({
    dados = dados_lista()[[input$municipio]]
    semana_inicio = as.Date(input$semanas_inicio, format = "%d/%m/%Y")
    semana_fim = as.Date(input$semanas_fim, format = "%d/%m/%Y")
    dados = dados %>% filter(data_iniSE >= semana_inicio & data_iniSE <= semana_fim)
    dados
  })
  
  # Output para o InfoBox da população
  output$populationInfoBox = renderInfoBox({
    dados = dados_filtrados()
    populacao = dados %>%
      pull(pop) %>%
      first()  
    
    infoBox(
      title = "População",
      value = formatC(as.numeric(populacao), format = "d", big.mark = "."),
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  # Criando output para o InfoBox de casos
  output$casesInfoBox = renderInfoBox({
    dados = dados_filtrados()
    
    casos = dados %>%
      pull(casos) %>%
      sum(na.rm = TRUE)
    
    infoBox(
      title = "Número de casos",
      value = formatC(casos, format = "d", big.mark = "."),
      icon = icon("hospital"),
      color = "red"
    )
  })
  
  # Criando output para o InfoBox da taxa de incidência
  output$incidenceRateInfoBox = renderInfoBox({
    dados = dados_filtrados()
    taxa_incidencia = dados %>%
      pull(p_inc100k) %>%
      first()
    
    infoBox(
      title = "Taxa de incidência",
      value = formatC(as.numeric(taxa_incidencia), format = "f", digits = 2, big.mark = "."),
      icon = icon("chart-line"),
      color = "yellow"
    )
  })
  
  # Criando output para o InfoBox para probabilidade de reprodução > 1
  output$probRtInfoBox = renderInfoBox({
    dados = dados_filtrados()
    prob_rt = dados %>%
      pull(p_rt1) %>%
      first()
    
    infoBox(
      title = "Probabilidade de Rt > 1",
      value = paste0(formatC(prob_rt * 100, format = "f", digits = 1), "%"),
      icon = icon("percent"),
      color = "fuchsia"
    )
  })
  
  # Criando output para o InfoBox temperatura
  output$avgTempInfoBox = renderInfoBox({
    dados = dados_filtrados()
    temp_media = dados %>%
      pull(tempmed) %>%
      first()  # Assume que todos os valores de tempmed são iguais para um mesmo município
    
    infoBox(
      title = "Temperatura média",
      value = paste0(formatC(as.numeric(temp_media), format = "f", digits = 1), "°C"),
      icon = icon("thermometer-half"),
      color = "light-blue"
    )
  })
  
  # Criando output para o InfoBox da umidade
  output$humidityInfoBox = renderInfoBox({
    dados = dados_filtrados()
    umidade_media = dados %>%
      pull(umidmed) %>%
      first()
    
    infoBox(
      title = "Umidade média",
      value = paste0(formatC(as.numeric(umidade_media), format = "f", digits = 1), "%"),
      icon = icon("tint"),
      color = "blue"
    )
  })
  
  # Criando output para o InfoBox da receptividade climática
  output$climateReceptivityInfoBox = renderInfoBox({
    dados = dados_filtrados()
    receptividade_climatica = dados %>%
      pull(round(receptivo)) %>%
      first()
    
    infoBox(
      title = "Receptividade climática",
      value = formatC(as.numeric(receptividade_climatica), format = "f", digits = 1),
      icon = icon("cloud-sun"),
      color = "green"
    )
  })
  
  # InfoBox para o nível de alerta
  output$alertLevelInfoBox = renderInfoBox({
    
    # Filtrando os dados com base no município selecionado no filtro
    dados = dados_filtrados()
    
    # Calculando o nível médio com base nos filtros
    nivel_media = dados %>% summarise(nivel_medio = mean(nivel, na.rm = TRUE))
    nivel_alerta = round(nivel_media$nivel_medio)
    
    # InfoBox com o nível de alerta correspondente (utilizaremos essa paleta de cores como padrão no restante do código)
    infoBox(
      title = "Nível de alerta",
      value = switch(nivel_alerta,
                     "1" = "Baixo",
                     "2" = "Moderado",
                     "3" = "Alto",
                     "4" = "Muito Alto"),
      icon = icon("exclamation-circle"),
      color = switch(nivel_alerta,
                     "1" = "green",
                     "2" = "yellow",
                     "3" = "orange",
                     "4" = "red")
    )
  })
  
  # Criando output para o InfoBox para transmissão
  output$transmissaoInfoBox = renderInfoBox({
    dados = dados_filtrados()
    
    # Cálculo da média de transmissão
    transmissao_media = dados %>%
      summarise(transmissao_media = mean(transmissao, na.rm = TRUE)) %>%
      pull(transmissao_media)
    transmissao_media_arredondada = round(transmissao_media)
    
    # Determinando a cor com base na média de transmissão
    cor_transmissao = case_when(
      transmissao_media_arredondada == 0 ~ "green",
      transmissao_media_arredondada == 1 ~ "yellow",
      transmissao_media_arredondada == 2 ~ "orange",
      transmissao_media_arredondada == 3 ~ "red",
      TRUE ~ "gray"
    )
    
    infoBox(
      title = "Transmissão média",
      value = transmissao_media_arredondada,
      icon = icon("exclamation-triangle"),
      color = cor_transmissao
    )
  })
  
  # Gráfico de casos notificados
  output$grafico_casos_notificados = renderPlotly({
    dados = dados_filtrados()
    nome_municipio = municipios_central$name_muni[municipios_central$code_muni == as.numeric(input$municipio)]
    
    p = ggplot(dados, aes(x = factor(SE), y = casos)) +
      geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
      labs(
        title = paste("Número de casos notificados em", nome_municipio),
        x = "Semana epidemiológica",
        y = "Número de casos"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 12, color = "darkblue"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray"),
        panel.grid.minor = element_line(size = 0.25, linetype = "dashed", color = "lightgray")
      )
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Gráfico de taxa de incidência
  output$plot_taxa_incidencia = renderPlot({
    dados = dados_filtrados()
    nome_municipio = municipios_central$name_muni[municipios_central$code_muni == as.numeric(input$municipio)]
    
    ggplot(data = dados, aes(x = SE, y = p_inc100k)) +
      geom_line(color = "green", size = 1.2) +
      labs(
        title = paste("Taxa de incidência por 100.000 habitantes em", nome_municipio),
        x = "Semana epidemiológica",
        y = "Taxa de incidência"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, color = "darkgreen"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray"),
        panel.grid.minor = element_line(size = 0.25, linetype = "dashed", color = "lightgray")
      )
  })
  
  # Mapa de nível de alerta
  output$mapa_nivel_alerta = renderLeaflet({
    # Filtragem dos dados de acordo com o intervalo de semanas selecionado
    dados_filtrados_por_semana = reactive({
      dados_lista_filtrada = list()
      for (geocode in geocodes) {
        dados = dados_lista()[[as.character(geocode)]]
        semana_inicio = as.Date(input$semanas_inicio, format = "%d/%m/%Y")
        semana_fim = as.Date(input$semanas_fim, format = "%d/%m/%Y")
        dados = dados %>% filter(data_iniSE >= semana_inicio & data_iniSE <= semana_fim)
        dados_lista_filtrada[[as.character(geocode)]] = dados
      }
      dados_lista_filtrada
    })
    
    # Calculando o nível médio com base nos dados filtrados
    nivel_media = lapply(dados_filtrados_por_semana(), function(df) {
      df %>% summarise(nivel_medio = mean(nivel, na.rm = TRUE))
    })
    
    # Convertendo lista para dataframe
    nivel_media_df = do.call(rbind, nivel_media) %>%
      as.data.frame()
    nivel_media_df$geocode = as.numeric(names(nivel_media))
    colnames(nivel_media_df) = c("nivel_medio", "code_muni")
    nivel_medio = as.numeric(nivel_media_df$nivel_medio)
    
    # Atualizando os dados espaciais de cada município
    municipios_central_atualizado = municipios_central %>%
      left_join(nivel_media_df, by = "code_muni")
    
    # Paleta de cores para utilizarmos no mapa abaixo
    pal = colorFactor(palette = c("green", "yellow", "orange", "red"), 
                       levels = c(1, 2, 3, 4))
    
    # Mapa interativo
    leaflet(data = municipios_central_atualizado) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(
        fillColor = ~pal(round(nivel_medio)),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste0("<strong>Município: </strong>", name_muni,
                        "<br><strong>Nível Médio: </strong>", nivel_medio)
      ) %>%
      addLegend(
        pal = pal,
        values = c(1, 2, 3, 4),
        title = "Nível médio",
        position = "bottomright",
        labels = c("Baixo", "Moderado", "Alto", "Muito alto")
      )
  })
  
  # Gráfico de temperaturas médias
  output$grafico_temperatura = renderPlot({
    dados = dados_filtrados()
    ggplot(dados, aes(x = SE)) +
      geom_line(aes(y = tempmed, color = "Temperatura média"), size = 1) +
      geom_ribbon(aes(ymin = tempmin, ymax = tempmax), alpha = 0.3, fill = "lightblue") +
      labs(
        title = "Temperatura média com intervalo de variação",
        x = "Semana epidemiológica",
        y = "Temperatura (°C)",
        color = "Legenda"
      ) +
      scale_color_manual(values = c("Temperatura Média" = "blue")) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, color = "darkblue"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray"),
        panel.grid.minor = element_line(size = 0.25, linetype = "dashed", color = "lightgray")
      )
  })
  
  # Gráfico de umidade relativa
  output$grafico_umidade = renderPlot({
    dados = dados_filtrados()
    ggplot(dados, aes(x = SE)) +
      geom_line(aes(y = umidmed, color = "Umidade média"), size = 1) +
      geom_ribbon(aes(ymin = umidmin, ymax = umidmax), alpha = 0.3, fill = "lightgreen") +
      labs(
        title = "Umidade média com intervalo de variação",
        x = "Semana epidemiológica",
        y = "Umidade (%)",
        color = "Legenda"
      ) +
      scale_color_manual(values = c("Umidade média" = "green")) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, color = "darkgreen"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray"),
        panel.grid.minor = element_line(size = 0.25, linetype = "dashed", color = "lightgray")
      )
  })
  
  # Gráfico de receptividade climática
  output$grafico_receptividade = renderPlot({
    dados = dados_filtrados()
    ggplot(dados, aes(x = SE, y = receptivo)) +
      geom_area(fill = "pink", alpha = 0.5) +
      geom_line(color = "pink", size = 1.2) +
      labs(title = "Receptividade climática",
           x = "Semana epidemiológica", y = "Nível de receptividade") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, color = "purple"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray"),
        panel.grid.minor = element_line(size = 0.25, linetype = "dashed", color = "lightgray")
      )
  })
  
  # Gráfico de probabilidade de transmissão
  output$grafico_prob_transmissao = renderPlot({
    dados = dados_filtrados()
    ggplot(dados, aes(x = SE, y = p_rt1)) +
      geom_area(fill = "orange", alpha = 0.5) +
      geom_line(color = "orange", size = 1.2) +
      labs(title = "Probabilidade de transmissão (Rt > 1)",
           x = "Semana epidemiológica", y = "Probabilidade") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, color = "darkorange"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_line(size = 0.5, linetype = "dashed", color = "gray"),
        panel.grid.minor = element_line(size = 0.25, linetype = "dashed", color = "lightgray")
      )
  })
 
  # Gráfico da variável Rt
  output$grafico_rt = renderPlot({
    dados = dados_filtrados()
    
    dados %>%
      ggplot(aes(x = data_iniSE, y = Rt)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(title = "Estimativa do número reprodutivo (Rt)",
           x = "Semana epidemiológica",
           y = "Rt") +
      theme_minimal()
  })
  
  # Gráfico da variável nivel_inc
  output$grafico_nivel_inc = renderPlot({
    dados = dados_filtrados()
    
    dados %>%
      ggplot(aes(x = data_iniSE, fill = as.factor(nivel_inc))) +
      geom_bar(stat = "count", position = "dodge") +
      labs(title = "Nível de incidência por semana epidemiológica",
           x = "Semana epidemiológica",
           y = "Contagem",
           fill = "Nível de incidência") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set3")  # Escolha uma paleta de cores apropriada
  })
  
  
  # Mapa de transmissão
  output$mapa_transmissao = renderLeaflet({
    # Filtragem dos dados de acordo com o intervalo de semanas que for selecionado
    dados_filtrados_por_semana = reactive({
      dados_lista_filtrada = list()
      for (geocode in geocodes) {
        dados = dados_lista()[[as.character(geocode)]]
        semana_inicio = as.Date(input$semanas_inicio, format = "%d/%m/%Y")
        semana_fim = as.Date(input$semanas_fim, format = "%d/%m/%Y")
        dados = dados %>% filter(data_iniSE >= semana_inicio & data_iniSE <= semana_fim)
        dados_lista_filtrada[[as.character(geocode)]] = dados
      }
      dados_lista_filtrada
    })
    
    # Calculando a média da transmissão com base nos dados filtrados
    transmissao_media = lapply(dados_filtrados_por_semana(), function(df) {
      df %>%
        summarise(transmissao_media = mean(transmissao, na.rm = TRUE))
    })
    
    # Convertendo a lista para dataframe
    transmissao_media_df = do.call(rbind, transmissao_media) %>%
      as.data.frame()
    transmissao_media_df$geocode = as.numeric(names(transmissao_media))
    colnames(transmissao_media_df) = c("transmissao_media", "code_muni")
    
    # Atualizando os dados espaciais dos municípios
    municipios_central_atualizado = municipios_central %>%
      left_join(transmissao_media_df, by = "code_muni")
    
    # Paleta de cores para utilizarmos no mapa abaixo (mantemos as mesmas cores para melhor entendimento)
    pal = colorFactor(palette = c("green", "yellow", "orange", "red"), 
                       levels = c(0, 1, 2, 3))
    
    # Mapa interativo
    leaflet(data = municipios_central_atualizado) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(
        fillColor = ~pal(round(transmissao_media)),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste0("<strong>Município: </strong>", name_muni,
                        "<br><strong>Transmissão média: </strong>", transmissao_media)
      ) %>%
      addLegend(
        pal = pal,
        values = c(0, 1, 2, 3),
        title = "Transmissão média",
        position = "bottomright",
        labels = c("Baixa", "Moderada", "Alta", "Muito alta")
      )
  })
}

# Run do aplicativo
shinyApp(ui, server)

