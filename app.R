if(require(shiny) == F) install.packages("shiny"); require(shiny)
if(require(shinydashboard) == F) install.packages("shinydashboard"); require(shinydashboard)
if(require(shinydashboardPlus) == F) install.packages("shinydashboardPlus"); require(shinydashboardPlus)
if(require(DT) == F) install.packages("DT"); require(DT)
if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
if(require(ggplot2) == F) install.packages("ggplot2"); require(ggplot2)
if(require(ggrepel) == F) install.packages("ggrepel"); require(ggrepel)
if(require(shinyWidgets) == F) install.packages("shinyWidgets"); require(shinyWidgets)
if(require(ggraph) == F) install.packages("ggraph"); require(ggraph)
if(require(tidygraph) == F) install.packages("tidygraph"); require(tidygraph)
if(require(splitstackshape) == F) install.packages("splitstackshape"); require(splitstackshape)
if(require(readxl) == F) install.packages("readxl"); require(readxl)
if(require(scales) == F) install.packages("scales"); require(scales)
if(require(patchwork) == F) install.packages("patchwork"); require(patchwork)

options(scipen = 999)

theme_set(theme_bw() +
            theme(axis.text = element_text(size = 12),
                  legend.text = element_text(size = 12),
                  legend.position = "top"))


notas_maximas <- data.frame(EDITAL = c("Hip Hop", "Quadrilhas", "Salvaguarda",
                                       "Técnicos", "Multilinguagem", "Economia Criativa",
                                       "Festivais", "Museus", "Formação", "Diversidade Cultural",
                                       "Bolsas Artísticas", "Bolsas Brincadeiras", "Mulher Negra", "Comunidades"),
                            nota_maxima = c(50, 40, 60, 50, 70, 50, 70, 60, 65, 65, 30, 40, 50, 50))

pnab <- readRDS("demanda_gedh_pnab.rds") %>% 
  mutate(vaga_utilizada = ifelse(str_detect(VAGA.UTILIZADA, "AMPLA CONCORRÊNCIA") == T, "Ampla Concorrência",
                                 ifelse(str_detect(VAGA.UTILIZADA, "COM DEFICIÊNCIA") == T, "PcD",
                                        ifelse(str_detect(VAGA.UTILIZADA, "PESSOA NEGRA") == T, "Pessoa Negra",
                                               ifelse(str_detect(VAGA.UTILIZADA, "INDÍGENA") == T, "Pessoa Indígena",
                                                      "ERRO"))))) %>% 
  mutate(vaga_utilizada = ifelse(EDITAL == "Mulher Negra" & vaga_utilizada == "Ampla Concorrência", "Pessoa Negra",
                                 vaga_utilizada),
         VAGA.UTILIZADA = NULL) %>% 
  mutate(nota_s_i = as.numeric(nota_s_i),
         nota_final = as.numeric(nota_final)) %>% 
  left_join(notas_maximas, by = "EDITAL") %>% mutate(nota_pad_s_i = (100*nota_s_i)/nota_maxima,
                                                     multiplicador_ind = nota_final/nota_s_i,
                                                     nota_pad_final = nota_pad_s_i*multiplicador_ind)


notas_maximas_lpg <- data.frame(EDITAL = c("AÇÕES CRIATIVAS - PRODUÇÃO", "AÇÕES CRIATIVAS - FINALIZAÇÃO",
                                           "AÇÕES CRIATIVAS - CINECLUBE", "AÇÕES CRIATIVAS - FESTIVAIS",
                                           "AÇÕES CRIATIVAS - DIGITALIZAÇÃO", "AÇÕES CRIATIVAS - ROTEIRO",
                                           "AÇÕES CRIATIVAS - PESQUISA", "CADEIA PRODUTIVA",
                                           "SALAS CINEMA", "LICENCIAMENTO", "FORMAÇÃO",
                                           "DESENVOLVE CULTURA", "MUSEUS", "PERIFERIAS",
                                           "SALVAGUARDA", "AÇÕES CRIATIVAS", "FESTIVAIS", "TECNICOS"),
                            nota_maxima = c(80, 80, 80, 80, 80, 80, 80, 70, 70, 32, 60, 100, 60, 50, 50, 80, 90, 35))

lpg <- readRDS("demanda_gedh_lpg.rds") %>% 
  mutate(inducao = ifelse(inducao == "Comunidades Tradicionais", "Povos e Comunidades",
                          ifelse(inducao == "Identidade Racial/Cor", "Pessoa Negra",
                                 ifelse(inducao == "Mulheres", "Mulher/Travesti",
                                        ifelse(inducao == "Não Se Enquadra", "Ampla Concorrência",
                                               ifelse(inducao == "Pcd", "PcD",
                                                      inducao))))),
         raca = case_when(
           raca == "Indígena/povos originários" ~ "Indígena",
           raca == "Não declarada" ~ "Não declarar", 
           TRUE ~ raca
         )) %>%  
  left_join(notas_maximas_lpg, by = "EDITAL") %>% mutate(nota_pad_s_i = (100*nota_s_i)/nota_maxima,
                                                         multiplicador_ind = nota_final/nota_s_i,
                                                         nota_pad_final = nota_pad_s_i*multiplicador_ind) %>% 
  mutate(EDITAL = case_when(EDITAL_AUDIOVISUAL == "SIM" ~ paste0("A - ", EDITAL),
                            TRUE ~ paste0("M - ", EDITAL)))


pnab_no_mn <- readRDS("demanda_gedh_pnab.rds") %>% filter(EDITAL != "Mulher Negra")
pnab_mn <- readRDS("demanda_gedh_pnab.rds") %>% filter(EDITAL == "Mulher Negra")


status <- unique(pnab$status)
edital_pnab <- sort(unique(pnab$EDITAL))

edital_lpg <- lpg %>% select(EDITAL) %>% unique() %>% pull() %>% sort()
macrorreg <- sort(unique(pnab$macrorreg))

indutor <- sort(unique(pnab_no_mn$inducao))

raca <- sort(unique(pnab$raca))

genero <- sort(unique(pnab$genero))

rd_municipio <- read_xlsx("RD_Municipio.xlsx") %>% mutate(macrorreg_new = ifelse(macrorreg_new == "Mata", "Zona da Mata",
                                                                                 macrorreg_new))

pnab <- pnab %>% left_join(rd_municipio, by = "municipio")

rd <- sort(unique(pnab$rd))

cruzamentos <- c("genero", "raca", "macrorreg", "escolaridade", "renda")
edital_tipo <- sort(unique(pnab$EDITAL_TIPO))

df_edital <- pnab %>% select(EDITAL, EDITAL_TIPO) %>% unique()

lei <- c("LPG", "PNAB")



user_base <- tibble::tibble(
  user = c("user1", "user2"),
  password = sapply(c("pass1", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)


header <- dashboardHeader(title = span("PNAB LPG",
                                       style = "font-weight: bold;"))


sidebar <- dashboardSidebar(
  collapsed = T,
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Cota PNAB", tabName = "cota", icon=icon("people-roof", lib = "font-awesome")),
              menuItem("Indução PNAB", tabName = "inducao", icon=icon("users", lib = "font-awesome")),
              menuItem("PNAB sem Política Social", tabName = "contrafactual", icon=icon("star-half-stroke", lib = "font-awesome")),
              menuItem("LPG x PNAB", tabName = "lpgxpnab", icon=icon("scale-unbalanced", lib = "font-awesome")),
              menuItem("Outros Cruzamentos", tabName = "cruzamento", icon=icon("table", lib = "font-awesome"))
              ))


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "cota",
            fluidRow(
              column(2,
                     pickerInput("tipo1", "Escolha o Tipo de Edital", choices = edital_tipo, selected = edital_tipo,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("edital1", "Selecione um ou mais Editais", choices = edital_pnab, selected = edital_pnab,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("raca1", "Selecione uma ou mais Etnias", choices = raca, selected = raca,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("macrorreg1", "Escolha a Macrorregião", choices = macrorreg, selected = macrorreg,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T))
            ),
            fluidRow(
              column(10,
                     box(plotOutput("plotcota1", height = 550), title = "Distribuição das Cotas", width = 11,
                         solidHeader = TRUE, status = "success")),
              column(2,
                     downloadButton("downloadplotecota1", "Baixar Gráfico")
              )),
            fluidRow(
              column(11,
                     dataTableOutput("tabelacota1"), style="text-align:center"
              ))
    ),
    tabItem(tabName = "inducao",
            fluidRow(
              column(2,
                     pickerInput("edital3.1", "Edital de Mulher Negra", choices = c("Sim", "Não"), selected = "Não",
                                 options = list(`actions-box` = TRUE),multiple = F)),
              column(2,
                     pickerInput("tipo3", "Escolha o Tipo de Edital", choices = edital_tipo, selected = edital_tipo,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("edital3", "Selecione um ou mais Editais", choices = edital_pnab, selected = edital_pnab,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("raca3", "Selecione uma ou mais Etnias", choices = raca, selected = raca,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("genero3", "Selecione um ou mais gênero", choices = genero, selected = genero,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("macrorreg3", "Escolha a Macrorregião", choices = macrorreg, selected = macrorreg,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("rd3", "Escolha a Região de Desenvolvimento", choices = rd, selected = rd,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T))
          ),
          
          fluidRow(
            column(10,
                   box(plotOutput("plotinducaotipo3", height = 500), title = "Aproveitamento por Tipo de Edital", width = 11,
                       solidHeader = TRUE, status = "success")),
            column(2,
                   downloadButton("downloadplotinducao3", "Baixar Gráfico")
            )),
          fluidRow(
            column(10,
                   box(plotOutput("plotinducaoedital3", height = 500), title = "Distribuição das Induções x Edital", width = 10,
                       solidHeader = TRUE, status = "success")),
            column(2,
                   downloadButton("downloadplotinducao3", "Baixar Gráfico")
            )),
            fluidRow(
              column(10,
                     box(plotOutput("plotinducao3", height = 1000), title = "Distribuição das Induções", width = 10,
                         solidHeader = TRUE, status = "success")),
              column(2,
                     downloadButton("downloadplotinducao3", "Baixar Gráfico")
              )),
            fluidRow(
              column(11,
                     dataTableOutput("tabela3"), style="text-align:center"
              ))
    ),
    tabItem(tabName = "contrafactual",
            fluidRow(
              column(2,
                     pickerInput("tipo2", "Escolha o Tipo de Edital", choices = edital_tipo, selected = edital_tipo,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("edital2", "Selecione um ou mais Editais", choices = edital_pnab, selected = edital_pnab,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("raca2", "Selecione uma ou mais Etnias", choices = raca, selected = raca,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("genero2", "Selecione um ou mais gênero", choices = genero, selected = genero,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("macrorreg2", "Escolha a Macrorregião", choices = macrorreg, selected = macrorreg,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("rd2", "Escolha a Região de Desenvolvimento", choices = rd, selected = rd,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T))
            ),
            fluidRow(
              column(10,
                     box(plotOutput("plotraca2", height = 550), title = "Distribuição por Grupo Étnico Racial", width = 11,
                         solidHeader = TRUE, status = "success")),
              column(2,
                     downloadButton("downloadplotraca2", "Baixar Gráfico")
              )),
            fluidRow(
              column(10,
                     box(plotOutput("plotgenero2", height = 550), title = "Distribuição  por Gênero", width = 11,
                         solidHeader = TRUE, status = "success")),
              column(2,
                     downloadButton("downloadplotgenero2", "Baixar Gráfico")
              )),
            fluidRow(
              column(10,
                     box(plotOutput("plotreg2", height = 550), title = "Distribuição por Macrorregião", width = 11,
                         solidHeader = TRUE, status = "success")),
              column(2,
                     downloadButton("downloadplotreg2", "Baixar Gráfico")
              )),
            fluidRow(
              column(10,
                     box(plotOutput("plotrd2", height = 550), title = "Distribuição por Região de Desenvolvimento", width = 11,
                         solidHeader = TRUE, status = "success")),
              column(2,
                     downloadButton("downloadplotrd2", "Baixar Gráfico")
              )),
            fluidRow(
              column(11,
                     dataTableOutput("tabela2"), style="text-align:center"
              ))
    ),
    tabItem(tabName = "lpgxpnab",
            fluidRow(
              column(2,
                     pickerInput("editallpg", "Editais LPG", choices = edital_lpg, selected = edital_lpg,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("editalpnab", "Editais PNAB", choices = edital_pnab[-9], selected = edital_pnab[-9],
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("raca4", "Selecione uma ou mais Etnias", choices = raca, selected = raca,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("genero4", "Selecione um ou mais gênero", choices = genero, selected = genero,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("macrorreg4", "Escolha a Macrorregião", choices = macrorreg, selected = macrorreg,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("rd4", "Escolha a Região de Desenvolvimento", choices = rd, selected = rd,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T))
            ),
            fluidRow(
              column(11,
                     box(plotOutput("plotlpgpnab4", height = 550), title = "Distribuição das Induções", width = 11,
                         solidHeader = TRUE, status = "success")),
              column(2,
                     downloadButton("downloadplotlpgpnab4", "Baixar Gráfico")
              )),
            fluidRow(
              column(11,
                     dataTableOutput("tabela4"), style="text-align:center"
              ))
    ),
    tabItem(tabName = "cruzamento",
            fluidRow(
              column(2,
                     pickerInput("lpgaudiovisual5", "LPG do Audiovisual?", choices = c("SIM", "NÃO"), selected = c("SIM", "NÃO"),
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("tipo5", "Tipo Edital PNAB", choices = edital_tipo, selected = edital_tipo,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("editallpg5", "Editais LPG", choices = edital_lpg, selected = edital_lpg,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("editalpnab5", "Editais PNAB", choices = edital_pnab, selected = edital_pnab,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T)),
              column(2,
                     pickerInput("status5", "Selecione o Status", choices = status, selected = status,
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T))
            ),
            fluidRow(
              column(2,
                     pickerInput("variaveis52", "Variáveis LPG", choices = c("Cor ou Raça", "Gênero",
                                                                             "Ecolaridade", "Renda",
                                                                             "Macrorregião", "Edital"),
                                 options = list(`actions-box` = TRUE,
                                                `live-search`=TRUE),multiple = T))
            ),
            fluidRow(
              column(11,
                     box(div(style="text-align:center", DTOutput("tabela51")), title = "PNAB",
                         solidHeader = TRUE, status = "success", width = 11)
              )),
            fluidRow(
              column(11,
                     box(div(style="text-align:center", DTOutput("tabela52")), title = "LPG",
                         solidHeader = TRUE, status = "success", width = 11)
              ))
    )
  )
)




ui <- dashboardPage(header, sidebar, body,
                    controlbar = dashboardControlbar(),
                    footer = dashboardFooter(),
                    #setBackgroundImage(src = "https://raw.githubusercontent.com/observatorio-secult/pnab/main/Dashboard_PNAB2.png", shinydashboard = TRUE)
)


server <- function(input, output, session) {
  
  data51 <- reactive(
    lpg %>% 
      rename("Cor ou Raça" = raca, 
             "Gênero" = genero,
             "Ecolaridade" = escolaridade,
             "Renda" = renda,
             "Macrorregião" = macrorreg, 
             "Edital" = EDITAL) %>% 
      filter(status %in% input$status5) %>% 
      filter(EDITAL_AUDIOVISUAL %in% input$lpgaudiovisual5) %>% 
      filter(Edital %in% input$editallpg5) %>% 
      group_by_at(input$variaveis52) %>% summarise(Inscritos = n(),
                                                   `Nota Mínima` = round(min(nota_pad_final),2),
                                                   `Nota Média` = round(mean(nota_pad_final),2),
                                                   `Nota Máxima` = round(max(nota_pad_final),2)) %>% 
      mutate(`Inscritos (%)` = percent(Inscritos/sum(Inscritos), accuracy = 0.01)) %>% 
      arrange(-Inscritos)
  )
  
  data52 <- reactive(
    pnab %>% 
      rename("Cor ou Raça" = raca, 
             "Gênero" = genero,
             "Ecolaridade" = escolaridade,
             "Renda" = renda,
             "Macrorregião" = macrorreg, 
             "Edital" = EDITAL) %>% 
      filter(status %in% input$status5) %>% 
      filter(EDITAL_TIPO %in% input$tipo5) %>% 
      filter(Edital %in% input$editalpnab5) %>% 
      group_by_at(input$variaveis52) %>% summarise(Inscritos = n(),
                                                   `Nota Mínima` = round(min(nota_pad_final),2),
                                                   `Nota Média` = round(mean(nota_pad_final),2),
                                                   `Nota Máxima` = round(max(nota_pad_final),2)) %>% 
      mutate(`Inscritos (%)` = percent(Inscritos/sum(Inscritos), accuracy = 0.01)) %>% 
      arrange(-Inscritos)
  )
  
  output$tabela52 <- renderDT(
    data51(),
    filter = "top",
    options = list(pageLength = 10)
    
 )
  
  output$tabela51 <- renderDT(
    data52(),
    filter = "top",
    options = list(pageLength = 10)
    
  )
  
  datatab3 <- reactive({
    if(input$edital3.1 == "Sim"){
      pnab %>% filter(EDITAL == "Mulher Negra") %>% 
        filter(status == "Selecionada") %>% 
        filter(EDITAL_TIPO %in% input$tipo3) %>% 
        filter(EDITAL_TIPO %in% input$tipo3) %>% 
        filter(EDITAL %in% input$edital3) %>% 
        filter(raca %in% input$raca3) %>% 
        filter(genero %in% input$genero3) %>% 
        filter(macrorreg %in% input$macrorreg3) %>% 
        filter(rd %in% input$rd3) %>% 
        cSplit('inducao', sep = ";", direction = "long") %>% 
        group_by(EDITAL_TIPO,
                 EDITAL) %>% summarise(selecionados = n(),
                                        `Aproveitamento Médio sem Indução` = round(mean(nota_pad_s_i),2),
                                       `Aproveitamento Médio com Indução` = round(mean(nota_pad_final),2),
                                       `Aproveitamento Mínimo sem Indução` = round(min(nota_pad_s_i),2),
                                       `Aproveitamento Mínimo com Indução` = round(min(nota_pad_final),2),
                                       `Aproveitamento Máximo sem Indução` = round(max(nota_pad_s_i),2),
                                       `Aproveitamento Máximo com Indução` = round(max(nota_pad_final),2)) %>% 
        mutate(prop_selecionados = percent(selecionados/sum(selecionados), accuracy = 0.01))
    } else {
      pnab %>% filter(EDITAL != "Mulher Negra") %>% 
        filter(status == "Selecionada") %>% filter(EDITAL_TIPO %in% input$tipo3) %>% 
        filter(EDITAL_TIPO %in% input$tipo3) %>% 
        filter(EDITAL %in% input$edital3) %>% 
        filter(raca %in% input$raca3) %>% 
        filter(genero %in% input$genero3) %>% 
        filter(macrorreg %in% input$macrorreg3) %>% 
        filter(rd %in% input$rd3) %>% 
        group_by(EDITAL_TIPO,
                 EDITAL) %>% summarise(selecionados = n(),
                                       `Aproveitamento Médio sem Indução` = round(mean(nota_pad_s_i), 2),
                                       `Aproveitamento Médio com Indução` = round(mean(nota_pad_final), 2),
                                       `Aproveitamento Mínimo sem Indução` = round(min(nota_pad_s_i), 2),
                                       `Aproveitamento Mínimo com Indução` = round(min(nota_pad_final), 2),
                                       `Aproveitamento Máximo sem Indução` = round(max(nota_pad_s_i), 2),
                                       `Aproveitamento Máximo com Indução` = round(max(nota_pad_final), 2)) %>% 
        mutate(prop_selecionados = percent(selecionados/sum(selecionados), accuracy = 0.01))
    }
    
    
  })
  
  output$tabela3 <-  DT::renderDT(server = FALSE, {
    DT::datatable(
      datatab3(),
      extensions = c("Buttons"),
      options = list(
        dom = 'Bfrtip',
        pageLength = 10,
        buttons = list(
          list(extend = "excel", text = "Baixar Planilha", filename = "data",
               exportOptions = list(
                 modifier = list(page = "all")
               )
          )
        )
      )
    )
  })
  
  data3 <- reactive({
    if(input$edital3.1 == "Sim"){
      pnab %>% filter(EDITAL == "Mulher Negra") %>% 
        filter(status == "Selecionada") %>% 
        filter(EDITAL_TIPO %in% input$tipo3) %>% 
        filter(EDITAL %in% input$edital3) %>% 
        filter(raca %in% input$raca3) %>% 
        filter(genero %in% input$genero3) %>% 
        filter(macrorreg %in% input$macrorreg3) %>% 
        filter(rd %in% input$rd3) %>% 
        cSplit('inducao', sep = ";", direction = "long") %>% 
        group_by(inducao) %>% summarise(selecionados = n(),
                                        nota1_media = mean(nota_pad_s_i),
                                        nota2_media = mean(nota_pad_final),
                                        nota1_min = min(nota_pad_s_i),
                                        nota2_min = min(nota_pad_final),
                                        nota1_max = max(nota_pad_s_i),
                                        nota2_max = max(nota_pad_final)) %>% 
        mutate(prop_selecionados = percent(selecionados/sum(selecionados), accuracy = 0.01))
    } else {
      pnab %>% filter(EDITAL != "Mulher Negra") %>% 
        filter(status == "Selecionada") %>% filter(EDITAL_TIPO %in% input$tipo3) %>% 
        filter(EDITAL_TIPO %in% input$tipo3) %>% 
        filter(EDITAL %in% input$edital3) %>% 
        filter(raca %in% input$raca3) %>% 
        filter(genero %in% input$genero3) %>% 
        filter(macrorreg %in% input$macrorreg3) %>% 
        filter(rd %in% input$rd3) %>% 
        group_by(inducao) %>% summarise(selecionados = n(),
                                        nota1_media = mean(nota_pad_s_i),
                                        nota2_media = mean(nota_pad_final),
                                        nota1_min = min(nota_pad_s_i),
                                        nota2_min = min(nota_pad_final),
                                        nota1_max = max(nota_pad_s_i),
                                        nota2_max = max(nota_pad_final)) %>% 
        mutate(prop_selecionados = percent(selecionados/sum(selecionados), accuracy = 0.01))
    }
    
    
  })
  
  plot3 <- reactive({
    g1 <- ggplot(data3(), aes(x = inducao, y = selecionados)) +
      geom_bar(stat = "identity", width = .6, fill = "tomato3") +
      geom_text(aes(label = paste0(selecionados, " (", prop_selecionados, ")")),
                hjust = -.1) +
      labs(x = "Indução", y = "Qtd. Selecionados", title = paste0("Selecionados", " - Total de ",
                                                                                sum(data3() %>% select(selecionados) %>% 
                                                                                      pull()))) +
      theme(axis.text = element_text(size = 12)) +
      coord_flip() +
      expand_limits(y = c(0, max(data3()$selecionados) + max(data3()$selecionados)/4))
    
    df1 <- data3() %>% gather(key = "tipo_nota", value = "nota", nota1_media:nota2_max) %>% 
      filter(tipo_nota %in% c("nota1_media", "nota1_min", "nota1_max")) %>% 
      mutate(tipo_nota = case_when(tipo_nota == "nota1_media" ~ "Nota Média",
                                   tipo_nota == "nota1_min" ~ "Nota Mínima",
                                   tipo_nota == "nota1_max" ~ "Nota Máxima",
                                   TRUE ~ tipo_nota))
    df2 <- data3() %>% gather(key = "tipo_nota", value = "nota", nota1_media:nota2_max) %>% 
      filter(tipo_nota %in% c("nota2_media", "nota2_min", "nota2_max")) %>% 
      mutate(tipo_nota = case_when(tipo_nota == "nota2_media" ~ "Nota Média",
                                   tipo_nota == "nota2_min" ~ "Nota Mínima",
                                   tipo_nota == "nota2_max" ~ "Nota Máxima",
                                   TRUE ~ tipo_nota))
    
    g2 <- ggplot(df1, aes(x = inducao, y = nota, group = tipo_nota)) +
      geom_bar(stat = "identity", position = "dodge", width = .8, aes(fill = tipo_nota)) +
      geom_text(aes(label = paste0(round(nota, 2))),
                position = position_dodge(width = .9), hjust = -.3) +
      labs(x = "Indução", y = "Nota", title = "Nota sem Indução", fill = "") +
      coord_flip() +
      expand_limits(y = c(0, 110))
    
    g3 <- ggplot(df2, aes(x = inducao, y = nota, group = tipo_nota)) +
      geom_bar(stat = "identity", position = "dodge", width = .8, aes(fill = tipo_nota)) +
      geom_text(aes(label = paste0(round(nota, 2))),
                position = position_dodge(width = .9), hjust = -.3) +
      labs(x = "Indução", y = "Nota", title = "Nota Final (com Indução)", fill = "") +
      coord_flip() +
      expand_limits(y = c(0, max(df2$nota) + max(df2$nota)/6))
    
    g1/g2/g3
  })
  
  data1 <- reactive(
    pnab %>% filter(status == "Selecionada") %>%  
      filter(EDITAL_TIPO %in% input$tipo1) %>% 
      filter(EDITAL %in% input$edital1) %>% 
      filter(raca %in% input$raca1) %>% 
      filter(macrorreg %in% input$macrorreg1) %>% 
      group_by(vaga_utilizada) %>% summarise(selecionados = n(),
                                             nota1_media = mean(nota_pad_s_i),
                                             nota2_media = mean(nota_pad_final)) %>% 
      mutate(prop_selecionados = percent(selecionados/sum(selecionados), accuracy = 0.01))
  )
  
  plot1 <- reactive({
    g1 <- ggplot(data1(), aes(x = vaga_utilizada, y = selecionados)) +
      geom_bar(stat = "identity", width = .6, fill = "tomato3") +
      geom_text(aes(label = paste0(selecionados, " (", prop_selecionados, ")")),
                vjust = -.5) +
      labs(x = "Cota (Vaga Utilizada)", y = "Qtd. Selecionados", title = paste0("Selecionados", " - Total de ",
                                                                                sum(data1() %>% select(selecionados) %>% 
                                                                                      pull()))) +
      theme(axis.text = element_text(size = 12))
    
    g2 <- ggplot(data1(), aes(x = vaga_utilizada, y = nota1_media)) +
      geom_bar(stat = "identity", width = .6, fill = "goldenrod2") +
      geom_text(aes(label = paste0(round(nota1_media, 2))),
                vjust = -.5) +
      labs(x = "Cota (Vaga Utilizada)", y = "Nota Média", title = "Nota sem Indução")
    
    g3 <- ggplot(data1(), aes(x = vaga_utilizada, y = nota2_media)) +
      geom_bar(stat = "identity", width = .6, fill = "goldenrod2") +
      geom_text(aes(label = paste0(round(nota2_media, 2))),
                vjust = -.5) +
      labs(x = "Cota (Vaga Utilizada)", y = "Nota Média", title = "Nota Final (com Indução)") +
      theme(axis.text = element_text(size = 12))
    
    g1 + g2 + g3
    
  })
  
  output$plotcota1 <- renderPlot({
    plot1()
  })
  
  output$plotinducao3 <- renderPlot({
    plot3()
  })
  
  
  output$downloadplotecota1 <- downloadHandler(
    filename = function() {
      paste0("download", ".png")
    },
    content = function(file) {
      ggsave(plot = plotcota1(), filename = file, width = 12)
    }
  )
  
  observeEvent(input$tipo1, { 
    selected_tipo <- input$tipo1 
    updatePickerInput(session, "edital1", 
                      choices = df_edital %>% filter(EDITAL_TIPO %in% selected_tipo) %>% select(EDITAL) %>% pull() %>% sort(), 
                      selected = df_edital %>% filter(EDITAL_TIPO %in% selected_tipo) %>% select(EDITAL) %>% pull() %>% sort()) }) 
  
  
  
  
  output$tabelacota1 <-  DT::renderDT(server = FALSE, {
    DT::datatable(
      pnab %>% filter(status == "Selecionada") %>% filter(EDITAL_TIPO %in% input$tipo1) %>% 
        filter(EDITAL %in% input$edital1) %>% 
        filter(raca %in% input$raca1) %>% 
        filter(macrorreg %in% input$macrorreg1) %>%
        group_by(EDITAL_TIPO,
                 EDITAL,
                 raca,
                 macrorreg,
                 vaga_utilizada) %>% summarise(`Aproveitamento sem Indução` = round(mean(nota_pad_s_i), 2),
                                               `Aproveitamento com Indução` = round(mean(nota_pad_final), 2),
                                               `Nota sem Indução` = round(mean(nota_s_i), 2),
                                               `Nota com Indução` = round(mean(nota_final), 2),
                                               `Nota Máxima` = head(nota_maxima, 1),
                                               Selecionados = n()) %>% 
        group_by(EDITAL) %>% 
        mutate(`Selecionados (%)` = percent(Selecionados/sum(Selecionados), accuracy = 0.01)) %>% 
        rename(`Tipo de Edital` = 1,
               Edital = 2,
               `Grupo étnico-racial` = 3,
               Macrorregião = 4,
               Cota = 5),
      extensions = c("Buttons"),
      options = list(
        dom = 'Bfrtip',
        pageLength = 10,
        buttons = list(
          list(extend = "excel", text = "Baixar Planilha", filename = "data",
               exportOptions = list(
                 modifier = list(page = "all")
               )
          )
        )
      )
    )
  })
  
  data2 <- reactive({
    data21_1 <- pnab %>% filter(status_estudo == "Selecionada") %>% 
      filter(EDITAL_TIPO %in% input$tipo2) %>% 
      filter(EDITAL %in% input$edital2) %>% 
      filter(raca %in% input$raca2) %>% 
      filter(genero %in% input$genero2) %>% 
      filter(macrorreg %in% input$macrorreg2) %>% 
      filter(rd %in% input$rd2) %>% 
      group_by(raca) %>% summarise(selecionados = n()) %>% 
      mutate(prop = percent(selecionados/sum(selecionados), accuracy = 0.01))
    
    data21_2 <- pnab %>% filter(status == "Selecionada") %>% 
      filter(EDITAL_TIPO %in% input$tipo2) %>% 
      filter(EDITAL %in% input$edital2) %>% 
      filter(raca %in% input$raca2) %>% 
      filter(genero %in% input$genero2) %>% 
      filter(macrorreg %in% input$macrorreg2) %>% 
      filter(rd %in% input$rd2) %>% 
      group_by(raca) %>% summarise(selecionados = n()) %>% 
      mutate(prop = percent(selecionados/sum(selecionados), accuracy = 0.01))
    
    data22_1 <- pnab %>% filter(status_estudo == "Selecionada") %>% 
      filter(EDITAL_TIPO %in% input$tipo2) %>% 
      filter(EDITAL %in% input$edital2) %>% 
      filter(raca %in% input$raca2) %>% 
      filter(genero %in% input$genero2) %>% 
      filter(macrorreg %in% input$macrorreg2) %>% 
      filter(rd %in% input$rd2) %>% 
      group_by(genero) %>% summarise(selecionados = n()) %>% 
      mutate(prop = percent(selecionados/sum(selecionados), accuracy = 0.01))
    
    data22_2 <- pnab %>% filter(status == "Selecionada") %>% 
      filter(EDITAL_TIPO %in% input$tipo2) %>% 
      filter(EDITAL %in% input$edital2) %>% 
      filter(raca %in% input$raca2) %>% 
      filter(genero %in% input$genero2) %>% 
      filter(macrorreg %in% input$macrorreg2) %>% 
      filter(rd %in% input$rd2) %>% 
      group_by(genero) %>% summarise(selecionados = n()) %>% 
      mutate(prop = percent(selecionados/sum(selecionados), accuracy = 0.01))
    
    data23_1 <- pnab %>% filter(status_estudo == "Selecionada") %>% 
      filter(EDITAL_TIPO %in% input$tipo2) %>% 
      filter(EDITAL %in% input$edital2) %>% 
      filter(raca %in% input$raca2) %>% 
      filter(genero %in% input$genero2) %>% 
      filter(macrorreg %in% input$macrorreg2) %>% 
      filter(rd %in% input$rd2) %>% 
      group_by(macrorreg) %>% summarise(selecionados = n()) %>% 
      mutate(prop = percent(selecionados/sum(selecionados), accuracy = 0.01),
             INTERIOR = ifelse(macrorreg == "RMR", "RMR", "Interior do Estado"))
    
    data23_2 <- pnab %>% filter(status == "Selecionada") %>% 
      filter(EDITAL_TIPO %in% input$tipo2) %>% 
      filter(EDITAL %in% input$edital2) %>% 
      filter(raca %in% input$raca2) %>% 
      filter(genero %in% input$genero2) %>% 
      filter(macrorreg %in% input$macrorreg2) %>% 
      filter(rd %in% input$rd2) %>% 
      group_by(macrorreg) %>% summarise(selecionados = n()) %>% 
      mutate(prop = percent(selecionados/sum(selecionados), accuracy = 0.01),
             INTERIOR = ifelse(macrorreg == "RMR", "RMR", "Interior do Estado"))
    
    data23_X <- pnab %>% filter(status_estudo == "Selecionada") %>% 
      filter(EDITAL_TIPO %in% input$tipo2) %>% 
      filter(EDITAL %in% input$edital2) %>% 
      filter(raca %in% input$raca2) %>% 
      filter(genero %in% input$genero2) %>% 
      filter(macrorreg %in% input$macrorreg2) %>% 
      filter(rd %in% input$rd2) %>% 
      mutate(INTERIOR = ifelse(macrorreg == "RMR", "RMR", "Interior do Estado")) %>% 
      group_by(INTERIOR) %>% summarise(selecionados = n()) %>% 
      mutate(prop = percent(selecionados/sum(selecionados), accuracy = 0.01))
    
    data23_Y <- pnab %>% filter(status == "Selecionada") %>% 
      filter(EDITAL_TIPO %in% input$tipo2) %>% 
      filter(EDITAL %in% input$edital2) %>% 
      filter(raca %in% input$raca2) %>% 
      filter(genero %in% input$genero2) %>% 
      filter(macrorreg %in% input$macrorreg2) %>% 
      filter(rd %in% input$rd2) %>% 
      mutate(INTERIOR = ifelse(macrorreg == "RMR", "RMR", "Interior do Estado")) %>% 
      group_by(INTERIOR) %>% summarise(selecionados = n()) %>% 
      mutate(prop = percent(selecionados/sum(selecionados), accuracy = 0.01))
  
    
    data24_1 <- pnab %>% filter(status_estudo == "Selecionada") %>% 
      filter(EDITAL_TIPO %in% input$tipo2) %>% 
      filter(EDITAL %in% input$edital2) %>% 
      filter(raca %in% input$raca2) %>% 
      filter(genero %in% input$genero2) %>% 
      filter(macrorreg %in% input$macrorreg2) %>% 
      filter(rd %in% input$rd2) %>% 
      group_by(rd, macrorreg_new) %>% summarise(selecionados = n()) %>% 
      group_by() %>% 
      mutate(prop = percent(selecionados/sum(selecionados), accuracy = 0.01))
    
    data24_2 <- pnab %>% filter(status == "Selecionada") %>% 
      filter(EDITAL_TIPO %in% input$tipo2) %>% 
      filter(EDITAL %in% input$edital2) %>% 
      filter(raca %in% input$raca2) %>% 
      filter(genero %in% input$genero2) %>% 
      filter(macrorreg %in% input$macrorreg2) %>% 
      filter(rd %in% input$rd2) %>% 
      group_by(rd, macrorreg_new) %>% summarise(selecionados = n()) %>% 
      group_by() %>% 
      mutate(prop = percent(selecionados/sum(selecionados), accuracy = 0.01))
    
    list(data21_1 = data21_1, data21_2 = data21_2,
         data22_1 = data22_1, data22_2 = data22_2,
         data23_1 = data23_1, data23_2 = data23_2,
         data23_X = data23_X, data23_Y = data23_Y,
         data24_1 = data24_1, data24_2 = data24_2)
    
  })
  
  plot21 <- reactive({
    g1 <- ggplot(data2()$data21_1, aes(x = reorder(raca, selecionados), y = selecionados)) +
      geom_bar(stat = "identity", width = .6, fill = "tomato3") +
      geom_text(aes(label = paste0(selecionados, " (", prop, ")")), 
                size = 4, vjust = -.5) +
      labs(x = "Grupo Étnico-Racial", y = "Selecionados", title = "CONTRAFACTUAL") +
      expand_limits(y = c(0, max(data2()$data21_2$selecionados) + max(data2()$data21_2$selecionados)/5))
    
    g2 <- ggplot(data2()$data21_2, aes(x = reorder(raca, selecionados), y = selecionados)) +
      geom_bar(stat = "identity", width = .6, fill = "tomato3") +
      geom_text(aes(label = paste0(selecionados, " (", prop, ")")), 
                size = 4, vjust = -.5) +
      labs(x = "Grupo Étnico-Racial", y = "Selecionados", title = "FACTUAL") +
      expand_limits(y = c(0, max(data2()$data21_2$selecionados) + max(data2()$data21_2$selecionados)/5))
    
    g1 + g2
  })
  
  plot22 <- reactive({
    g1 <- ggplot(data2()$data22_1, aes(x = reorder(genero, selecionados), y = selecionados)) +
      geom_bar(stat = "identity", width = .6, fill = "goldenrod3") +
      coord_flip() +
      geom_text(aes(label = paste0(selecionados, " (", prop, ")")), 
                size = 4, hjust = -.1) +
      labs(x = "Gênero", y = "Selecionados", title = "CONTRAFACTUAL") +
      expand_limits(y = c(0, max(data2()$data22_2$selecionados) + max(data2()$data22_2$selecionados)/3))
    
    g2 <- ggplot(data2()$data22_2, aes(x = reorder(genero, selecionados), y = selecionados)) +
      geom_bar(stat = "identity", width = .6, fill = "goldenrod3") +
      coord_flip() +
      geom_text(aes(label = paste0(selecionados, " (", prop, ")")), 
                size = 4, hjust = -.1) +
      labs(x = "Gênero", y = "Selecionados", title = "FACTUAL") +
      expand_limits(y = c(0, max(data2()$data22_2$selecionados) + max(data2()$data22_2$selecionados)/3))
    
    g1 + g2
  })
  
  plot23 <- reactive({
    g1 <- ggplot(data2()$data23_1) +
      geom_bar(stat = "identity", width = .6, aes(x = INTERIOR, y = selecionados, fill = reorder(macrorreg, selecionados))) +
      geom_text(aes(x = INTERIOR, y = selecionados, fill = reorder(macrorreg, selecionados),
                    label = paste0(selecionados, " (", prop, ")")), 
                position = position_stack(vjust = 0.5), size = 5) +
      geom_text(aes(x = INTERIOR, y = selecionados,
                    label = paste0(selecionados, " (", prop, ")")), 
                size = 5, fontface = "bold", vjust = -.5,
                data = data2()$data23_X) +
      labs(x = "Região do Estado",
           y = "Selecionados",
           fill = "Macrorregião",
           title = "CONTRAFACTUAL") +
      scale_fill_manual(values = c("Agreste" = "#F35E23",
                                   "Zona da Mata" = "#90C842", 
                                   "Sertão" = "goldenrod3", 
                                   "RMR" = "lightslateblue")) +
      expand_limits(y = c(0, max(data2()$data23_Y$selecionados) + max(data2()$data23_Y$selecionados)/4))
    
    g2 <- ggplot(data2()$data23_2) +
      geom_bar(stat = "identity", width = .6, aes(x = INTERIOR, y = selecionados, fill = reorder(macrorreg, selecionados))) +
      geom_text(aes(x = INTERIOR, y = selecionados, fill = reorder(macrorreg, selecionados),
                    label = paste0(selecionados, " (", prop, ")")), 
                position = position_stack(vjust = 0.5), size = 5) +
      geom_text(aes(x = INTERIOR, y = selecionados,
                    label = paste0(selecionados, " (", prop, ")")), 
                size = 5, fontface = "bold", vjust = -.5,
                data = data2()$data23_Y) +
      labs(x = "Região do Estado",
           y = "Selecionados",
           fill = "Macrorregião",
           title = "FACTUAL") +
      scale_fill_manual(values = c("Agreste" = "#F35E23",
                                   "Zona da Mata" = "#90C842", 
                                   "Sertão" = "goldenrod3", 
                                   "RMR" = "lightslateblue")) +
      expand_limits(y = c(0, max(data2()$data23_Y$selecionados) + max(data2()$data23_Y$selecionados)/4))
    
    g1 + g2
  })
  
  plot24 <- reactive({
    g1 <- ggplot(data2()$data24_1, aes(x = reorder(rd, selecionados), y = selecionados)) +
      geom_segment(aes(xend = rd, y = 0, yend = selecionados), lty = 3) +
      geom_point(size = 4, aes(color = macrorreg_new)) +
      coord_flip() +
      geom_text(aes(label = paste0(selecionados, " (", prop, ")")), 
                size = 4, hjust = -.1) +
      expand_limits(y = c(0, max(data2()$data24_1$selecionados) + max(data2()$data24_1$selecionados)/3)) +
      scale_color_manual(values = c("Agreste" = "#F35E23",
                                    "Zona da Mata" = "#90C842", 
                                    "Sertão" = "goldenrod3", 
                                    "RMR" = "lightslateblue")) +
      labs(x = "Região de Desenvolvimento", y = "Selecionadas", color = "Macrorregião", title = "CONTRAFACTUAL")
    
    g2<- ggplot(data2()$data24_2, aes(x = reorder(rd, selecionados), y = selecionados)) +
      geom_segment(aes(xend = rd, y = 0, yend = selecionados), lty = 3) +
      geom_point(size = 4, aes(color = macrorreg_new)) +
      coord_flip() +
      geom_text(aes(label = paste0(selecionados, " (", prop, ")")), 
                size = 4, hjust = -.1) +
      expand_limits(y = c(0, max(data2()$data24_2$selecionados) + max(data2()$data24_2$selecionados)/3)) +
      scale_color_manual(values = c("Agreste" = "#F35E23",
                                    "Zona da Mata" = "#90C842", 
                                    "Sertão" = "goldenrod3", 
                                    "RMR" = "lightslateblue")) +
      labs(x = "Região de Desenvolvimento", y = "Selecionadas", color = "Macrorregião", title = "FACTUAL")
    
    g1 + g2
  })
  
  output$plotraca2 <- renderPlot({
    plot21()
  })
  
  output$plotgenero2 <- renderPlot({
    plot22()
  })
  
  output$plotreg2 <- renderPlot({
    plot23()
  })
  
  output$plotrd2 <- renderPlot({
    plot24()
  })
  
  
  tabela2_df <- reactive({
    d1 <- pnab %>% filter(status == "Selecionada") %>% 
      filter(EDITAL_TIPO %in% input$tipo1) %>% 
      filter(EDITAL %in% input$edital1) %>% 
      filter(raca %in% input$raca1) %>% 
      filter(macrorreg %in% input$macrorreg1) %>%
      group_by(EDITAL_TIPO,
               EDITAL,
               genero,
               raca,
               macrorreg) %>% summarise(`Selecionados Factual` = n()) %>% 
      mutate(`Selecionados Factual (%)` = percent(`Selecionados Factual`/sum(`Selecionados Factual`), accuracy = 0.01)) %>% 
      rename(`Tipo de Edital` = 1,
             Edital = 2,
             Gênero = 3,
             `Grupo Étnico-Racial` = 4,
             Macrorregião = 5)
    
    d2 <- pnab %>% filter(status_estudo == "Selecionada") %>% 
      filter(EDITAL_TIPO %in% input$tipo1) %>% 
      filter(EDITAL %in% input$edital1) %>% 
      filter(raca %in% input$raca1) %>% 
      filter(macrorreg %in% input$macrorreg1) %>%
      group_by(EDITAL_TIPO,
               EDITAL,
               genero,
               raca,
               macrorreg) %>% summarise(`Selecionados Contrafactual` = n()) %>% 
      mutate(`Selecionados Contrafactual (%)` = percent(`Selecionados Contrafactual`/sum(`Selecionados Contrafactual`), accuracy = 0.01)) %>% 
      rename(`Tipo de Edital` = 1,
             Edital = 2,
             Gênero = 3,
             `Grupo Étnico-Racial` = 4,
             Macrorregião = 5)
    
    d1 %>% left_join(d2, by = c("Tipo de Edital", "Edital", "Gênero", "Grupo Étnico-Racial", "Macrorregião"))
  })
  
  output$tabela2 <-  DT::renderDT(server = FALSE, {
    DT::datatable(
      tabela2_df(),
      extensions = c("Buttons"),
      options = list(
        dom = 'Bfrtip',
        pageLength = 10,
        buttons = list(
          list(extend = "excel", text = "Baixar Planilha", filename = "data",
               exportOptions = list(
                 modifier = list(page = "all")
               )
          )
        )
      )
    )
  })
  
  data3.1 <- reactive({
    d1 <- pnab %>%  
      filter(status == "Selecionada") %>% 
      filter(EDITAL_TIPO %in% input$tipo3) %>% 
      filter(EDITAL %in% input$edital3) %>% 
      filter(raca %in% input$raca3) %>% 
      filter(genero %in% input$genero3) %>% 
      filter(macrorreg %in% input$macrorreg3) %>% 
      filter(rd %in% input$rd3) %>% 
      group_by(EDITAL) %>% summarise(Aproveitamento_ind = round(mean(nota_pad_final),2),
                                     Aproveitamento_s_ind = round(mean(nota_pad_s_i),2))
    
    d2 <- pnab  %>%  
      filter(status == "Selecionada") %>% 
      filter(EDITAL_TIPO %in% input$tipo3) %>% 
      filter(EDITAL %in% input$edital3) %>% 
      filter(raca %in% input$raca3) %>% 
      filter(genero %in% input$genero3) %>% 
      filter(macrorreg %in% input$macrorreg3) %>% 
      filter(rd %in% input$rd3) %>%  
      group_by(EDITAL_TIPO) %>% summarise(`Aproveitamento Médio sem Indução` = round(mean(nota_pad_s_i),2),
                                          `Aproveitamento Médio com Indução` = round(mean(nota_pad_final),2),
                                          `Aproveitamento Mínimo sem Indução` = round(min(nota_pad_s_i),2),
                                          `Aproveitamento Mínimo com Indução` = round(min(nota_pad_final),2),
                                          `Aproveitamento Máximo sem Indução` = round(max(nota_pad_s_i),2),
                                          `Aproveitamento Máximo com Indução` = round(max(nota_pad_final),2)) %>% 
      gather(key = tipo_nota, value = nota, `Aproveitamento Médio sem Indução`:`Aproveitamento Máximo com Indução`)
    
    list(d1 = d1, d2 = d2)
  })
  
  plot3.1 <- reactive({
    g1 <- ggplot(data3.1()$d1, aes(x = reorder(EDITAL, Aproveitamento_ind), y = Aproveitamento_ind)) +
      geom_bar(stat = "identity", width = .6, fill = "goldenrod3") +
      coord_flip() +
      geom_text(aes(label = Aproveitamento_ind), size = 4, hjust = -.3) +
      expand_limits(y = c(0, max(data3.1()$d1$Aproveitamento_ind) + max(data3.1()$d1$Aproveitamento_ind)/4)) +
      labs(x = "Edital", y = "Aproveitamento com Indução", title = "COM INDUÇÃO")
    
    g2 <- ggplot(data3.1()$d1, aes(x = reorder(EDITAL, Aproveitamento_ind), y = Aproveitamento_s_ind)) +
      geom_bar(stat = "identity", width = .6, fill = "tomato3") +
      coord_flip() +
      geom_text(aes(label = Aproveitamento_s_ind), size = 4, hjust = -.3) +
      expand_limits(y = c(0, max(data3.1()$d1$Aproveitamento_s_ind) + max(data3.1()$d1$Aproveitamento_s_ind)/4)) +
      labs(x = "Edital", y = "Aproveitamento sem Indução", title = "SEM INDUÇÃO")
    
    g2 + g1
  })
  
  plot3.2 <- reactive({
    g1 <- ggplot(data3.1()$d2 %>% filter(str_detect(tipo_nota, "com Indução")), 
                 aes(x = reorder(EDITAL_TIPO, nota), y = nota, group = tipo_nota)) +
      geom_bar(stat = "identity", position = "dodge", width = .6, aes(fill = tipo_nota)) +
      geom_text(aes(label = paste0(round(nota, 2))),
                position = position_dodge(width = .6), hjust = -.3, size = 4) +
      coord_flip() +
      expand_limits(y = c(0, max(data3.1()$d2$nota) + max(data3.1()$d2$nota)/4)) +
      labs(x = "Edital", y = "Aproveitamento com Indução", title = "COM INDUÇÃO") +
      scale_fill_manual(values = c("Aproveitamento Médio com Indução" = "#00BA38",
                                   "Aproveitamento Mínimo com Indução" = "#619CFF",
                                   "Aproveitamento Máximo com Indução" = "#F8766D"),
                        labels = c("Aproveitamento Médio com Indução" = "Médio",
                                   "Aproveitamento Mínimo com Indução" = "Mínimo",
                                   "Aproveitamento Máximo com Indução" = "Máximo"))
    
    g2 <- ggplot(data3.1()$d2 %>% filter(str_detect(tipo_nota, "sem Indução")), 
                 aes(x = reorder(EDITAL_TIPO, nota), y = nota, group = tipo_nota)) +
      geom_bar(stat = "identity", position = "dodge", width = .6, aes(fill = tipo_nota)) +
      geom_text(aes(label = paste0(round(nota, 2))),
                position = position_dodge(width = .6), hjust = -.3, size = 4) +
      coord_flip() +
      expand_limits(y = c(0, max(data3.1()$d2$nota) + max(data3.1()$d2$nota)/4)) +
      labs(x = "Edital", y = "Aproveitamento sem Indução", title = "SEM INDUÇÃO", fill = "") +
      scale_fill_manual(values = c("Aproveitamento Médio sem Indução" = "#00BA38",
                                   "Aproveitamento Mínimo sem Indução" = "#619CFF",
                                   "Aproveitamento Máximo sem Indução" = "#F8766D"),
                        labels = c("Aproveitamento Médio sem Indução" = "Médio",
                                   "Aproveitamento Mínimo sem Indução" = "Mínimo",
                                   "Aproveitamento Máximo sem Indução" = "Máximo"))
    
    g2 + g1
  })
  
  output$plotinducaoedital3 <- renderPlot({
    plot3.1()
  })
  
  output$plotinducaotipo3 <- renderPlot({
    plot3.2()
  })
  
  
  data4 <- reactive({
    pnab <- pnab %>% filter(EDITAL != "Mulher Negra") %>% 
      filter(status == "Selecionada") %>% 
      filter(EDITAL %in% input$editalpnab) %>% 
      filter(raca %in% input$raca4) %>% 
      filter(genero %in% input$genero4) %>% 
      filter(macrorreg %in% input$macrorreg4) %>% 
      filter(rd %in% input$rd4) %>% 
      group_by(inducao) %>% summarise(Selecionados = n()) %>% 
      mutate(prop = percent(Selecionados/sum(Selecionados), accuracy = 0.01))
    
    lpg <- lpg %>% filter(status == "Selecionada") %>% 
      filter(EDITAL %in% input$editallpg) %>% 
      filter(raca %in% input$raca4) %>% 
      filter(genero %in% input$genero4) %>% 
      filter(macrorreg %in% input$macrorreg4) %>% 
      filter(rd %in% input$rd4) %>% 
      group_by(inducao) %>% summarise(Selecionados = n()) %>% 
      mutate(prop = percent(Selecionados/sum(Selecionados), accuracy = 0.01))
    
    list(pnab = pnab, lpg = lpg)
  })
  
  plot4 <- reactive({
    g1 <- ggplot(data4()$lpg, aes(x = reorder(inducao, Selecionados), y = Selecionados)) +
      geom_bar(stat = "identity", width = .6, fill = "goldenrod3") +
      coord_flip() +
      geom_text(aes(label = paste0(Selecionados, " (", prop, ")")), size = 4, hjust = -.3) +
      labs(title = "LPG", x = "Indução LPG") +
      expand_limits(y = c(0, max(data4()$lpg$Selecionados) + max(data4()$lpg$Selecionados)/3))
    
    g2 <- ggplot(data4()$pnab, aes(x = reorder(inducao, Selecionados), y = Selecionados)) +
      geom_bar(stat = "identity", width = .6, fill = "tomato3") +
      coord_flip() +
      geom_text(aes(label = paste0(Selecionados, " (", prop, ")")), size = 4, hjust = -.3) +
      labs(title = "PNAB", x = "Indução PNAB") +
      expand_limits(y = c(0, max(data4()$pnab$Selecionados) + max(data4()$pnab$Selecionados)/3))
    
    g1 + g2
  })
  
  output$plotlpgpnab4 <- renderPlot({
    plot4()
  })
  
  datatabela4 <- reactive({
    d1 <- lpg %>% filter(status == "Selecionada") %>% select(EDITAL, inducao, raca, genero) %>% 
      mutate(LEI = "LPG")
    
    d2 <- pnab %>% filter(status == "Selecionada") %>% select(EDITAL, inducao, raca, genero) %>% 
      mutate(LEI = "PNAB")
    
    df <- rbind(d1, d2)
    
    df %>% group_by(LEI, EDITAL, inducao, raca, genero) %>% summarise(Selecionados = n()) %>% 
      group_by(LEI, EDITAL) %>% 
      mutate(`Selecionados (%)` = percent(Selecionados/sum(Selecionados), accuracy = 0.01)) %>% 
      arrange(desc(LEI), EDITAL, -Selecionados)
  })
  
  output$tabela4 <-  DT::renderDT(server = FALSE, {
    DT::datatable(
      datatabela4(),
      extensions = c("Buttons"),
      options = list(
        dom = 'Bfrtip',
        pageLength = 10,
        buttons = list(
          list(extend = "excel", text = "Baixar Planilha", filename = "data",
               exportOptions = list(
                 modifier = list(page = "all")
               )
          )
        )
      )
    )
  })
  
  
}


shinyApp(ui, server)

#library(rsconnect)

# rsconnect::setAccountInfo(name='obicrestrito',
#                           token='F98219BA602AD1A5A23858D7515EE796',
#                           secret='XowWBHm+7mYXQpJd5Uhn1uJeyU6R4sbaxHdPJ6LD')
# 
# deployApp()
