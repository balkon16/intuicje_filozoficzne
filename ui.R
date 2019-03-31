library(plotly)

ui <- fixedPage(
  tags$head(
    tags$style(HTML(".scenariusz {font-size: 16px;} .wnioski {font-size: 16px;}"))),

  shinyjs::useShinyjs(),
  
  fluidRow(
    column(10,
           selectInput("scenariusz",
                       textOutput("choose_scenario"),
                       c('Gettier', 
                         'Goldman',
                         'Kripke',
                         'Lehrer',
                         'Knobe',
                         'Nozick',
                         'Frankfurt I',
                         'Frankfurt II',
                         'Frankfurt III',
                         'Parfit',
                         'Putnam',
                         'Thomson'))
    ),
    column(2, radioButtons(inputId = "language", label = textOutput("choose_language"),
                           choices = c("English" = "en", "Polski" = "pl"),
                           selected = "en"))),
  
  a(id = "toggleDescConc", textOutput("hide_show_desc_conc"), href = "#"),
  
  div(id='opis_wnioski',
      fluidRow(
        column(8, htmlOutput('scenariusz_html', class = 'scenariusz')),
        column(4, htmlOutput('wnioski_html'), class = 'wnioski'))
  ),
  
  fluidRow(
    column(6, 
           plotlyOutput("przeplyw_odpowiedzi_fil", width ='100%', height = "400px")),
    column(6, 
           plotlyOutput("przeplyw_odpowiedzi_contr", width ='100%', height = "400px"))
  ),

  fluidRow(
    column(12,
           tabsetPanel(type = 'tabs',
                       tabPanel(textOutput("fraction"), plotlyOutput("intuicje_plot", width = "100%", height = "330px")),
                       tabPanel(textOutput("average"), plotlyOutput("pewnosc_razy_odpowiedz_plot", width = "100%", height = "300px"))
            )
    )),
  
  fluidRow(
    column(4,
           plotlyOutput("pewnosc_plot", width ='100%', height = "300px")
    )
  )

)


ui