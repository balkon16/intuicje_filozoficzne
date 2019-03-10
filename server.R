library(shiny)
library(stringr)
library(dplyr)
library(plotly)
library(Rmisc)
library(gtools)
library(readr)

# POWER UP THE BASS CANNON 
s1phil = read.csv('wyniki_s1_phil.csv')
s1contr = read.csv('wyniki_s1_contr.csv')
s2phil = read.csv('wyniki_s2_phil.csv')
s2contr = read.csv('wyniki_s2_contr.csv')

s1phil$ident = str_sub(s1phil$Naklejka, 3, -1) # str_sub to funkcja z pakietu `stringr`
s1contr$ident = str_sub(s1contr$Naklejka, 3, -1)
s2phil$ident = str_sub(s2phil$Naklejka, 3, -1)
s2contr$ident = str_sub(s2contr$Naklejka, 3, -1)

phil = merge(s2phil, s1phil, by = 'ident', suffixes = c('.s2', '.s1')) # left_join to funkcja z pakietu `dplyr`
contr = merge(s2contr, s1contr, by = 'ident', suffixes = c('.s2', '.s1')) # left_join to funkcja z pakietu `dplyr`

s1phil$Grupa = 'Filozofowie'
s1contr$Grupa = 'Grupa kontrolna'
s2phil$Grupa = 'Filozofowie'
s2contr$Grupa = 'Grupa kontrolna'

s1phil$Semestr = 'Semestr 1'
s1contr$Semestr = 'Semestr 1'
s2phil$Semestr = 'Semestr 2'
s2contr$Semestr = 'Semestr 2'

data_all = smartbind(s1phil, s1contr, s2phil, s2contr)
data_all_l = smartbind(s1phil, s1contr, s2phil, s2contr)


tabelka = data.frame(scenariusz = c('Gettier',
                                    'Goldman',
                                    'Kripke',
                                    'Lehrer',
                                    'Thomson',
                                    'Knobe',
                                    'Nozick',
                                    'Frankfurt I',
                                    'Frankfurt II',
                                    'Frankfurt III',
                                    'Parfit',
                                    'Putnam'),
                     html = c('gettier.html',
                              'barns.html',
                              'incompleteness_theorem.html',
                              'john-thermometer.html',
                              'violinist.html',
                              'name-day.html',
                              'experience_machine.html',
                              'frank_and_furt.html',
                              'frank_and_furt.html',
                              'frank_and_furt.html',
                              'teleportation.html',
                              'planet_b297a.html'),
                     zmienna_pytanie = c('Gettier',
                                         'Stodoły',
                                         'Kripke',
                                         'Truetemp',
                                         'Skrzypek',
                                         'Knobe',
                                         'Maszyna.przyjemności',
                                         'Frankfurt1',
                                         'Frankfurt2',
                                         'Frankfurt3',
                                         'Parfit',
                                         'Putnam'),
                     
                     title1 = c('Odpowiedź pozytywna (Bartek wie)',
                                'Odpowiedź pozytywna (Zuza wie)',
                                'Odpowiedź deskrypcjonistyczna (oszust)',
                                'Odpowiedź pozytywna (Jan wie)',
                                'Odpowiedź pozytywna (obowiązek)',
                                'Odpowiedź pozytywna (umyślnie)',
                                'Odpowiedź "pozostać w świecie rzeczywistym"',
                                'Frank mógł nie zabić Furta',
                                'Frank jest odpowiedzialny za śmierć Furta',
                                'Frank jest winny śmierci Furta',
                                'Parfit',
                                'Odpowiedź pozytywna (XYZ to woda)'),
                     title2 = c('Odpowiedź pozytywna (Bartek wie)',
                                'Odpowiedź pozytywna (Zuza wie)',
                                'Odpowiedź deskrypcjonistyczna (oszust)',
                                'Odpowiedź pozytywna (Jan wie)',
                                'Odpowiedź pozytywna (obowiązek)',
                                'Odpowiedź pozytywna (umyślnie)',
                                'Odpowiedź "pozostać w świecie rzeczywistym"',
                                'Frank mógł nie zabić Furta',
                                'Frank jest odpowiedzialny za śmierć Furta',
                                'Frank jest winny śmierci Furta',
                                'Parfit',
                                'Odpowiedź pozytywna (XYZ to woda)')
)


server <- function(input, output) {
  
  output$scenariusz_html = renderUI(HTML(read_file(paste0('scenarios/', tabelka[tabelka$scenariusz == input$scenariusz,]$html))))
  output$wnioski_html = renderUI(HTML(read_file(paste0('analyses/', tabelka[tabelka$scenariusz == input$scenariusz,]$html))))
  
  output$intuicje_plot<- renderPlotly({
    zmienna = as.character(tabelka[tabelka$scenariusz == input$scenariusz,]$zmienna)
    title1 = as.character(tabelka[tabelka$scenariusz == input$scenariusz,]$title1)
    s1p = prop.table(table(phil[[paste0(zmienna, '.s1')]]))[2]
    s2p = prop.table(table(phil[[paste0(zmienna, '.s2')]]))[2]
    
    s1c = prop.table(table(contr[[paste0(zmienna, '.s1')]]))[2]
    s2c = prop.table(table(contr[[paste0(zmienna, '.s2')]]))[2]
    s = c('Filozofowie', 'Grupa kontrolna')
    data = data.frame(Grupa = s, S1 = c(s1p, s1c), S2 = c(s2p, s2c), 
                      S1text = round(c(s1p, s1c)*100, 1),
                      S2text = round(c(s2p, s2c)*100, 1))
    p = plot_ly(data,
                x = ~Grupa,
                y = ~S1,
                type = 'bar',
                name = 'Semestr 1',
                text = ~S1text,
                textposition = 'auto',
                textfont = list(size = 25),
                color = I('#bcbddc')) %>%
      add_trace(y = ~S2,
                name = 'Semestr 2',
                text = ~S2text,
                textposition = 'auto',
                color = I('#756bb1')) %>%
      layout(title= title1, 
             yaxis = list(title = '', range = c(0,1)), 
             xaxis = list(title = ''), 
             barmode = 'group',
             legend = list(orientation = 'h'),
             font = list(size = 18),
             margin = list(t=60))
    p
  })
  
  
  output$intuicje_plot_zmiana<- renderPlotly({
    zmienna = tabelka[tabelka$scenariusz == input$scenariusz,]$zmienna
    tab = table(phil[[paste0(zmienna, '.s1')]], phil[[paste0(zmienna, '.s2')]], dnn = c('S1', 'S2')) # Tabela 
    tab = prop.table(tab)
    data1 = data.frame(tab)
    data1$change = paste(data1$S1, data1$S2, sep = '-')
    
    tab = table(contr[[paste0(zmienna, '.s1')]], contr[[paste0(zmienna, '.s2')]], dnn = c('S1', 'S2')) # Tabela 
    tab = prop.table(tab)
    data2 = data.frame(tab)
    data2$change = paste(data2$S1, data2$S2, sep = '-')
    
    data = inner_join(data1, data2, by = 'change')
    
    
    p = plot_ly(data,
                x = ~change,
                y = ~Freq.x,
                type = 'bar',
                name = 'Filozofowie') %>%
      add_trace(y = ~Freq.y,
                name = 'Grupa kontrolna') %>%
      layout(title = 'Zmiany w odpowiedziach',
             yaxis = list(title = ''),
             xaxis = list(title=''),
             barmode = 'group',
             margin = list(t = 60, b=ifelse(input$scenariusz != 'Nozick', 40, 120)),
             legend = list(orientation= ifelse(input$scenariusz != 'Nozick', 'h', 'v')),
             font = list(size = 15)) # Bo Nozick się rozjeżdża
    p
  })
  
  output$pewnosc_plot <- renderPlotly({
    zmienna = tabelka[tabelka$scenariusz == input$scenariusz,]$zmienna
    dataAllSummary <- summarySE(data_all, measurevar=paste0(zmienna, '...poziom'), groupvars=c("Grupa", "Semestr"), na.rm = TRUE)
    dataAllSummary$mean = dataAllSummary[[paste0(zmienna, '...poziom')]]
    ci = dataAllSummary$ci
    p <- plot_ly(data = dataAllSummary[which(dataAllSummary$Grupa == 'Filozofowie'),],
                 x = ~Semestr,
                 y = ~mean,
                 type = 'bar',
                 name = 'Filozofowie',
                 error_y = ~list(array = ci,
                                 color = '#000000')) %>%
      add_trace(data = dataAllSummary[which(dataAllSummary$Grupa == 'Grupa kontrolna'),],
                name = 'Grupa kontrolna') %>%
      layout(title = 'Stopień przekonania',
             xaxis = list(title = ''),
             legend = list(orientation = 'h'),
             font = list(size=15),
             margin = list(t=60))
  })
  
  
  
  output$pewnosc_razy_odpowiedz_plot <- renderPlotly({
    zmienna = as.character(tabelka[tabelka$scenariusz == input$scenariusz,]$zmienna)
    title2 = as.character(tabelka[tabelka$scenariusz == input$scenariusz,]$title2)
    
    data_all_l = smartbind(s1phil, s1contr, s2phil, s2contr)
    data_all_l = data_all_l[!is.na(data_all_l[[paste0(zmienna, '...poziom')]]), ]
    data_all_l = data_all_l[!is.na(data_all_l[[zmienna]]), ]
    data_all_l[as.character(data_all_l[[zmienna]]) == 'Nie',
               paste0(zmienna, '...poziomX')] = data_all_l[as.character(data_all_l[[zmienna]]) == 'Nie', paste0(zmienna, '...poziom')] * -1 
    
    data_all_l[data_all_l[[zmienna]] == 'Tak',
               paste0(zmienna, '...poziomX')] = data_all_l[data_all_l[[zmienna]] == 'Tak', paste0(zmienna, '...poziom')] 
    
    
    dataAllSummary <- summarySE(data_all_l, measurevar=paste0(zmienna, '...poziomX'), groupvars=c("Grupa", "Semestr"), na.rm = TRUE)
    dataAllSummary$mean = dataAllSummary[[paste0(zmienna, '...poziomX')]]
    dataAllSummary$meanText = as.character(round(dataAllSummary$mean, 3))
    ci = dataAllSummary$ci
    print(dataAllSummary)
    p <- plot_ly(data = dataAllSummary[which(dataAllSummary$Grupa == 'Filozofowie'),],
                 x = ~Semestr,
                 y = ~mean,
                 type = 'bar',
                 text = ~meanText,
                 textposition = 'auto',
                 textfont = list(size = 25),
                 name = 'Filozofowie',
                 error_y = ~list(array = ci,
                                 color = '#555555',
                                 thickness = 1)) %>%
      add_trace(data = dataAllSummary[which(dataAllSummary$Grupa == 'Grupa kontrolna'),],
                name = 'Grupa kontrolna') %>%
      layout(title = title2,
             xaxis = list(title = ''),
             yaxis = list(title = '', range = c(-5,5)),
             legend = list(orientation = 'h'),
             font = list(size=15),
             margin = list(t=60))
  })
}

server