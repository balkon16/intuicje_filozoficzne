library(shiny)
library(shinyjs)
library(plyr)
library(stringr)
library(dplyr)
library(plotly)
library(Rmisc)
library(gtools)
library(readr)
library(tidyr)

# obsługa pliku .csv ze słownikiem
translationContent <- read.delim("dictionary.csv", header = TRUE, sep = "\t", as.is = TRUE) 
translation <- dlply(translationContent ,.(key), function(s) key = as.list(s))
save(translation, file = "translation.bin")


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
                     title1 = c("odp_poz_Bartek", 
                                'odp_poz_Zuza',
                                'odp_desk_oszust',
                                'odp_poz_Jan_wie', #Odpowiedź pozytywna (Jan wie)
                                'odp_poz_obowiazek', 
                                'odp_poz_umy', 
                                'poz_swiat_rzecz', 
                                'Frank_nie_Furt', #Frank mógł nie zabić Furta
                                'Frank_odp_Furt',
                                'Frank_winny_Furt',
                                'Parfit',
                                'XYZ_woda')
)


server <- function(input, output) {
  
  tr <- function(text){ # funkcja na podstawie klucza (argument text) wybiera odpowiedni element listy
    sapply(text,function(s) translation[[s]][[input$language]], USE.NAMES=FALSE)
  }
  
  ### elementy, które zmieniają się w zależności od wybranego języka i muszą zostać przekazane do UI ###
  output$choose_scenario <- renderText({ 
    tr("choose_scenario")
  })
  
  output$fraction <- renderText({
    tr("fraction")
  })
  
  output$average <- renderText({
    tr("average")
  })
  
  output$choose_language <- renderText({
    tr("choose_language")
  })
  
  output$hide_show_desc_conc <- renderText({
    tr("hide_show_desc_conc")
  })
  
  ### koniec 
  
  shinyjs::onclick("toggleDescConc",
                   shinyjs::toggle(id = "opis_wnioski", anim = TRUE)) 

  
  output$scenariusz_html = renderUI(HTML(read_file(paste0(paste(c('scenarios_', input$language, "/"), collapse = ""), tabelka[tabelka$scenariusz == input$scenariusz,]$html))))
  output$wnioski_html = renderUI(HTML(read_file(paste0(paste(c('analyses_', input$language, "/"), collapse = ""), tabelka[tabelka$scenariusz == input$scenariusz,]$html))))
  
  output$intuicje_plot<- renderPlotly({
    zmienna = as.character(tabelka[tabelka$scenariusz == input$scenariusz,]$zmienna)
    title1 = tr(as.character(tabelka[tabelka$scenariusz == input$scenariusz,]$title1))
    s1p = prop.table(table(phil[[paste0(zmienna, '.s1')]]))[2]
    s2p = prop.table(table(phil[[paste0(zmienna, '.s2')]]))[2]
    
    s1c = prop.table(table(contr[[paste0(zmienna, '.s1')]]))[2]
    s2c = prop.table(table(contr[[paste0(zmienna, '.s2')]]))[2]
    s = c(tr('phil'), tr('ctrl'))
    data = data.frame(Grupa = s, S1 = c(s1p, s1c), S2 = c(s2p, s2c), 
                      S1text = round(c(s1p, s1c)*100, 1),
                      S2text = round(c(s2p, s2c)*100, 1))
    p = plot_ly(data,
                x = ~Grupa,
                y = ~S1,
                type = 'bar',
                name = tr('Semestr 1'),
                text = ~S1text,
                textposition = 'auto',
                textfont = list(size = 25),
                color = I('#bcbddc')) %>%
      add_trace(y = ~S2,
                name = tr('Semestr 2'),
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
  
  rysuj_diagram_przeplywu <- function(zmienna, ramka_grupa){
    
    nazwa_grupy_str <- deparse(substitute(ramka_grupa))
    print(nazwa_grupy_str)
    
    # użyte w celu zapewnienia kompatybilności ze słownikiem, gdzie mam klucz 'ctrl'
    if (nazwa_grupy_str == "contr"){
      nazwa_grupy_str = "ctrl"
    }
    
    # numer najświeższego semestru; uwaga jest on poza scope funkcji, w której się znajdujemy
    # liczba_semestrow <- 
    #   ls(all.names = TRUE) %>% str_match("s\\d+") %>% na.omit() %>% gsub(pattern = "[A-Za-z]+", replacement = "") %>% as.numeric() %>% max()
    # print_nazwa(liczba_semestrow)
    # Z tego powodu wpisuję na razie semestr z palca
    liczba_semestrow <- 2
    
    # kolumny, które dotyczą wskazanego scenariusza: odpowiedź i wartość pewności dla każdego semestru (oraz numer identyfikacyjny)
    cols <- grep(zmienna, colnames(ramka_grupa), value = TRUE)
    cols <- c("ident", cols)
    
    ramka_grupa_wybrane <- ramka_grupa[, cols]
    
    # usuwam wiersze bez identyfikacji
    ramka_grupa_wybrane <- ramka_grupa_wybrane %>%
      dplyr::filter(!(ident == ""))
    
    ramka_grupa_wybrane <- ramka_grupa_wybrane %>% na.omit()
    
    # obsłużona sytuacja, że w pierwszym semestrze wcale nie udzielono jednej z możliwych odpowiedzi, ale udzielono ją w którymś następnym semestrze
    odpowiedzi <- c()
    for (i in 1:liczba_semestrow){
      if (length(levels(ramka_grupa_wybrane[[paste0(zmienna, ".s", i)]])) > length(odpowiedzi)){
        odpowiedzi <- levels(ramka_grupa_wybrane[[paste0(zmienna, ".s", i)]])
      }
    }
    print(odpowiedzi)
    
    # tworze napisy typu: S1_Tak, S1_Nie, S2_Tak itd.
    obiekty <- c()
    for (i in 1:liczba_semestrow){ 
      for (odp in odpowiedzi){
        obiekt <- paste0("S", i, "_", odp)
        obiekty <- c(obiekty, obiekt)
      }
    }
    print(obiekty)
    
    # tworzę listę wierzchołków (source - wychodzące, target - przychodzące). Pamiętam, że S1_Tak, S2_Nie idą każdy do dwóch: S2_Tak oraz S2_Nie
    source <- c()
    target <- c()
    for (i in 0:(length(obiekty)-length(odpowiedzi)-1)) {# -1, bo wierzchołki w source indeksowane są od 0
      source <- c(source, rep(i, length(odpowiedzi)))
      for (j in seq(0, length(odpowiedzi)-1)){
        target <- c(target, i + j + length(odpowiedzi) - (i %% length(odpowiedzi)))
      }
    }
    print(source)
    print(target)
    
    # obsługuje problem, że nie w każdym semestrze udzielono wszystkich możliwych odpowiedzi
    for (i in 1:liczba_semestrow){
      levels(ramka_grupa_wybrane[[paste0(zmienna, ".s", i)]]) <- odpowiedzi
    }
    
    # Liczenie wartości połączeń pomiędzy semestrami - przypadek uogólniony. 
    # W wierszach tabeli krzyżowej mam odpowiedzi z poprzedniego semestru (i), a w kolumnach następnego semestru (i+1). 
    # Macierz przechodzę wierszami od lewej do prawej
    values <- c()
    for (i in 1:(liczba_semestrow - 1)){
      tab <- table(ramka_grupa_wybrane[[paste0(zmienna, ".s", i)]], ramka_grupa_wybrane[[paste0(zmienna, ".s", i+1)]])
      # print(tab)
      values <- c(values, as.vector(t(tab))) # spłaszcza transponowaną macierz, czyli kopiuje kolejne wiersze (od lewej do prawej)
    }
    
    # Ramka ze zmianą pewności - przypadek ogólny. Poniższa tabela zawiera wszystkie możliwe pary zmian pewności. 
    # W odniesieniu do tabeli krzyżowej, trzy kolejne wiersze to jeden wiersz tabeli krzyżowej.
    belief_changes <- c()
    for (i in 1:(liczba_semestrow - 1)){
      belief_ramka <- ramka_grupa_wybrane %>%
        group_by(!!! syms(c(paste0(zmienna, ".s", i), paste0(zmienna, ".s", i+1)))) %>%
        dplyr::summarise(belief = sum(!! sym(paste0(zmienna, "...poziom.s", i))) - sum(!! sym(paste0(zmienna, "...poziom.s", i+1)))) %>% 
        ungroup() %>%
        complete(!!! syms(c(paste0(zmienna, ".s", i), paste0(zmienna, ".s", i+1)))) %>% 
        mutate(belief = if_else(is.na(belief), 0, belief)) # zamiana NA na 0
      belief_changes <- c(belief_changes, belief_ramka$belief)
    }
    
    # Aby wartości zmiany pewności wyświetlały się jako kolor należy zmienić je na wektor kolorów. 
    # Ujemne wartości będą oznaczone przez kolor czerwony, a dodatnie przez zielony:
    range_col <- colorRamp(c("red","green"))
    normalized_vector <- (belief_changes - min(belief_changes)) / diff(range(belief_changes)) # uwaga: 0 są zamieniane na coś innego niż 0 - czy chciałbym wycentrować wokół 0?
    cols <- range_col(normalized_vector)
    colours_min <- rgb(cols, maxColorValue=256)
    
    # Lista z danymi do wyrysowania diagramu przepływu
    links <- data.frame(source = source,
                        target = target,
                        value = values,
                        colour = colours_min)
    
    nodes <- data.frame(name = obiekty)
    
    phil_results <- list(nodes = nodes,
                         links = links)
    
    p <- plot_ly(
      type = "sankey",
      
      
      domain = list(
        x =  c(0,1),
        y =  c(0,1)
      ),
      orientation = "h",
      valueformat = ".0f",
      
      node = list(
        label = phil_results$nodes$name,
        color = terrain.colors(8),
        pad = 15,
        thickness = 50,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      
      link = list(
        source = phil_results$links$source,
        target = phil_results$links$target,
        value =  phil_results$links$value
        # , color = phil_results$links$colour   # colorRampPalette(c('dodgerblue', 'dodgerblue4'))(length(colour))[rank(colour)])
      ) 
    ) %>% 
      layout(
        title = tr(nazwa_grupy_str),
        font = list(
          size = 10
        ),
        xaxis = list(showgrid = F, zeroline = F),
        yaxis = list(showgrid = F, zeroline = F)
      )
    
    p
    
  }
  
  output$przeplyw_odpowiedzi_fil <- renderPlotly(
    rysuj_diagram_przeplywu(
      zmienna = tabelka[tabelka$scenariusz == input$scenariusz,]$zmienna,
      ramka_grupa = phil))
  
  output$przeplyw_odpowiedzi_contr <- renderPlotly(
    rysuj_diagram_przeplywu(
      zmienna = tabelka[tabelka$scenariusz == input$scenariusz,]$zmienna,
      ramka_grupa = contr))
  
  output$pewnosc_plot <- renderPlotly({
    zmienna = tabelka[tabelka$scenariusz == input$scenariusz,]$zmienna
    dataAllSummary <- summarySE(data_all, measurevar=paste0(zmienna, '...poziom'), groupvars=c("Grupa", "Semestr"), na.rm = TRUE)
    dataAllSummary$mean = dataAllSummary[[paste0(zmienna, '...poziom')]]
    ci = dataAllSummary$ci
    dataAllSummary$Semestr <- unname(sapply(dataAllSummary$Semestr, tr))
    p <- plot_ly(data = dataAllSummary[which(dataAllSummary$Grupa == 'Filozofowie'),],
                 x = ~Semestr,
                 y = ~mean,
                 type = 'bar',
                 name = tr('phil'),
                 error_y = ~list(array = ci,
                                 color = '#000000')) %>%
      add_trace(data = dataAllSummary[which(dataAllSummary$Grupa == 'Grupa kontrolna'),],
                name = tr('ctrl')) %>%
      layout(title = tr("belief_degree"),
             xaxis = list(title = ''),
             yaxis = list(title = tr('mean')),
             legend = list(orientation = 'h'),
             font = list(size=15),
             margin = list(t=60))
  })
  
  output$pewnosc_razy_odpowiedz_plot <- renderPlotly({
    zmienna = as.character(tabelka[tabelka$scenariusz == input$scenariusz,]$zmienna)
    title1 = as.character(tabelka[tabelka$scenariusz == input$scenariusz,]$title2)
    
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
                 name = tr('phil'),
                 error_y = ~list(array = ci,
                                 color = '#555555',
                                 thickness = 1)) %>%
      add_trace(data = dataAllSummary[which(dataAllSummary$Grupa == 'Grupa kontrolna'),],
                name = tr('ctrl')) %>%
      layout(title = title1,
             xaxis = list(title = ''),
             yaxis = list(title = '', range = c(-5,5)),
             legend = list(orientation = 'h'),
             font = list(size=15),
             margin = list(t=60))
  })
}

server