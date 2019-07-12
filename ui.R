library(shiny)

# Signifikanztest - Hinfuehrung
shinyUI(
  
  fluidPage(
  
  # Application title
  titlePanel("Von der Werteverteilung zum Signifikanztest"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar 
    sidebarPanel(
      # submitButton("Aenderungen anwenden"),
      # hr(),
      h4("Theoretische Werte"),
      sliderInput("mw",
                      "Mittelwert",
                      min = 50,
                      max = 150,
                      value = 100,
                      step= 1),
      sliderInput("std",
                      "Standardabweichung",
                      min = 0,
                      max = 30,
                      value = 15,
                      step= 1),
      h4("Anzeige"),
      checkboxInput("theoret", "Theoretischer Mittelwert"),
      checkboxInput("schaetz", "Stichproben-Werte"),
      checkboxInput("schaetzki", "Konfidenzintervall", value = TRUE),
      numericInput("nummer", "Stichprobe Nr.", 1, min=1, max = "nstpr"),
      h4("Simulation"),
      numericInput("npers", "Beobachtungen je Stichprobe (max. 1000)", 20, min=1, max=1000),
      numericInput("nstpr", "Anzahl der Stichproben (max. 2000)", 500, min=0, max=2000, step = 50),
      h4("Flaechen"), 
      sliderInput("alpha",
                  "alpha-Niveau",
                  min = 0,
                  max = 0.2,
                  value = 0.05,
                  step=0.01), 
      sliderInput("mw2",
                  "Mittelwert einer H1-Verteilung",
                  min = 50,
                  max = 150,
                  value = 110,
                  step= 1),
      checkboxInput("H0", "H0-Flaechen anzeigen", value = TRUE),
      checkboxInput("H1", "H1-Flaechen anzeigen"),
      checkboxInput("H1mw", "H1-Mittelwert anzeigen"),
      width=3
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Eine Stichprobe", plotOutput("einzelPlot")), 
                  tabPanel("Viele Stichproben", plotOutput("wertePlot")), 
                  tabPanel("Mittelwerte", plotOutput("mittelPlot")),
                  tabPanel("Konfidenzintervall", plotOutput("kiPlot")),
                  tabPanel("p-Wert", plotOutput("pPlot")),
                  tabPanel("Power", plotOutput("Plot2")),
                  tabPanel("About",
                           br(),
                           strong("App"),
                           br(),
                           p("Diese App wurde mit R und ", a("Shiny", href="http://www.rstudio.com/shiny/", target="_blank"), " entwickelt.", br(),
                           "Der Code für die App kann bei ",  a('GitHub', href='https://github.com/SonjaHahn/SignApp', target="_blank"),
                             "eingesehen und heruntergeladen werden.", br(),
                             "Die App kann mit den folgenden Befehlen lokal auf einem Rechner in R ausgeführt werden:"),
                           code('library(shiny)'),br(),
                           code('runGitHub("SignApp","SonjaHahn")'),


                           br(),      br(),
                           strong('Autor'),
                           p("Sonja Hahn", br(),
                             a("sonja.hahn@ph-karlsruhe.de", href="mailto:sonja.hahn@ph-karlsruhe.de"), br(),
                             a("Pädagogische Hochschule Karlsruhe", href="https://www.ph-karlsruhe.de")), br(),
                           
                           a(img(src="https://i.creativecommons.org/l/by/4.0/88x31.png"), target="_blank", href="http://creativecommons.org/licenses/by/4.0/"))
 
        )) #end tabset panel
      ) # end main panel
    ) # end sidebar layout
  ) # end fluid page



