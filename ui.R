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
      numericInput("npers", "Beobachtungen je Stichprobe", 20, min=1),
      numericInput("nstpr", "Anzahl der Stichproben", 500, min=1),
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
                  tabPanel("Kontakt", h1("Kontaktdaten"), "Bei Rueckfragen und Verbesserungsvorschlaege zu dieser Shiny-App kontaktieren Sie bitte sonja.hahn@ph-karlsruhe.de")))
#       
      ) # end main panel
    ) # end sidebar layout
  ) # end fluid page



