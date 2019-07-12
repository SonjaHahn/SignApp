library(shiny)

# Berechnungen und Diagramm
shinyServer(function(input, output, session) {
  
  # Sicherstellen, dass Rechenaufwand nicht zu groß wird bei Eingabe der Stichprobengroesse und -anzahl ueber die Tastatur
  observeEvent(input$npers, if (input$npers>1000 | is.na(input$npers)) updateNumericInput(session, "npers", value=20))
  observeEvent(input$nstpr, if (input$nstpr>2000 | is.na(input$nstpr)) updateNumericInput(session, "nstpr", value=500))  
  # Datensaetze ####
  # Datensatz fuer die Graphiken 
  datasetInput <- reactive({
    matrix(rnorm(input$npers*input$nstpr, input$mw, input$std), ncol = input$nstpr)})
  # Datensatz unter H1
  datasetInput2 <- reactive({
    matrix(rnorm(input$npers*input$nstpr, input$mw2, input$std), ncol = input$nstpr)})

  
  # Ein Datensatz plotten ####
  output$einzelPlot <- renderPlot({
    
    # Histogramm
    hist(datasetInput()[,input$nummer], xlim=c(50,150), breaks = seq(-100,350, by = 5), 
         main = "Verteilung der Rohwerte einer Stichprobe",
         ylab = "Haeufigkeiten",
         xlab = "",
         col = "steelblue3")
    
    # Mittelwerte einzeichnen
    mwdaten <- mean(datasetInput()[,input$nummer])
    if (input$theoret==TRUE) abline(v=input$mw, col = "darkgrey", lwd=3)
    if (input$schaetz==TRUE) abline(v=mwdaten, col = "steelblue4", lwd=3)
    
    # Deskriptive Statistiken angeben
    if (input$schaetz==TRUE) { 
      axis(1, at = c(60, input$mw, 140), tick = FALSE, font = 2, line=1,
           labels = c("Rohwerte einer Stichprobe", 
                      paste("M =", round(c(mwdaten), digits=2)), 
                      paste("SD =", round(sd(datasetInput()[,input$nummer]), digits=2))),
           col.axis="steelblue4")}
    })

  # Alle Daten zusammen plotten ####
  output$wertePlot <- renderPlot({ 
    
    # Histogramm
    hist(datasetInput(),  xlim=c(50,150), breaks = seq(-100,350, by = 5),
         main = "Verteilung der Rohwerte aller Stichproben",
         ylab = "Haeufigkeiten",
         xlab = "", 
         border = "steelblue4")
    # Eine Stichprobe mit einzeichnen
    hist(datasetInput()[,input$nummer], xlim=c(50,150), breaks = seq(-100,350, by = 5), 
         main = "Verteilung der Rohwerte einer Stichprobe",
         ylab = "Haeufigkeiten",
         xlab = "",
         col = "steelblue3",
         add = TRUE) 
    
    # Zwischenberechnungen
    mwdaten <- mean(datasetInput()[,input$nummer])
    # Mittelwerte einzeichnen
    if (input$theoret==TRUE) abline(v=input$mw, col = "darkgrey", lwd=3)
    if (input$schaetz==TRUE) abline(v=mean(datasetInput()[,input$nummer]), col = "steelblue4", lwd=3)
    if (input$schaetz==TRUE) abline(v=mean(datasetInput()), col = "midnightblue", lwd=3)
    # Deskriptive Angaben
    if (input$schaetz==TRUE) { 

      axis(1, at = c(60, input$mw, 140), tick = FALSE, font = 2, line=1,
           labels = c("Rohwerte einer Stichprobe", 
                      paste("M =", round(c(mwdaten), digits=2)), 
                      paste("SD =", round(sd(datasetInput()[,input$nummer]), digits=2))),
           col.axis="steelblue4")
      
      axis(1, at = c(60, input$mw, 140), tick = FALSE, font = 2, line=2,
           labels = c("Rohwerte aller Stichproben", 
                      paste("M =", round(mean(datasetInput()), digits=2)),
                      paste("SD =", round(sd(datasetInput()), digits=2))), 
           col.axis="midnightblue")
    } 
    })
    
    # Mittelwerte plotten ####
    output$mittelPlot <- renderPlot({ 
      
      # Histogramm
      hist(colMeans(datasetInput()),  xlim=c(50,150), 
           main = "Verteilung der Mittelwerte aller Stichproben",
           ylab = "Haeufigkeiten",
           xlab = "", 
           border = "firebrick")
      
      # Mittelwerte und andere Linien
      segments(45,0,155) # x-Achsen-Ersatz
      if (input$theoret==TRUE) abline(v=input$mw, col = "darkgrey", lwd=3)
      if (input$schaetz==TRUE) abline(v=mean(datasetInput()[,input$nummer]), col = "steelblue4", lwd=3)
      if (input$schaetz==TRUE) abline(v=mean(datasetInput()), col = "firebrick", lwd=3)
    
      # Deskriptive Angaben
      if (input$schaetz==TRUE){
      axis(1, at = c(60, input$mw, 140), tick = FALSE, font = 2, line=1,
           labels = c("Rohwerte einer Stichprobe", 
                      paste("M =", round(mean(datasetInput()[,input$nummer]), digits=2)), 
                      paste("SD =", round(sd(datasetInput()[,input$nummer]), digits=2))),
          col.axis="steelblue4")
      axis(1, at = c(60, input$mw, 140), tick = FALSE, font = 2, line=3,
           labels = c("Mittelwerte aller Stichproben", 
                      paste("M =", round(mean(datasetInput()), digits=2)),
                      paste("SD = ", round(sd(colMeans(datasetInput())), digits=2))), 
           col.axis="firebrick")}

       })
    
    # Konfidenzintervall plotten ####
    output$kiPlot <- renderPlot({ 
      
      # Histogramm (als Grundlage fuer die Achsen etc.)
      hist(colMeans(datasetInput()),  xlim=c(50,150), 
           main = "Verteilung der Mittelwerte der Stichproben",
           ylab = "relative Haeufigkeiten",
           xlab = "", 
           border = "darksalmon",
           freq = FALSE, # relative Haeufigkeiten
           xaxt="n")
      
      # Zwischenberechnung
      mwdaten <- mean(datasetInput()[,input$nummer])
      
      
      # Grenzpunkte Vertrauensintervall
      punkte <- c(50, qnorm(input$alpha/2, mean = input$mw, sd=input$std/sqrt(input$npers)),
                  qnorm(input$alpha/2, mean = input$mw, sd=input$std/sqrt(input$npers), lower.tail=FALSE), 150)
      # Grenzpunkte Konfidenzintervall
      punkte2 <- c(50, qnorm(input$alpha/2, mean = mwdaten, sd=input$std/sqrt(input$npers)),
                  qnorm(input$alpha/2, mean = mwdaten, sd=input$std/sqrt(input$npers), lower.tail=FALSE), 150)
      
      # Flaeche unter der Kurve (Vertrauensintervall)
      if (input$H0 == TRUE){
      cord.x1 <- c(punkte[1], seq(punkte[1], punkte[2], length.out = 100), punkte[2]) 
      cord.y1 <- c(0, dnorm(seq(punkte[1], punkte[2], length.out = 100), mean = input$mw, sd = input$std/sqrt(input$npers)), 0) 
      polygon(cord.x1,cord.y1,col="lightcoral", border = NA)
      cord.x2 <- c(punkte[2], seq(punkte[2], punkte[3], length.out = 1000), punkte[3]) 
      cord.y2 <- c(0,dnorm(seq(punkte[2], punkte[3], length.out = 1000), mean = input$mw, sd = input$std/sqrt(input$npers)),0) 
      polygon(cord.x2,cord.y2,col="palegreen", border = NA)
      cord.x3 <- c(punkte[3], seq(punkte[3], punkte[4], length.out = 100), punkte[4]) 
      cord.y3 <- c(0,dnorm(seq(punkte[3], punkte[4], length.out = 100), mean = input$mw, sd = input$std/sqrt(input$npers)),0) 
      polygon(cord.x3,cord.y3,col="lightcoral", border = NA)
      }
      
      # darueber noch einmal das Histogramm
      hist(colMeans(datasetInput()),  xlim=c(50,150), 
           main = "Verteilung der Mittelwerte der Stichproben",
           ylab = "relative Haeufigkeiten",
           xlab = "", 
           border = "darksalmon",
           freq = FALSE,
           xaxt="n",
           add=TRUE)
      
      # Mittelwerte und Grenzwerte des KI einzeichnen
      segments(45,0,155) # x-Achsen-Ersatz
      if (input$theoret==TRUE) abline(v=input$mw, col = "darkgrey", lwd=3)
      if (input$schaetz==TRUE) abline(v=mean(datasetInput()[,input$nummer]), col = "steelblue4", lwd=3)
      if (input$schaetz==TRUE)  axis(1, at = c(60, mwdaten), tick = FALSE, font = 2, line=1,
                                     labels = c("einzelne Stichprobe", paste("M =", round(c(mwdaten), digits=2))), 
                                     col.axis="steelblue4")
      if (input$schaetzki==FALSE) axis(1)
      if (input$schaetzki==TRUE) axis(1,col="firebrick1", labels = FALSE, lwd = 1, col.ticks="white")
      if (input$schaetzki==TRUE)  axis(1, col = "limegreen", at = c(punkte2[2], punkte2[3]), lwd= 2, 
                                       tick = TRUE, col.ticks="black",
                                        labels = round(c(punkte2[2], punkte2[3]), digits=2))

      curve(dnorm(x, mean=input$mw, sd=input$std/sqrt(input$npers)), col = "darkgrey", lwd = 2, add = TRUE)
       
      })
    
# Plot fuer den p-Wert ####
    output$pPlot <- renderPlot({ 
      
      # Histogramm
      hist(colMeans(datasetInput()),  xlim=c(50,150), 
           main = "Verteilung der Mittelwerte der Stichproben",
           ylab = "relative Haeufigkeiten",
           xlab = "", 
           border = "darksalmon",
           freq = FALSE)
      
      # Zwischenrechnungen (Mittelwert und gespiegelter Mittelwert)
      mwdaten <- mean(datasetInput()[,input$nummer])
      mwdaten2 <- 2*input$mw - mwdaten

     # Punkte: Grenzen fuer p-Wert
       punkte <- sort(c(50, mwdaten, mwdaten2, 150))
       
      # Flaechen einzeichnen
      cord.x1 <- c(punkte[1], seq(punkte[1], punkte[2], length.out = 100), punkte[2]) 
      cord.y1 <- c(0, dnorm(seq(punkte[1], punkte[2], length.out = 100), mean = input$mw, sd = input$std/sqrt(input$npers)), 0) 
      polygon(cord.x1,cord.y1,col="steelblue", border = NA)

      cord.x3 <- c(punkte[3], seq(punkte[3], punkte[4], length.out = 100), punkte[4]) 
      cord.y3 <- c(0,dnorm(seq(punkte[3], punkte[4], length.out = 100), mean = input$mw, sd = input$std/sqrt(input$npers)),0) 
      polygon(cord.x3,cord.y3,col="steelblue", border = NA)
      
      # noch einmal das Histogramm
      hist(colMeans(datasetInput()),  xlim=c(50,150), 
           main = "Verteilung der Mittelwerte der Stichproben",
           ylab = "relative Haeufigkeiten",
           xlab = "", 
           border = "darksalmon",
           freq = FALSE,
           xaxt="n",
           add=TRUE)
      legend( "right", fill = "steelblue", bty = "n", legend = paste("p-Wert: ", round(2*pnorm(min(c(mwdaten,mwdaten2)), mean=input$mw, sd=input$std/sqrt(input$npers)), digits=3), sep = ""))
      
      # Diverse Linien
      if (input$theoret==TRUE) abline(v=input$mw, col = "darkgrey", lwd=3)
      if (input$schaetz==TRUE) abline(v=mwdaten, col = "steelblue4", lwd=3)
      if (input$schaetz==TRUE) abline(v=mwdaten2, col = "steelblue4", lwd=3, lty=3)
      if (input$schaetz==TRUE)  axis(1, at = c(60, mwdaten), tick = FALSE, font = 2, line=1,
                                     labels = c("einzelne Stichprobe", paste("M =", round(c(mwdaten), digits=2))), 
                                     col.axis="steelblue4")
        # Kurve darueber
      curve(dnorm(x, mean=input$mw, sd=input$std/sqrt(input$npers)), col = "darkgrey", lwd = 2, add = TRUE)
      
    })
  # Zwei Hypothesen ####
  output$Plot2 <- renderPlot({ 
    
    # Histogramm
    hist(colMeans(datasetInput()),  xlim=c(50,150), 
         main = "Verteilung der Mittelwerte der Stichproben unter H0 und H1",
         ylab = "relative Haeufigkeiten",
         xlab = "", 
         border = "white",
         freq = FALSE)
    
    # Zwischenrechnung
    mwdaten <- mean(datasetInput()[,input$nummer])

    # nochmal die Grenzpunkte vom Konfidenzintervall
    punkte <- c(50, qnorm(input$alpha/2, mean = input$mw, sd=input$std/sqrt(input$npers)),
                qnorm(input$alpha/2, mean = input$mw, sd=input$std/sqrt(input$npers), lower.tail=FALSE), 150)
    
     
     # Flaechen unter der H0-Verteilung
    if (input$H0 == TRUE){
    cord.x1 <- c(punkte[1], seq(punkte[1], punkte[2], length.out = 100), punkte[2]) 
    cord.y1 <- c(0, dnorm(seq(punkte[1], punkte[2], length.out = 100), mean = input$mw, sd = input$std/sqrt(input$npers)), 0) 
    polygon(cord.x1,cord.y1,col="lightcoral", border = NA)
    cord.x2 <- c(punkte[2], seq(punkte[2], punkte[3], length.out = 1000), punkte[3]) 
    cord.y2 <- c(0,dnorm(seq(punkte[2], punkte[3], length.out = 1000), mean = input$mw, sd = input$std/sqrt(input$npers)),0) 
    polygon(cord.x2,cord.y2,col="palegreen", border = NA)
    cord.x3 <- c(punkte[3], seq(punkte[3], punkte[4], length.out = 100), punkte[4]) 
    cord.y3 <- c(0,dnorm(seq(punkte[3], punkte[4], length.out = 100), mean = input$mw, sd = input$std/sqrt(input$npers)),0) 
    polygon(cord.x3,cord.y3,col="lightcoral", border = NA)}
    
    # Flaechen unter der H1 Verteilung
    if (input$H1 == TRUE){
      cord.x1 <- c(punkte[1], seq(punkte[1], punkte[2], length.out = 100), punkte[2]) 
      cord.y1 <- c(0, dnorm(seq(punkte[1], punkte[2], length.out = 100), mean = input$mw2, sd = input$std/sqrt(input$npers)), 0) 
      polygon(cord.x1, cord.y1, col="darkgreen", density = 20, border = NA)
      cord.x2 <- c(punkte[2], seq(punkte[2], punkte[3], length.out = 1000), punkte[3]) 
      cord.y2 <- c(0,dnorm(seq(punkte[2], punkte[3], length.out = 1000), mean = input$mw2, sd = input$std/sqrt(input$npers)),0) 
      polygon(cord.x2, cord.y2, col=c("firebrick"),angle=130, density = 20, border = NA)
      cord.x3 <- c(punkte[3], seq(punkte[3], punkte[4], length.out = 100), punkte[4]) 
      cord.y3 <- c(0,dnorm(seq(punkte[3], punkte[4], length.out = 100), mean = input$mw2, sd = input$std/sqrt(input$npers)),0) 
      polygon(cord.x3, cord.y3, col="darkgreen", density = 20, border = NA)}
    
    # Mittelwerte 
    if (input$theoret==TRUE) {abline(v=input$mw, col = "darkgrey", lwd=3)
      abline(v=input$mw2, col = "darkgrey", lty=2, lwd=3)}
    
    if (input$schaetz==TRUE & input$H1mw==FALSE)  abline(v=mean(datasetInput()[,input$nummer]), col = "steelblue4", lwd=3)

    if ( input$H1mw==TRUE) abline(v=mean(datasetInput2()[,input$nummer]), col = "steelblue4", lwd=3, lty=2)
    
    # Berechnung beta-Fehler
    beta <- pnorm(punkte[3], mean = input$mw2, sd = input$std/sqrt(input$npers)) - 
       pnorm(punkte[2], mean = input$mw2, sd = input$std/sqrt(input$npers))
    
    # Angabe WSK unter H0
    if (input$H0 == TRUE) legend( "topright", fill = c("lightcoral","lightgreen"), bty = "n", 
            title = "Wahrscheinlichkeiten gegeben die H0 gilt:",
                    legend = c(paste("alpha-Fehler:", input$alpha),
                       paste("Korrekte Entscheidung:", 1-input$alpha)))
    
    # Angabe WSK unter H1
    if (input$H1 == TRUE) legend( "right", fill = c("firebrick","darkgreen"), 
                                  density = 20, angle = c(135,45), bty = "n",
                                  title = "Wahrscheinlichkeiten gegeben die H1 gilt:",
            legend = c(paste("beta-Fehler:", round(beta, digits = 2)),
                       paste("Power:", round(1-beta, digits = 2))))
    # Grenzlinien vom KI
   abline(v=punkte[2],col="firebrick")
   abline(v=punkte[3],col="firebrick")
   # Kurven
    curve(dnorm(x, mean=input$mw, sd=input$std/sqrt(input$npers)), col = "darkgrey", lwd = 2, add = TRUE)
    curve(dnorm(x, mean=input$mw2, sd=input$std/sqrt(input$npers)),  lty = 2, col = "darkgrey", lwd = 2, add = TRUE)
  })
    

})
