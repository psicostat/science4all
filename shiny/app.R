rm(list=ls())

library(shiny)
library(ggplot2)
# source("R/utils.R")

################### UI ###########################

# Define UI
ui <- navbarPage(
  title = "Il gioco dei pacchi",
  id = "pages",
  
  ################### UI Pg 1 ###########################
  
  tabPanel(
    title="Gli sfidanti!",
    br(),  # a capo
    h1("Il gioco",align="left"), # titoletto di primo livello
    br(),
    p("
    La moneta è truccata? È più forte il Cosenza o il Taranto? I Bucuringi sono più alti dei Titizi? Le nonne sono più vecchie dei dinosauri? La palla è più rotonda del quadrato?
    "),
    p("
    Rispondi a tutte queste domande utilizzando il minor numero possibile di indizi/informazioni. 
    Meno pescate farai, più punti otterrai...ma attenzione! 
    Se sbagli, darai tanti punti all'avversario quanti ne avresti fatti.
    Te la senti di rischiare?
      "),
    br(),  # a capo
    h2("I vostri nomi", align="left"), # titoletto di terzo livello
    br(),
    p("Quando siete pronti, cliccate qua!.
             "),
    sidebarPanel(
      textInput("player1", "Sfidante 1:", value = "Tizio"),
      textInput("player2", "Sfidante 2:", value = "Caio"),
      actionButton(inputId = "p1p2", label = "Iniziamo!")
    )
  ), #END PAGE 1
  
  ################### UI Pg 2 moneta ###########################
  
  #### PAGE 2 LA MONETA
  tabPanel(
    title = "Sfida 1",
    br(),  # a capo
    h1("La moneta", align = "left"), # titoletto di primo livello
    br(),
    # Sidebar panel with reduced width
    sidebarLayout(
      sidebarPanel(
        actionButton(inputId = "draw1.1", label = "Pesca, sfidante 1", 
                     style = "color: #fff; background-color: green; border-color: #2e6da4"),
        checkboxGroupInput(
          "risp1.1",
          "Cosa è più facile che esca?",
          choices = list("Testa" = 1, "Croce" = 2),
          selected = NULL
        ),
        br(),
        actionButton(inputId = "draw1.2", label = "Pesca, sfidante 2", 
                     style = "color: #fff; background-color: red; border-color: #2e6da4"),
        checkboxGroupInput(
          "risp1.2",
          "Cosa è più facile che esca?",
          choices = list("Testa" = 1, "Croce" = 2),
          selected = NULL
        ),
        br(),
        actionButton(inputId = "p2p3", label = "Continuiamo"),
        br(),
        width = 3  # Reducing the sidebar width
      ),
      # Main panel
      mainPanel(
        h4("Qualcuno ci ha detto che questa moneta è truccata: 
          dicono che una delle due facce esca col 60% di possibilità
          ...se scopriamo qual è, faremo un sacco di soldi!"),       
        br(),
        h3("Risultati"),
        # First row: Table and first plot side by side
        fluidRow(
          column(width = 4, tableOutput("table1")),  # Table on the left
          column(width = 8, plotOutput("plot1"))   # First plot on the right
        )
      )
    )
  ),  # END PAGE 2
  ################### UI Pg 3 calcio ###########################
  tabPanel(
    title = "Sfida 2",
    br(),  # a capo
    h1("La partita di pallone", align = "left"), # titoletto di primo livello
    br(),
    # Sidebar panel with reduced width
    sidebarLayout(
      sidebarPanel(
        actionButton(inputId = "draw2.2", label = "Pesca, sfidante 2", 
                     style = "color: #fff; background-color: red; border-color: #2e6da4"),
        checkboxGroupInput("risp2.2","Chi è più facile che vinca?",
                           choices = list("Cosenza" = 1, "Taranto" = 2),selected = NULL),
        br(),
        sliderInput("ndraw2.1","Quante volte vuoi giocare il derby?",min=0,max=100,value=0),
        actionButton(inputId = "draw2.1", label = "Lancia, sfidante 1",
                     style = "color: #fff; background-color: green; border-color: #2e6da4"),
        checkboxGroupInput("risp2.1","Qual è la squadra più forte?",
                           choices = list("Cosenza" = 1, "Taranto" = 2),selected = NULL),
        actionButton(inputId = "p3p4", label = "Continuiamo"),
        br(),
        width = 3  # Reducing the sidebar width
      ),
      # Main panel layout
      mainPanel(
        h4("Il Cosenza ha appena comprato Kuvumbolo, ma il Taranto ha risposto con l'acquisto di Suvatovic. 
           Due giocatori fortissimi! Quale sarà ora la squadra più forte?.
           L'unica cosa che sappiamo è che la squadra più forte, in media, segna poco più di un gol in più degli avversari in ogni partita.
           Quante volte dovremo ripetere l'incontro per scoprire chi sono i più forti?"),       
        br(),
        h3("Risultati"),
        # First row: Table and first plot side by side
        fluidRow(
          column(width = 4, tableOutput("table2")),  # Table on the left
          column(width = 8, tableOutput("plot2"))   # First plot on the right
        )
      )
    )
  ),  # END PAGE 3
  ################### UI Pg 4 Altezza ###########################
  tabPanel(
    title = "Sfida 3",
    br(),  # a capo
    h1("I Vatussi", align = "left"), # titoletto di primo livello
    br(),
    # Sidebar panel with reduced width
    sidebarLayout(
      sidebarPanel(
        actionButton(inputId = "draw3.1", label = "Confronta una coppia, sfidante 1", 
                     style = "color: #fff; background-color: green; border-color: #2e6da4"),
        checkboxGroupInput("risp3.1","Chi è più alto?",
                           choices = list("Bucuringi" = 1, "Titizi" = 2),selected = NULL),
        actionButton(inputId = "draw3.2", label = "Confronta una coppia, sfidante 2", 
                     style = "color: #fff; background-color: red; border-color: #2e6da4"),
        checkboxGroupInput("risp3.2","Chi è più alto?",
                           choices = list("Bucuringi" = 1, "Titizi" = 2),selected = NULL),
        br(),
        actionButton(inputId = "p4p5", label = "Continua"),
        width = 3  # Reducing the sidebar width
      ),
      # Main panel layout
      mainPanel(
        h4("Si narra che Bucuringi e Titizi siano due popolazioni di persone altissime!
           Si dice che entrambi superino i due metri di altezza, in media
           ...ma una delle due popolazioni è addirittura più alta dell'altra di ben 10 cm.
           Conoscerne uno però è impresa ardua. 
           Dovremo selezionarne il meno possibile per non perderci troppo tempo.
           Quanti ce ne servono per capire quale sia la popolazione più alta?"),       
        br(),
        h3("Risultati"),
        # First row: Table and first plot side by side
        fluidRow(
          column(width = 4, tableOutput("table3")),  # Table on the left
          column(width = 8, plotOutput("plot3"))   # First plot on the right
        )
      )
    )
  ),  # END PAGE 4
  
  ################### UI Pg 5 Sunto ###########################
  tabPanel(
    title = "Sunto",
    br(),  # a capo
    h1("Pronti per scoprire il vincitore?", align = "left"), # titoletto di primo livello
    br(),
    # Main panel layout
    mainPanel(
      textOutput("message"),
      br()
      )
  ), # END PAGE 5 
) #END UI


################### SERVER ###########################

# Define server
server <- function(input, output, session) {
  
  ################### SR Pg 1 players ###########################
  player1_name <- reactiveVal("player1")
  player2_name <- reactiveVal("player2")
  Punti1 <- reactiveVal(0)
  Punti2 <- reactiveVal(0)
  observeEvent(input$p1p2, {
    player1_name(input$player1)
    player2_name(input$player2)
    updateNavbarPage(session = session, inputId = "pages", selected = "Sfida 1")
  })
  
  ################### SR Pg 2 coin ###########################
  N = 20
  prob = .60 # Same for the two players
  # P1
  results1.1 <- reactiveVal()
  Ndraws1.1 <- reactiveVal(0)
  Positive1.1 <- reactiveVal(0)
  monete1.1 <- reactiveVal(rep("-", N))
  observeEvent(input$draw1.1, {
    result <- rbinom(1, 1, prob)  # Generate random result
    results1.1(result)           # Update reactive result
    Ndraws1.1(Ndraws1.1() + 1) # Increment the draw count
    Positive1.1(Positive1.1() + result) # Update positive results count
    # Update the vector of results
    current_monete <- monete1.1()
    current_monete[Ndraws1.1()] <- result
    monete1.1(current_monete)  # Update the reactive value of monete1.1
  })
  # P2
  results1.2 <- reactiveVal()
  Ndraws1.2 <- reactiveVal(0)
  Positive1.2 <- reactiveVal(0)
  monete1.2 <- reactiveVal(rep("-", N))
  randomx = rep(rnorm(100), 2)
  randomy = rep(rnorm(100), 2)
  observeEvent(input$draw1.2, {
    result <- rbinom(1, 1, prob)  # Generate random result
    results1.2(result)           # Update reactive result
    Ndraws1.2(Ndraws1.2() + 1) # Increment the draw count
    Positive1.2(Positive1.2() + result) # Update positive results count
    # Update the vector of results
    current_monete <- monete1.2()
    current_monete[Ndraws1.2()] <- result
    monete1.2(current_monete)  # Update the reactive value of monete1.2
  })
  # Tabella 1
  output$table1 <- renderTable({
    df1 <- data.frame(V0 = c("N pescate", "N teste", "% teste", "Punti"),
                      V1 = c(Ndraws1.1(), Positive1.1(),paste0(round((Positive1.1()/Ndraws1.1())*100), "%"), Punti1()),
                      V2 = c(Ndraws1.2(), Positive1.2(),paste0(round((Positive1.2()/Ndraws1.2())*100), "%"), Punti2()))
    colnames(df1) <- c("",input$player1, input$player2)
    df1
  })
  output$plot1 <- renderPlot({
    # Create a data frame
    dp2 <- data.frame(
      x = rep(1:N,2),
      y = as.factor(rep(1:5,8)),
      Testa = c(monete1.1(), monete1.2()),  # Ensure both are called as reactive values
      Sfidante = rep(c(input$player1, input$player2), each = 20)  # Input values are used within reactive
    )
    # Recode the "Testa" column to labels for heads/tails
    dp2$Testa <- ifelse(dp2$Testa == 1, "Testa", ifelse(dp2$Testa == 0, "Croce", "boh"))
    # Create the plot
    ggplot(dp2, aes(x = x, y = y, color = Testa)) + 
      geom_point(size = 4,shape = 21, stroke = 5, alpha = .7) +
      scale_color_manual(values = c(Testa = "green", "Croce" = "red", "boh" = "grey90"))+ 
      theme_minimal() +
      theme(text = element_text(size = 20),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())  +
      ylab("") + xlab("") +
      facet_wrap(~Sfidante)
  })
  # Score update
  observeEvent(input$risp1.1, {
    punteggio1.1 <- ifelse(input$risp1.1 == 1, 
                           N - Ndraws1.1(), 
                           (N - Ndraws1.1()) * (-1))
    Punti1(Punti1() + punteggio1.1)
  })
  observeEvent(input$risp1.2, {
    punteggio1.2 <- ifelse(input$risp1.2 == 1, 
                           N - Ndraws1.2(), 
                           (N - Ndraws1.2()) * (-1))
    Punti2(Punti2() + punteggio1.2)
  })
  observeEvent(input$p2p3, {
    updateNavbarPage(session = session, inputId = "pages", selected = "Sfida 2")
  })
  
  ################### SR Pg 3 calcio ###########################
  lambda_CS1 <- 2.3  # Expected goals for Team A (stronger)
  lambda_TA1 <- 1.2
  lambda_CS2 <- 1.2  # Expected goals for Team A (weaker for P2)
  lambda_TA2 <- 2.3
  # P1
  N = 20
  obs2.1 <- reactiveVal(0)
  results2.1CS <- reactiveVal(rep(NA,N))
  results2.1TA <- reactiveVal(rep(NA,N))
  storico2.1 <- reactiveVal(rep("-",N)) 
  observeEvent(input$draw2.1, {
    resCS1 <- rpois(1, lambda_CS1) 
    resTA1 <- rpois(1, lambda_TA1)
    obs2.1(obs2.1()+1)
    rs11 <- results2.1CS()
    rs12 <- results2.1TA()
    rs11[obs2.1()] <- resCS1
    rs12[obs2.1()] <- resTA1
    results2.1CS(rs11)
    results2.1TA(rs12)
    stori1 <- storico2.1()
    stori1[obs2.1()] <- paste0(resCS1,"-",resTA1)
    storico2.1(stori1)
  })
  # P2
  obs2.2 <- reactiveVal(0)
  results2.2CS <- reactiveVal(rep(NA,N))
  results2.2TA <- reactiveVal(rep(NA,N))
  storico2.2 <- reactiveVal(rep("-",N)) 
  observeEvent(input$draw2.2, {
    resCS2 <- rpois(1, lambda_CS2) 
    resTA2 <- rpois(1, lambda_TA2)
    obs2.2(obs2.2()+1)
    rs21 <- results2.2CS()
    rs22 <- results2.2TA()
    rs21[obs2.2()] <- resCS2
    rs22[obs2.2()] <- resTA2
    results2.2CS(rs21)
    results2.2TA(rs22)
    stori2 <- storico2.2()
    stori2[obs2.2()] <- paste0(resCS2,"  -  ",resTA2)
    storico2.2(stori2)
  })
  # Combine results into a data frame
  output$table2 <- renderTable({
    n <- c(obs2.1(), obs2.2())
    win <- c(sum(results2.1CS() > results2.1TA(), na.rm = T),
             sum(results2.2CS() > results2.2TA(), na.rm = T))
    loss <- c(sum(results2.1CS() < results2.1TA(), na.rm = T),
              sum(results2.2CS() < results2.2TA(), na.rm = T))
    draw <- c(sum(results2.1CS() == results2.1TA(), na.rm = T),
              sum(results2.2CS() == results2.2TA(), na.rm = T))
    diffG <- c(sum(results2.1CS() - results2.1TA(), na.rm = T),
               sum(results2.2CS() - results2.2TA(), na.rm = T))
    df2 <- data.frame(
      Sfidante = c(paste0(1,input$player1), paste0(2,input$player2)),
      Match = n,
      W_CS = win,
      W_TA = loss,
      Pari = draw,
      DifferenzaReti = diffG,
      Punti = c(Punti1(), Punti2())
      )
    df2
    df2.1 <- data.frame(Var = c("Partite", "Vittorie CS", "Sconfitte CS", "Pareggi CS", "Differenza reti CS", "Punti"),
                        V1 = c(n[1],win[1],loss[1],draw[1],diffG[1],Punti1()),
                        V2 = c(n[2],win[2],loss[2],draw[2],diffG[2],Punti2()))
    colnames(df2.1) <- c("",input$player1,input$player2)
    df2.1
  })
  # Plot 2
  output$plot2 <- renderTable({
    df2.2 <- data.frame(Giocatore1 <- c("CS-TA",storico2.1()),
                        Giocatore2 <- c("CS-TA",storico2.2()))
    colnames(df2.2) <- c(paste0(input$player1), paste0(input$player2))
    df2.2
  })
  # Score update
  observeEvent(input$risp2.1, {
    punteggio2.1 <- ifelse(input$risp2.1 == 1,
                           N - obs2.1(),
                           (N - obs2.1()) * (-1))
    Punti1(Punti1() + punteggio2.1)
  })
  observeEvent(input$risp2.2, {
    punteggio2.2 <- ifelse(input$risp2.2 == 2, # P2 correct is Taranto
                           N - obs2.2(),
                           (N - obs2.2()) * (-1))
    Punti2(Punti2() + punteggio2.2)
  })
  observeEvent(input$p3p4, {
    updateNavbarPage(session = session, inputId = "pages", selected = "Sfida 3")
  })
  ################### SR Pg 4 Bucuringi ###########################
  mBU <- 203  # Altezza media dei Bucuringi
  mTI <- 222  # Altezza media dei Titizi # Uguale per i due giocatori
  N = 20
  # P1
  obs3.1 <- reactiveVal(0)
  results3.1BU <- reactiveVal(rep(NA,N))
  results3.1TI <- reactiveVal(rep(NA,N))
  observeEvent(input$draw3.1, {
    resBU1 <- rnorm(1, mBU, 25) 
    resTI1 <- rnorm(1, mTI, 27) 
    obs3.1(obs3.1()+1)
    rsBU1 <- results3.1BU()
    rsTI1 <- results3.1TI()
    rsBU1[obs3.1()] <- resBU1
    rsTI1[obs3.1()] <- resTI1
    results3.1BU(rsBU1)
    results3.1TI(rsTI1)
  })
  # P2
  obs3.2 <- reactiveVal(0)
  results3.2BU <- reactiveVal(rep(NA,N))
  results3.2TI <- reactiveVal(rep(NA,N))
  observeEvent(input$draw3.2, {
    resBU2 <- rnorm(1, mBU, 25) 
    resTI2 <- rnorm(1, mTI, 27) 
    obs3.2(obs3.2()+1)
    rsBU2 <- results3.2BU()
    rsTI2 <- results3.2TI()
    rsBU2[obs3.2()] <- resBU2
    rsTI2[obs3.2()] <- resTI2
    results3.2BU(rsBU2)
    results3.2TI(rsTI2)
  })
  # Table with summary results
  output$table3 <- renderTable({
    df3 <- matrix(ncol = 3, nrow = 5)
    df3[,1] <- c("Coppie", "Media Bucuringi", "Media Titizi", "Differenza", "Punti")
    df3[,2] <- c(obs3.1(), round(mean(results3.1BU(),na.rm=T)),round(mean(results3.1TI(),na.rm=T)),
                 round(mean(results3.1BU() - results3.1TI(),na.rm=T)), Punti1())
    df3[,3] <- c(obs3.2(), round(mean(results3.2BU(),na.rm=T)),round(mean(results3.2TI(),na.rm=T)),
                 round(mean(results3.2BU() - results3.2TI(),na.rm=T)), Punti2())
    colnames(df3) <- c(" ",input$player1,input$player2)
    df3
  })
  # Plot of heights
  output$plot3 <- renderPlot({
    df3plot <- data.frame(Altezza = c(results3.1BU(),results3.1TI(),
                                results3.2BU(),results3.2TI()),
                          Popolo = c(rep(c("Bucuringi","Titizi"),each=N),
                                     rep(c("Bucuringi","Titizi"),each=N)),
                          Sfidante = rep(c(input$player1,input$player2),each=N*2),
                          Coppia = rep(1:N, times = 4))
    df3plot$Altezza <- ifelse(is.na(df3plot$Altezza) == T, 0, df3plot$Altezza)
    ggplot(df3plot, aes(x = Popolo, y = Altezza, fill = Popolo)) +
      geom_bar(stat = "identity") +
      theme(text = element_text(size = 20),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())  +
      theme_minimal() +
      coord_cartesian(ylim = c(0,300)) +
      facet_wrap(~ Sfidante+ Coppia, ncol = 10)
    # df3plot$Coppia = as.factor(df3plot$Coppia)
    # ggplot(df3plot, aes(x = Popolo, y = Altezza, color = Coppia, 
    #                     group = Coppia)) +
    #   geom_point(aes(shape=Popolo), size = 4) +
    #   geom_line(linetype = "dashed") +
    #   theme(text = element_text(size = 20),
    #         axis.text.x=element_blank(),
    #         axis.ticks.x=element_blank(),
    #         axis.text.y=element_blank(),
    #         axis.ticks.y=element_blank())  +
    #   theme_minimal() +
    #   coord_cartesian(ylim = c(150,300)) +
    #   facet_wrap(~ Sfidante)# + Coppia, ncol = 10)
  })
  # Score update
  observeEvent(input$risp3.1, {
    punteggio3.1 <- ifelse(input$risp3.1 == 2,
                           N - obs3.1(),
                           (N - obs3.1()) * (-1))
    Punti1(Punti1() + punteggio3.1)
  })
  observeEvent(input$risp3.2, {
    punteggio3.2 <- ifelse(input$risp3.2 == 2,
                           N - obs3.2(),
                           (N - obs3.2()) * (-1))
    Punti2(Punti2() + punteggio3.2)
  })
  observeEvent(input$p4p5, {
    updateNavbarPage(session = session, inputId = "pages", selected = "Sunto")
  })
  
  ################### SR Pg 5 sunto ###########################
  output$message <- renderText({
    
    # Calculate the difference in scores
    if (Punti1() > Punti2()) {
      diff <- Punti1() - Punti2()
      paste0(input$player2, " ha ", Punti2(), " punti. ", input$player1, " ha ", Punti1(), 
             " punti. ", input$player1, " con ", diff, 
             " punti in più vince la partita!")
    } else if (Punti2() > Punti1()) {
      diff <- Punti2() - Punti1()
      paste0(input$player1, " ha ", Punti1(), " punti. ", input$player2, " ha ", Punti2(), 
             " punti. ", input$player1, " con ", diff, 
             " punti in più vince la partita!")
    } else {
      paste0(input$player1, " e ", input$player2, " hanno entrambi ", Punti1(), " punti.")
    }
  })
  observeEvent(input$p5p6, {
    updateNavbarPage(session = session, inputId = "pages", selected = "Sfida 4")
  })
  
}

##############################################
##############################################

shinyApp(ui = ui, server = server)

##############################################
##############################################

# To deploy on sinyapp.io
# require('rsconnect')
# rsconnect::setAccountInfo(name='feracot',
#                          token='53991C3495678EB4C2EB512C8EB7E0A3',
#                          secret='54ngkn6hz0AsNnJiz63Qr2W4kQjNJMEs1mhzIfJr')
# # Crea cartella "shiny" sul desktop e copiaci dentro l'app chiamata "app"
# rsconnect::deployApp('C:/Users/feraco/OneDrive - Università degli Studi di Padova/TerzaMissione/NotteDellaRicerca/2024/Psicostat/Science4All')
