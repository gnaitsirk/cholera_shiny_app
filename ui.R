shinyUI(pageWithSidebar(
  headerPanel("SIR Model of Cholera Outbreak"),
  sidebarPanel(
    
    wellPanel(tags$b("Basics:"),
              sliderInput("beta.adj", "Rate of contact with with resevoir:", 
                          value=.5, min=10e-5, max=1, step=.01),
              sliderInput("gamma.adj", "1/Duration of cholera infection (days):", 
                          value=1/5, min=1/14, max=1/2.9, step=.001),
              sliderInput("omega.adj", "1/Cholera lifespan in water resevior (days):", 
                          value=1/3, min=1/41, max=1/3, step=.001),
              sliderInput("xi.adj", "Rate of water contamination by humans:", 
                          value=10, min=0.01, max=10, step=.1),
              sliderInput("asef.adg", "Concentration of cholera:", 
                          value=10e6, min=10e5, max=10e6, step=100000)),
    
    wellPanel(tags$b("Demographics"),
              sliderInput("N.adj", "Population size:", 
                          value=16900, min=1000, max=50000, step=5000),
              sliderInput("mu.b.adj", "Birth rate:",
                          value=(110/input$N.adj), min=0, max=300/input$N.adj, step=input$mu.b.adj/10),
              sliderInput("mu.d.adj", "Death rate:",
                          value=(275-input$mu.c.adj/input$N.adj), min=input$mu.c.adj/input$N.adj, max=500/input$N.adj, step=input$mu.d.adj/10),
              sliderInput("mu.c.adj", "Cholera death rate:",
                          value=(224/input$N.adj), min=0, max=400/inputN.adj, step=input$mu.c.adj/10),
              sliderInput("times.adj", "Duration of epidemic (days):",
                          value=364, min=140, max=500, step=7)
    )
  ),
  mainPanel(plotOutput("guessPlot")
  )
))





