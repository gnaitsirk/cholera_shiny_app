library(shiny); library(deSolve); library(ggplot2); library(quantmod)

SIRcholera <- function(t, x, parameters) {
  with(as.list(c(parameters, x)), {
    N=S+I+R
    dS = -((beta*B)/(B+alef))*S + mu.b*N - mu.d*S 
    dI = ((beta*B)/(B+alef))*S -gamma*I - (mu.c+mu.d)*I
    dR = gamma*I - mu.d*R
    dB = xi*I - omega*B
    dInc = ((beta*B)/(B+alef))*S
    result <- c(dS, dI, dR, dB, dInc)
    list(result)
  })
}

max.inf <- function(x) {
  infectious <- round(max(x$people[x$compartments=="I"]))
  timestep <- which.max(x$people[x$compartments=="I"])
  info <- cbind(infectious, timestep)
  return(info)
}

## Index of second highest infection value.
sec.index <- function(x) {
  index <- round(findPeaks(x$people[x$compartments=="I"], thresh=.1)[2])
  if (is.na(index)) {
    return(0)
  } else return(as.integer(index))
}

stacker <- function(df){
  df.stack <- stack(df[, -1])
  df.stack$time <- rep(seq_len(nrow(df)), length(table(df.stack$ind)))
  names(df.stack)[1:2] <- c("people", "compartments")
  df.stack$compartments <- factor(df.stack$compartments, 
                                  levels=c("S","I","R","Inc"), ordered=TRUE)
  return(df.stack)
}

shinyServer(function(input, output){
  output$guessPlot <- renderPlot(function(){
    I0=3
    S0=input$N.adj-I0
    B0=0.5
    R0=0
    Inc0=3
    times=0:times.adj
    dt <- seq(0, times.adj, by=1)
    state=c(S=S0,I=I0,R=R0,B=B0,Inc=Inc0)
    guess_parms=c(beta=input$beta.adj,# rate of contact with with resevoir
                  gamma=input$gamma.adj, # 1/gamma = duration of cholera infection
                  omega=input$omega.adj, # 1/omega = cholera life span in water 
                  xi=input$xi.adj, # rate of water contamination by humans
                  alef=input$alef.adj, # concentration of cholera that yields 50% chance of infection
                  mu.b=input$mu.b.adj, # birth rate 
                  mu.d=input$mu.d.adj, # death rate unrelated to cholera
                  mu.c=input$mu.c.adj # death rate related to cholera 
    )
    guess <- stacker(as.data.fram(ode(y=state,times=dt,func=SIRcholera,parms=guess_parms)))
    max.guess <- as.data.frame(max.inf(guess))
    max.guess$compartments <- "I"
    max.guess$adj.timestep <- max.guess$timestep / 100
    max.guess$maxtime <- max(guess$time)
    
    base.plot <- ggplot(guess, aes(x=time, y=people, 
                                   group=compartments, color=compartments)) + 
      xlab("Time") + ylab("Number of People")
    label.plot <- base.plot + geom_line(alpha=.8) + theme_bw() + 
      theme(legend.position="bottom") + theme(legend.title=element_blank())
  })
  
  
}
)

