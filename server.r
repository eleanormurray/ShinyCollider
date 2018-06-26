##Simulation to demonstrate potential bias when estimating the Natural Direct Effect##
#Oct 11, 2017#

library(stats)
library(tidyr)
library(dplyr)
library(knitr)
library(shiny)
library(DT)
library(DiagrammeR)
library(magrittr)



######################################
##1. Generate simple dataset##########
#####exposure (A)#####################
#####mediator (M)#####################
#####outcome (Y)######################
#####common cause of M & Y (U)########
#Note: all variables binary###########
######################################
set.seed(1234)
paramNames<-c("pY1U0", "sampSize", "RRuy", "pA", "pU", 
              "pM1U0A0", "RRum", "RRam", "RRmy", "RRay", "AMint", "isAMint")

simulate_dat<-function(pY1U0 = 0.1, sampSize = 1000, RRuy = 2, pA = 0.2, pU = 0.5, 
                       pM1U0A0 = 0.4, RRum = 1.3, RRam = 1.6, RRmy=1.0,RRay=1.0, AMint = 0.5, outlist, isAMint= TRUE){
  
  #Inputs
  N.samp = sampSize
  pA.1 = pA
  pU.1 = pU
  b0.Y = log(pY1U0)
  b1.YA = log(RRay)
  b2.YU = log(RRuy)
  b3.YM = log(RRmy)
  b4.AMint = log(AMint)
  a0.M = log(pM1U0A0)
  a1.MA = log(RRam)
  a2.MU = log(RRum)
  isAM.int = isAMint
  
  #Simulation
  
  #table1 <-data.frame()
  
 table1 <- bind_rows(lapply(as.list(1:N.samp), FUN = function(i){
    #A and U are random#
    A <-rbinom(1,1, pA.1)
    U <-rbinom(1,1, pU.1)
    M <- rbinom(1,1,(0.675*(1/(1+exp(-1*(a0.M + a1.MA*A)))) + 0.325*(1/(1+exp(-1*(a2.MU*U)))))) 
    if (isAM.int==TRUE){
      Y <- rbinom(1,1,1/(1+exp(-1*(b0.Y + b1.YA*A + b2.YU*U + b3.YM*M + b4.AMint*A*M))))  
    }
    else{
      Y <- rbinom(1,1,1/(1+exp(-1*(b0.Y + b1.YA*A + b2.YU*U + b3.YM*M))) )
    }
    return(data.frame(A,U,M,Y))
  }))
  
  if (isAM.int==TRUE){
  #calculate association between A and Y
    logitY.AM <-glm(Y~A+M +A*M, data = table1, family = binomial)
    CDE_m0 <-exp(coef(logitY.AM)[2])
    CDE_m1 <-exp(coef(logitY.AM)[2]+coef(logitY.AM)[4])
    logitY.A <-glm(Y~A, data = table1, family = binomial)
    TE <-exp(coef(logitY.A)[2])

    #bias factor
    logitY.UMA <-glm(Y ~A+M+U+M*U, data = table1, family = binomial)
    gamma_m1<-exp(coef(logitY.UMA)[3]+coef(logitY.UMA)[4])
    pU.A1m1 <-xtabs(~U+A, data = table1, subset=(M==1))[2,2]/xtabs(~A, data=table1, subset=(M==1))[2]
    pU.A0m1 <-xtabs(~U+A, data = table1, subset=(M==1))[2,1]/xtabs(~A, data=table1, subset=(M==1))[1]
    bmult_CDEm1<-(1+((gamma_m1-1)*pU.A1m1))/(1+((gamma_m1 -1)*pU.A0m1))
    gamma_m0<-exp(coef(logitY.UMA)[3])
    pU.A1m0 <-xtabs(~U+A, data = table1, subset=(M==0))[2,2]/xtabs(~A, data=table1, subset=(M==0))[2]
    pU.A0m0 <-xtabs(~U+A, data = table1, subset=(M==0))[2,1]/xtabs(~A, data=table1, subset=(M==0))[1]
    bmult_CDEm0<-(1+((gamma_m0-1)*pU.A1m0))/(1+((gamma_m0 -1)*pU.A0m0))
    
    E_CDE <- 0.5*(CDE_m1 + CDE_m0) 
    E_bmult <-0.5*(bmult_CDEm0 + bmult_CDEm1)
    
  }
  else{
    #calculate association between A and Y
    logitY.AM <-glm(Y~A+M, data = table1, family = binomial)
    CDE_m0 <-exp(coef(logitY.AM)[2])
    logitY.A <-glm(Y~A, data = table1, family = binomial)
    TE <-exp(coef(logitY.A)[2])
    
    #bias factor
    logitY.UMA <-glm(Y ~A+M+U+M*U, data = table1, family = binomial)
    gamma_m1<-exp(coef(logitY.UMA)[3]+coef(logitY.UMA)[4])
    pU.A1m1 <-xtabs(~U+A, data = table1, subset=(M==1))[2,2]/xtabs(~A, data=table1, subset=(M==1))[2]
    pU.A0m1 <-xtabs(~U+A, data = table1, subset=(M==1))[2,1]/xtabs(~A, data=table1, subset=(M==1))[1]
    bmult_CDEm1<-(1+((gamma_m1-1)*pU.A1m1))/(1+((gamma_m1 -1)*pU.A0m1))
    gamma_m0<-exp(coef(logitY.UMA)[3])
    pU.A1m0 <-xtabs(~U+A, data = table1, subset=(M==0))[2,2]/xtabs(~A, data=table1, subset=(M==0))[2]
    pU.A0m0 <-xtabs(~U+A, data = table1, subset=(M==0))[2,1]/xtabs(~A, data=table1, subset=(M==0))[1]
    bmult_CDEm0<-(1+((gamma_m0-1)*pU.A1m0))/(1+((gamma_m0 -1)*pU.A0m0))
    
    CDE_m1 <-CDE_m0
    
    E_CDE <- 0.5*(CDE_m1 + CDE_m0) 
    E_bmult <-0.5*(bmult_CDEm0 + bmult_CDEm1)
  
  }
  
  #simpop_dists
  pYsim <-xtabs(~Y, data = table1)[2]/(xtabs(~Y, data = table1)[1]+xtabs(~Y, data = table1)[2])
  
  #output results table
  b1.YA
  #table2<-data.frame(c(pYsim, TE, CDE_m0, CDE_m1, E_CDE, bmult_CDEm0, bmult_CDEm1, E_bmult), 
   #                  row.names=c('P(Y=1)','Total Effect','CDE(m=0)','CDE(m=1)','Average Direct Effect', 'Bias factor, m=0', 'Bias factor, m=1' , 'Average Bias'))
  
  table2<-data.frame(c(pYsim, TE, CDE_m0, CDE_m1, E_CDE, RRay), 
                      row.names=c('P(Y=1)','Total Effect','CDE(m=0)','CDE(m=1)','Average Direct Effect','True Direct Effect'))
  
  colnames(table2)<-'Estimated Values'
  
  #Simulated DAG
  RR.ay1 <-CDE_m1
  RR.ay0 <-CDE_m0
  RR.ay_avg<-E_CDE
  RR.my <-exp(coef(logitY.AM)[3])
  logitM.A <-glm(M~A, data = table1, family=binomial)
  RR.am <-exp(coef(logitM.A)[2])
  #nodes
  nodes =create_node_df(7,  label = c('A','Y', 'M', 'U',  
                        paste('OR', eval(parse(text=paste(round(RR.am, digits = 2)))), sep=" = "),
                        paste('OR', eval(parse(text=paste(round(RR.my, digits = 2)))), sep=" = "),
                        paste('OR', eval(parse(text=paste(round(RR.ay_avg, digits = 2)))), sep=" = ")
                        ),
                        x = c(1, 2.5, 1.5, 1, 1,1.8, 2), y = c(0, -0.5,-0.5, -1,-0.25,-0.4,-0.15), 
                        color = c('none','none','black','none','none','none','none' ),
                        fillcolor = c('none','none','none','none','none','none','none'),
                        shape = c('square','square','square','square','square','square','square'),
                        width = c(0.2,0.2,0.2,0.2,0.2,0.18,0.2),
                        fontsize = c(8,8,8,8,6,6,6))
# 
#   #nodes
#   nodes =create_node_df(8,  label = c('A','Y', 'M', 'U',  
#                                       paste('OR', eval(parse(text=paste(round(RR.am, digits = 2)))), sep=" = "),
#                                       paste('OR', eval(parse(text=paste(round(RR.my, digits = 2)))), sep=" = "),
#                                       paste('CDE(m=1): OR', eval(parse(text=paste(round(RR.ay1, digits = 2)))), sep=" = "),
#                                       paste('CDE(m=0): OR', eval(parse(text=paste(round(RR.ay0, digits = 2)))), sep=" = ")
#   ),
#   x = c(1, 2.5, 1.5, 1, 1,1.8, 2, 2), y = c(0, -0.5,-0.5, -1,-0.25,-0.4,0,-0.15), 
#   color = c('none','none','black','none','none','none','none' ,'none' ),
#   fillcolor = c('none','none','none','none','none','none','none', 'none'),
#   shape = c('square','square','square','square','square','square','square','square'),
#   width = c(0.2,0.2,0.2,0.2,0.2,0.18,0.2,0.2),
#   fontsize = c(8,8,8,8,6,6,6,6))
  
  
  #create dag
  simDAG<-
    create_graph(nodes_df=nodes, directed = TRUE)
  #add edges
  simDAG<-
    add_edge(
      simDAG,
      from = 1,
      to = 2
    )%>%
    add_edge(
      from = 3,
      to = 2
    )%>%
    add_edge(
      from = 1,
      to = 3
    )%>%
    render_graph()

  #"true" DAG
  #nodes
  nodes2 =create_node_df(9,  label = c('A','Y', 'M', 'U',  
                                       paste('OR', eval(parse(text=paste(round(RRam, digits = 2)))), sep=" = "),
                                       paste('OR', eval(parse(text=paste(round(RRmy, digits = 2)))), sep=" = "),
                                       paste('OR', eval(parse(text=paste(round(RRuy, digits = 2)))), sep=" = "),
                                       paste('OR', eval(parse(text=paste(round(RRay, digits = 2)))), sep=" = "),
                                       paste('OR', eval(parse(text=paste(round(RRum, digits = 2)))), sep=" = ")
                                      ),
                        x = c(1, 2.5, 1.5, 1, 1,1.8, 1.8, 1.8,1), y = c(0, -0.5,-0.5, -1,-0.25,-0.4,-0.9, -0.13, -0.65), 
                        color = c('none','none','none','none','none','none','none' ,'none' ,'none'),
                        fillcolor = c('none','none','none','none','none','none','none', 'none','none'),
                        shape = c('square','square','square','square','square','square','square','square','square'),
                        width = c(0.2,0.2,0.2,0.2,0.2,0.18,0.2,0.2,0.2),
                        fontsize = c(8,8,8,8,6,6,6,6,6))
    
  #create dag
  trueDAG<-
    create_graph(nodes_df=nodes2, directed = TRUE)
  #add edges
  trueDAG<-
    add_edge(
      trueDAG,
      from = 1,
      to = 2
    )%>%
    add_edge(
      from = 3,
      to = 2
    )%>%
    add_edge(
      from = 4,
      to = 2
    )%>%
    add_edge(
      from = 4,
      to = 3
    )%>%
    add_edge(
      from = 1,
      to = 3
    )%>%
    render_graph()
  
  outlist <-list("table1" = table1, "table2"=table2, "trueDAG"=trueDAG , "simDAG"=simDAG)
  return(outlist)   
}



function(input, output, session) {
  
  getParams <- function(prefix) {
    input[[paste0(prefix, "_recalc")]]
    
    params <- lapply(paramNames, function(p) {
      input[[paste0(prefix, "_", p)]]
    })
    names(params) <- paramNames
    params
  }
  
    results_tab1 <-reactive(do.call(simulate_dat, getParams("a")))


    output$table1<-renderDataTable({
      results_tab1()$table1[,2:4]
    })    
    output$table2<-renderDataTable({
      round(results_tab1()$table2,2)
      })
    
    output$trueDAG<-renderGrViz({
      results_tab1()$trueDAG
    })
    
    output$simDAG<-renderGrViz({
      results_tab1()$simDAG
    })
}

