# -----------------------------------------------------------------
# LIBRERIES AND PRELIMINARIES
# -----------------------------------------------------------------
setwd("~/Desktop/EESM/M2/S4_Drinkable-water-treatment/Project")

library(epanetReader)
library(epanet2toolkit)
library(dplyr)


# --------------------------------------------------------------------------
# SCHEME OF THE ALGORTIHM
# --------------------------------------------------------------------------
# We need to get the min pressure among ALL nodes for each time step (TS)
  # and the maximum velocity among ALL pipes for each time step
  # afterwards, we get the max/min out of ALL time steps
  # to obtain the value corresponded to a given demand

# We have to repeat the above process for each demand value to test
  # and identify the minimum demand that does not fulfill the constrain

# To do so, a biyeccion-type method will be employed


# --------------------------------------------------------------------------
# SETTING UP THE VARIABLES NEEDED
# --------------------------------------------------------------------------
# Loading the base file and the initial results
inp_file <- "Subject#4_Venissieux_cl-48.inp"
rtp_file <- "Subject#4_Venissieux_cl-48.rpt"
network <- read.inp(inp_file)
###network.res <- read.rpt(rtp_file)

# Chlorine dose to be tested (mg/L)
dose <- seq(from = 15, to = 20, by = 1)
###dose <- seq(from = 1.7, to = 1.8, by = 0.01)
###dose <- seq(from = 1.73, to = 1.74, by = 0.001)
###dose <- seq(from = 1.733, to = 1.734, by = 0.0001)
###dose <- seq(from = 1.7334, to = 1.7335, by = 0.00001)

# Other parameters for the iteration loops
N <- length(dose)                           #Num of demands to test
numNodes <- length(network$Junctions$ID)    #Num of nodes (pressure condition)
# We will run the first 24 hours as transient simulations and take the last
  # 24h as an steady simulation
numTS <- 48*60/5                            #Total num of time steps

# Here we will store the results for each demand
global.results <- tibble("Dose" = rep(NA, length(dose)), 
                  "Min conc" = rep(NA, length(dose)))

# Here we will store the results for each time step
TS.results <- tibble("Conc" = rep(NA, length(numTS)))


# --------------------------------------------------------------------------
# RUNNING THE ALGORITHM
# --------------------------------------------------------------------------
ENopen(inp_file, rtp_file)
ENopenH()
ENsolveH()
ENopenQ()

MyTank1 <- "Entree_500165252"              #First tank suppling the network
MyTank2 <- "Entree_500166280"              #Second tank suppling the network

# Initialize the quality on all the nodes to the minimum value
for (i in 1:numNodes){  
  ENsetnodevalue(i, 'EN_INITQUAL', 0)
}


i <- 1
#for(i in 1:N){                            #For each demand value
  ENsetnodevalue(ENgetnodeindex(MyTank1), 'EN_SOURCEQUAL', dose[i])
  ENsetnodevalue(ENgetnodeindex(MyTank2), 'EN_SOURCEQUAL', dose[i])
  
  global.results[i,1] <- dose[i]
  
  ENinitQ(0)                              #Initialize the simulation at TS = 0
  for (j in 1:numTS){                     #For each time step
    ENrunQ()                              #Simulate for ONE time step
    tmp.conc <- c()
    # For a given time step and value of demand
      # store the pressure values for all the nodes
    for (k in 1:numNodes){
      if (!(ENgetnodevalue(k, 'EN_BASEDEMAND') == 0)) {
        tmp.conc <- c(tmp.conc, ENgetnodevalue(k, 'EN_QUALITY'))
      }
    }
    # Get the min concentration out of all nodes for a given Ts
    ###plot(tmp.conc)
    TS.results[j,1] <- quantile(tmp.conc, 0.1)
    ENnextQ()                             #Move to the next TS and repeat
    ENnextH()
    print(paste("processed TS = ", j, sep = ""))
  }
  plot(TS.results$Conc, ylim=c(0,0.2))
  # Once all the TS are completed (simulated 24h)
    # get the min pressure among the minimums of all TS
  global.results[i,2] <- min(TS.results$Conc)
  print(paste("Processed dose: d =", dose[i]))
#}

ENcloseH()                                #Close the simulating tool
ENcloseQ()
ENclose()