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
inp_file <- "Subject#4_Venissieux.inp"
rtp_file <- "Subject#4_Venissieux.rtp"
network <- read.inp("Subject#4_Venissieux.inp")
###network.res <- read.rpt("Subject#4_Venissieux.rpt")

# Defining the demand's values to be tested
  # seeking for the fulfillment of both conditions
demand <- seq(from = 1, to = 2, by = 0.1)
###demand <- seq(from = 1.7, to = 1.8, by = 0.01)
###demand <- seq(from = 1.73, to = 1.74, by = 0.001)
###demand <- seq(from = 1.733, to = 1.734, by = 0.0001)
###demand <- seq(from = 1.7334, to = 1.7335, by = 0.00001)
###demand <- seq(from = 1.73343, to = 1.73344, by = 0.000001)

# Defining the demand's values to be tested
  # seeking for the fulfillment of minimum pressure condition
###demand <- seq(from = 1, to = 10, by = 1)
###demand <- seq(from = 3, to = 4, by = 0.1)
###demand <- seq(from = 3.8, to = 3.9, by = 0.01)
###demand <- seq(from = 3.81, to = 3.82, by = 0.001)
###demand <- seq(from = 3.811, to = 3.812, by = 0.0001)
###demand <- seq(from = 3.8113, to = 3.8114, by = 0.00001)

# Other parameters for the iteration loops
N <- length(demand)                         #Num of demands to test
numNodes <- length(network$Junctions$ID)    #Num of nodes (pressure condition)
numLinks <- length(network$Pipes$ID)        #Num of pipes (vel condition)
numTS <- 24*60/5                            #Total num of time steps

# Here we will store the results for each demand
global.results <- tibble("Demand" = rep(NA, length(demand)), 
                  "Min pressure" = rep(NA, length(demand)),
                  "Max vel" = rep(NA, length(demand)))

# Here we will store the results for each time step
TS.results <- tibble("Pressure" = rep(NA, length(numTS)),
                  "Vel" = rep(NA, length(numTS)))


# --------------------------------------------------------------------------
# RUNNING THE ALGORITHM
# --------------------------------------------------------------------------
ENopen(inp_file, rtp_file)
ENopenH()

MyNode <- "470028955"                     #Node whose demand is tested
ENgetnodevalue(ENgetnodeindex(MyNode), 'EN_BASEDEMAND')

for(i in 1:N){                            #For each demand value
  global.results[i,1] <- demand[i]
  ENsetnodevalue(ENgetnodeindex(MyNode), 'EN_BASEDEMAND', demand[i])
  ENinitH(0)                              #Initialize the simulation at TS = 0
  for (j in 1:numTS){                     #For each time step
    ENrunH()                              #Simulate for ONE time step
    tmp.press <- c()
    # For a given time step and value of demand
      # store the pressure values for all the nodes
    for (k in 1:numNodes){                
      tmp.press <- c(tmp.press, ENgetnodevalue(k, 'EN_PRESSURE'))
    }
    # For a given time step and value of demand
      # store the velocity values for all the pipes
    tmp.vel <- c()
    for (k in 1:numLinks){
      tmp.vel <- c(tmp.vel, ENgetlinkvalue(k, 'EN_VELOCITY'))
    }
    # get the min pressure among all nodes for a given TS
    TS.results[j,1] <- min(tmp.press)
    # get the max velocity among all pipes for a given TS
    TS.results[j,2] <- max(tmp.vel)
    ENnextH()                             #Move to the next TS and repeat
  }
  # Once all the TS are completed (simulated 24h)
    # get the min pressure among the minimums of all TS
    # get the max velocity among the maximums of all TS
  global.results[i,2] <- min(TS.results$Pressure)
  global.results[i,3] <- max(TS.results$Vel)
  print(paste("Processed demand: d =", demand[i]))
}

ENcloseH()                                #Close the simulating tool
ENclose()
