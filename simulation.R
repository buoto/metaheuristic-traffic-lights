library(stats)
library(magrittr)
library(fdrtool)

roadVector <- function(N = 0, S = 0, W = 0 , E = 0) c(N = N, S = S,  W = W, E = E)

simulate <- function(parameters, distsN, distsS, distsW, distsE, distEscape) {
  if (parameters %>% length %>% mod(24) > 0) stop('parameters number must be dividable by 24')
  
  dayPhaseSteps <- length(parameters) / 24 * c(4, 5, 4, 11)
  waitingCars <- roadVector()
  waitTime <- 0
  step <- 0
  phaseEnd <- 0
  currentMoves <- roadVector()
  
  for (phase in 1:length(dayPhaseSteps)) {
    phaseEnd <- phaseEnd + dayPhaseSteps[[phase]]
    print(c("phase", phase))
    
    while (step < phaseEnd) {
      # Update waiting time with cars left
      waitTime <- waitTime + sum(waitingCars)
      
      # Simulate arriving cars
      waitingCars <- waitingCars + c(rpois(1, distsN[[phase]]), rpois(1, distsS[[phase]]), rpois(1, distsW[[phase]]), rpois(1, distsE[[phase]]))
      print(c("step", step, waitingCars))
      
      cycles <- list(c(1, 2), c(3, 4))
      cycleEnd <- floor(step) + parameters[floor(step) + 1]
      for (cc in cycles) {
        while (step < cycleEnd && any(waitingCars[cc] > 0)) {
          # Update current moves
          print(c("moves1", currentMoves))
          
          startingDirections <- waitingCars[cc] > 0 & currentMoves[cc] <= step
          currentMoves[cc][startingDirections] <- step + rhalfnorm(sum(startingDirections), distEscape$theta)
          
          waitTime <- (waitTime + (step %% 1)) * sum(startingDirections)
          
          step <- min(currentMoves[currentMoves > step])
          direction <- currentMoves == step
          
          # Handle current car's move
          print(c("moves1", currentMoves))
          waitingCars[direction] <- waitingCars[direction] - 1
          print(c("left1", waitingCars))
        }
        
        if (step < cycleEnd) {
          step <- cycleEnd
          
        } else if (any(currentMoves > step)) {
          # Let last car leave the crossroad
          direction <- currentMoves > step
          print(c("moves2", currentMoves))
          
          step <- currentMoves[direction]
          
          waitingCars[direction] <- waitingCars[direction] - 1
          print(c("left2", waitingCars))
        }
        
        cycleEnd <- floor(step + 1)
      }
    }
  }
  
  waitTime
}

simulate(rep(1/2, 24), rep(1, 4), rep(0, 4), rep(0, 4), rep(1, 4), list(theta = 5))

