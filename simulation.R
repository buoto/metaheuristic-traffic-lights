library(stats)
library(magrittr)

roadVector <- function(N = 0, S = 0, W = 0 , E = 0) c(N = N, S = S,  W = W, E = E)

#' TODO handle day phases
simulate <- function(parameters, distsN, distsS, distsW, distsE, distEscape) {
  if (parameters %>% length %>% mod(24) > 0) stop('parameters number must be dividable by 24')
  
  dayPhaseSteps <- length(parameters) / 24 * c(4, 5, 4, 11)
  waitingCars <- roadVector()
  waitTime <- 0
  step <- 0
  phaseEnd <- 0
  currentTime <- 1 # TODO merge with step?
  currentMoves <- roadVector()
  
  for (phase in 1:length(dayPhaseSteps)) {
    phaseEnd <- phaseEnd + dayPhaseSteps[[phase]]
    print(c("phase", phase))
    
    while (step < phaseEnd) {
      currentTime <- currentTime - 1
      
      # Update waiting time with cars left
      waitTime <- waitTime + sum(waitingCars)
      
      # Simulate arriving cars
      # TODO day phases vvvv
      waitingCars[1] <- waitingCars[1] + rpois(1, distsN)
      waitingCars[2] <- waitingCars[2] + rpois(1, distsS)
      waitingCars[3] <- waitingCars[3] + rpois(1, distsW)
      waitingCars[4] <- waitingCars[4] + rpois(1, distsE)
      # TODO day phases ^^^^
      print(c("step", step, waitingCars))
      
      cycles <- list(c(1, 2), c(3, 4))
      cycleEnd <- parameters[step + 1]
      for (cc in cycles) {
        while (currentTime < cycleEnd && any(waitingCars[cc] > 0)) {
          # Update current moves
          print(c("moves1", currentMoves))
          
          msk <- waitingCars[cc] > 0 & currentMoves[cc] <= 0
          print(c("mask", msk))
          currentMoves[cc][msk] <- rnorm(sum(msk), distEscape$mean, distEscape$sd)
          
          move <- min(currentMoves[currentMoves > 0])
          direction <- currentMoves == move
          
          # Handle current car's move
          print(c("moves1", currentMoves))
          currentTime <- currentTime + move
          currentMoves <- currentMoves - move
          waitingCars[direction] <- waitingCars[direction] - 1
          print(c("left1", waitingCars))
        }
        
        if (currentTime < cycleEnd) {
          currentTime <- cycleEnd
          
        } else if (any(currentMoves > 0)) {
          
          # Let last car leave the crossroad
          direction <- currentMoves > 0
          print(c("moves2", currentMoves))
          move <- currentMoves[direction]
          currentTime <- currentTime + move
          currentMoves <- currentMoves - move
          waitingCars[direction] <- waitingCars[direction] - 1
          print(c("left2", waitingCars))
        }
        
        cycleEnd <- 1
      }
      
      step <- step + 1
    }
  }
  
  print(waitTime)
}

simulate(rep(1/2, 24), 2, 2, 2, 2, list(mean = 0.3, sd = 0.1))
