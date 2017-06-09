library(stats)
library(magrittr)

roadVector <- function(N = 0, S = 0, W = 0 , E = 0) c(N, S, W, E)

simulate <- function(parameters, distsN, distsS, distsW, distsE, distEscape) {
  if (parameters %>% length %>% mod(24) > 0) stop('parameters number must be dividable by 24')
  
  dayPhaseSteps <- length(parameters) / 24 * c(4, 5, 4, 11)
  waitingCars <- roadVector()
  waitTime <- 0
  step <- 0
  phaseEnd <- 0
  currentTime <- 0
  currentMoves <- roadVector()
  
  for (phase in 1:length(dayPhaseSteps)) {
    phaseEnd <- phaseEnd + dayPhaseSteps[[phase]]
    print(c("phase", phase))
    
    while (step < phaseEnd) {
      currentTime <- currentTime - 1
      
      if (currentMoves[3] > 0 || currentMoves[4] > 0) {
        
        # Lets last E/W car leave crossroad before switching lights
        direction <- ifelse(currentMoves[3] > 0, 3, 4)
        currentTime <- currentTime + currentMoves[direction]
        currentMoves <- currentMoves - currentMoves[direction]
        waitingCars[direction] <- waitingCars[direction] - 1
        print(c("moves1", currentMoves))
      }
      
      # Updates waiting time with cars left
      waitTime <- waitTime + sum(waitingCars)
      
      # Simulates arriving cars
      waitingCars[1] <- waitingCars[1] + rpois(1, distsN)
      waitingCars[2] <- waitingCars[2] + rpois(1, distsS)
      waitingCars[3] <- waitingCars[3] + rpois(1, distsW)
      waitingCars[4] <- waitingCars[4] + rpois(1, distsE)
      print(c("step", step, waitingCars))
      
      # Inits car entering a crossroad
      Nvalue <- ifelse(waitingCars[1] > 0, rnorm(1, distEscape[1], distEscape[2]), 0)
      Svalue <- ifelse(waitingCars[2] > 0, rnorm(1, distEscape[1], distEscape[2]), 0)
      currentMoves <- roadVector(Nvalue, Svalue, 0, 0)
      move <- min(currentMoves[currentMoves > 0])
      direction <- which.min(currentMoves[currentMoves > 0])
      
      while (currentTime < 1) {
        print(c("moves2", currentMoves))
        
        # Handles current car's move
        currentTime <- currentTime + move
        currentMoves <- currentMoves - move
        waitingCars[direction] <- waitingCars[direction] - 1
        print(c("left1", waitingCars))
        
        if (currentTime < parameters[step + 1]) {
          
          # Lets N/S car move
          currentMoves[direction] <- rnorm(1, distEscape[1], distEscape[2])
          move <- min(currentMoves[currentMoves > 0])
          direction <- which.min(currentMoves[currentMoves > 0])
          
        } else {
          if (currentMoves[1] > 0 || currentMoves[2] > 0) {
            
            # Lets last N/S car leave crossroad before switching lights
            direction <- ifelse (currentMoves[1] > 0, 1, 2)
            currentTime <- currentTime + currentMoves[direction]
            currentMoves <- currentMoves - currentMoves[direction]
            waitingCars[direction] <- waitingCars[direction] - 1
            print(c("moves3", currentMoves))
          }
          
          # Lets E/W car move
          currentMoves <- roadVector(0, 0, rnorm(1, distEscape[1], distEscape[2]), rnorm(1, distEscape[1], distEscape[2]))
          move <- min(currentMoves[currentMoves > 0])
          direction <- which.min(currentMoves[currentMoves > 0])
        }
      }
      
      step <- step + 1
    }
  }
  
  print(waitTime)
}

simulate(rep(1/2, 24), 3, 3, 3, 3, c(0.5, 0.1))
