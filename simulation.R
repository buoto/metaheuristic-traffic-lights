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
  
  extraStops <- c(1.5)
  stopEvents <- list('1.5' = roadVector(N = 1))
  
  for (phase in 1:length(dayPhaseSteps)) {
    phaseEnd <- phaseEnd + dayPhaseSteps[[phase]]
    
    print(c("phase", phase))
    while (step < phaseEnd) {
      print(c("step", step, waitingCars))
      # some cars have waited
      waitTime <- waitTime + sum(waitingCars)
      
      # new cars appear
      # newCars TODO
      
      extra <- min(extraStops)
      while (extra < step + 1) {
        print(c("extra", extra, waitingCars))
        # handle extra events
        waitingCars <- waitingCars + stopEvents[[as.character(extra)]]
        
        # remove extra
        stopEvents[[as.character(extra)]] <- NULL
        extraStops <- extraStops[-which.min(extraStops)]
        extra <- min(extraStops)
      }
      
      step <- step + 1
    }
  }
  
  print(dayPhaseSteps)
  print(waitTime)
}

simulate(rep(1, 24), list(1), list(2), list(2), list(1), c(1, 0.1))
