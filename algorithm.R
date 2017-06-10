library(plotrix)
library(polynom)
library(magrittr)

#' Mutation evolutionary algorithm.
#' 
#' @param bounds List of ranges for every dimension.
#' @param mu Parent population count.
#' @param heuristic Optimized function - must accept a list of points and return list of heuristic scores.
#' @param maxSteps steps 
#' @param mutationMean Mutation normal distribution mean parameter.
#' @param mutationSd Mutation normal distribution standard deviation parameter.
#' @param selectionMinProb Selection probability bias.
#' @return Best point in log.
mea <- function(bounds, mu, heuristic, maxSteps = 10, mutationMean = 1, mutationSd = mutationMean, selectionMinProb = 0.1) {
  P <- initPopulation(bounds, mu)
  hP <- sapply(P, heuristic)
  
  best <- list(point = P[[which.min(hP)]], h = min(hP))
  
  for (t in 1:maxSteps) {
    #tic()
    print(t)
    # for testing
    #points(unlist(P), hP)
    #invisible(readline(prompt="Press [enter] to continue"))
    R <- list()
    for (i in 1:mu) {
      msk <- select(P, hP, 2, minProb = selectionMinProb)
      R <- c(R, list(crossover(P[msk])))
    }
    
    O <- mutate(R, bounds, mutationMean, mutationSd)
    hO <- sapply(O, heuristic)
    
    if (min(hO) < best$h) {
      best <- list(point = O[[which.min(hO)]], h = min(hO))
      print(c(best = best$h))
    }
    
    P <- O
    hP <- hO
    #toc()
  }
  best
}

initPopulation <- function(bounds, count) replicate(count, randomPoint(bounds), simplify = FALSE)

randomPoint <- function(bounds) sapply(bounds, function(range) runif(1, min = range[1], max = range[2]))

reproduce <- function(population) population %>% combn(2, simplify = FALSE) %>%  lapply(crossover)

crossover <- function(pair) {
  ratio <- runif(1)
  pair[[1]] * ratio + pair[[2]] * (1 - ratio)
}

mutate <- function(population, bounds, mean, sd) {
  lapply(population, function(point) (point + rnorm(length(point), mean = mean, sd = sd)) %>% normalize(bounds))
}

normalize <- function(point, bounds) {
  apply(rbind(point, bounds), 2, function(pair) normalizeValue(pair[[1]], unlist(pair[[2]])))
}
  
normalizeValue <- function(value, range) {
  min <- range[[1]]
  max <- range[[2]]
  mod(value - min, max - min) + min
}

select <- function(population, scores, count, minProb = 0) {
  scores <- -scores # minimize
  normalizedScores <- scores - min(scores)
  sample(1:length(population), size = count, replace = TRUE, prob = normalizedScores + minProb * mean(normalizedScores))
}
