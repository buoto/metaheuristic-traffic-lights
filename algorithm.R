library(plotrix)
library(polynom)
library(magrittr)

#' Mutation evolutionary algorithm.
#' 
#' @param heuristic Optimized function - must accept a list of points and return list of heuristic scores.
#' @param min Explored space lower bound.
#' @param max Explored space upper bound.
#' @param mu Parent population count.
#' @param maxSteps steps 
#' @param mutationMean Mutation normal distribution mean parameter.
#' @param mutationSd Mutation normal distribution standard deviation parameter.
#' @param selectionMinProb Selection probability bias.
#' @return Best point in log.
mea <- function(heuristic, min, max, mu, maxSteps = 10, mutationMean = 1, mutationSd = mutationMean, selectionMinProb = 0.1) {
  if (any(max < min)) stop('All values in min must be lower than corresponding max values.') 
  P <- initPopulation(min, max, mu)
  hP <- sapply(P, heuristic)
  
  best <- list(point = P[[which.min(hP)]], h = min(hP))
  
  for (t in 1:maxSteps) {
    #tic()
    message('generation ', t)
    # for testing
    #points(unlist(P), hP)
    #invisible(readline(prompt = "Press [enter] to continue"))
    R <- reproduce(P, hP, mu, selectionMinProb)
    
    O <- mutate(R, min, max, mutationMean, mutationSd)
    hO <- sapply(O, heuristic)
    
    if (min(hO) < best$h) {
      best <- list(point = O[[which.min(hO)]], h = min(hO))
      message('new best found h = ', best$h)
    }
    
    P <- O
    hP <- hO
    #toc()
  }
  best
}

initPopulation <- function(min, max, count) replicate(count, runif(length(min), min = min, max = max), simplify = FALSE)

reproduce <- function(population, scores, lambda, minProb) {
  R <- list()
  for (i in 1:lambda) {
    msk <- select(population, scores, 2, minProb = minProb)
    R <- c(R, list(crossover(population[msk])))
  }
  R
}

crossover <- function(pair) {
  ratio <- runif(1)
  pair[[1]] * ratio + pair[[2]] * (1 - ratio)
}

mutate <- function(population, min, max, mean, sd) {
  lapply(population, function(point) (point + rnorm(length(point), mean = mean, sd = sd)) %>% normalize(min, max))
}

normalize <- function(point, min, max) mod(point - min, max - min) + min
  
select <- function(population, scores, count, minProb = 0) {
  scores <- -scores # minimize
  normalizedScores <- scores - min(scores)
  sample(1:length(population), size = count, replace = TRUE, prob = normalizedScores + minProb * mean(normalizedScores))
}
