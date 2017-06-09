source('algorithm.R')

randomPoint(list(c(1, 2), c(4,5), c(1,6)))
initPopulation(list(c(1, 2), c(4,5), c(1,6)), 4)

normalize(c(-1.1, 1, 0), list(c(-1, 1), c(0.12, 0.20), c(1,2)))

mutate(list(c(0.5, 0.5, 0.5), rep(1, 3)), list(c(0,1), c(0.5, 2), c(0, 1)), 0.1, 0.1)

f <- polynomial(c(-1, -1, -2, -1, 0.2))
plot(f, xlim=c(-10, 10))

mea(list(c(-10, 10)), 5, as.function(f))
