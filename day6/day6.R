input <- read.table(file.path("day6", "input.txt"), stringsAsFactors = FALSE)$V1
inputDF <- data.frame(Relation = input,
                      planet = sapply(input, function(x) unlist(strsplit(x, split = ")"))[1]),
                      orbit = sapply(input, function(x) unlist(strsplit(x, split = ")"))[2]),
                      stringsAsFactors = FALSE)


orbitDF <- data.frame(planets = unique(unlist(strsplit(input, ")"))),
                      stringsAsFactors = FALSE)
orbitDF$orbitCount <- 0
inputDF$done <- FALSE

curPlanet <- "COM"
planetIdxStack <- c()
while (!all(inputDF$done)) {
  planetIdx <- which(inputDF$planet == curPlanet & !inputDF$done)
  if (length(planetIdx) == 0) {
    planetIdx <- planetIdxStack[1]
    planetIdxStack <- planetIdxStack[-1]
  }
  if (length(planetIdx) > 1) {
    planetIdxStack <- c(planetIdxStack, planetIdx[c(2:length(planetIdx))])
    planetIdx <- planetIdx[1]
  }
  orbit <- inputDF$Relation[planetIdx]
  inputDF$done[planetIdx] <- TRUE
  # direct
  motherPlanet <- inputDF$planet[planetIdx]
  orbited <- inputDF$orbit[planetIdx]
  orbitDF$orbitCount[which(orbitDF$planets == orbited)] <- orbitDF$orbitCount[which(orbitDF$planets == orbited)] + 1
  # indirect
  orbitDF$orbitCount[which(orbitDF$planets == orbited)] <- (orbitDF$orbitCount[which(orbitDF$planets == orbited)] +
                                                            orbitDF$orbitCount[which(orbitDF$planets == motherPlanet)])
  curPlanet <- orbited
}
# Part 1:
sum(orbitDF$orbitCount)

grid <- matrix(0, ncol = nrow(orbitDF), nrow = nrow(orbitDF),
               dimnames = list(orbitDF$planets,
                               orbitDF$planets))
for (i in 1:nrow(inputDF)) {
  grid[which(rownames(grid) == inputDF$planet[i]), which(colnames(grid) == inputDF$orbit[i])] <- 1
  grid[which(rownames(grid) == inputDF$orbit[i]), which(colnames(grid) == inputDF$planet[i])] <- 1
}
library(qgraph)
gridGraph <- qgraph(grid)
cent <- centrality(gridGraph, all.shortest.paths = TRUE, weighted = FALSE)
pathMa <- cent$ShortestPathLengths
# Part 2:
pathMa[which(colnames(pathMa) == "SAN"), which(rownames(pathMa) == "YOU")] - 2
