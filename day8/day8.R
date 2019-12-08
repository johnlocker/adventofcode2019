input <- readChar(file.path("day8", "input.txt"), nchars = 1000000)
input <- as.numeric(unlist(strsplit(as.character(input), "")))

width <- 25
height <- 6
size <- width * height

layerList <- list()
nrLayers <- length(input) / size
for (i in 1:nrLayers) {
  layer <- matrix(input[c(1:size)], ncol = width, nrow = height, byrow = TRUE)
  input <- input[-c(1:size)]
  layerList[[i]] <- layer
}
zeros <- sapply(layerList, function(x) sum(x == 0))
targetLayer <- layerList[[which.min(zeros)]]

# Part 1:
sum(targetLayer == 1) * sum(targetLayer == 2)

getPixel <- function(layerList, rowNr, colNr) {
  for (i in 1:length(layerList)) {
    if (layerList[[i]][rowNr, colNr] != 2) return(layerList[[i]][rowNr, colNr])
  }
}

finalPic <- matrix(NA, ncol = width, nrow = height)
for (colNr in 1:width) {
  for (rowNr in 1:height) {
    finalPic[rowNr, colNr] <- getPixel(layerList, rowNr, colNr)
  }
}
# Part 2:
finalPic
