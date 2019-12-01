input <- read.table(file.path("day1", "input.txt"))
input <- input$V1

compFuel <- function(mass) {
  return(floor(mass / 3) - 2)
}

fuel <- sapply(input, compFuel)
sum(fuel)

compTotalFuel <- function(mass) {
  fuelVec <- c()
  remMass <- mass
  while (remMass > 0) {
    remMass <- floor(remMass / 3) - 2
    if (remMass > 0) {
      fuelVec <- c(fuelVec, remMass)
    }
  }
  return(sum(fuelVec))
}

totalFuel <- sapply(input, compTotalFuel)
sum(totalFuel)
