possiblePws <- c(165432:707912)

checkPw <- function(num, part2 = FALSE) {
  splitNum <- as.numeric(unlist(strsplit(as.character(num), split = "")))
  sameTwice <- logical(length = length(splitNum) - 1)
  for (i in 2:length(splitNum)) sameTwice[i - 1] <- splitNum[i - 1] == splitNum[i]
  if (!any(sameTwice)) return(FALSE)

  if (part2) {
    dupIndex <- splitNum[which(sameTwice) + 1]
    numTab <- table(splitNum)
    dupFreq <- as.numeric(sapply(unique(dupIndex), function(x) numTab[names(numTab) == x]))
    if (!2 %in% dupFreq) return(FALSE)
  }

  higher <- logical(length = length(splitNum) - 1)
  for (i in 2:length(splitNum)) higher[i - 1] <- splitNum[i - 1] <= splitNum[i]
  if (!all(higher)) return(FALSE)

  return(TRUE)
}

checks <- sapply(possiblePws, checkPw)
# Part 1:
sum(checks)

# Part 2:
checks <- sapply(possiblePws, function(x) checkPw(x, part2 = TRUE))
sum(checks)
