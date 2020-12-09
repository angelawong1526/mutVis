context("Helper functions")
library("mutVis")
library(seqinr)


test_that("get percentage of same", {
  a <- c(1, 2, 3, 4)
  b <- c(1, 2, 4, 4)
  percent <- findPercentage(a, b)
  n <- 0.75

  expect_equal(percent, n)
})


test_that("matchedProteins works", {
  unknownProt <- paste("MAVLILVLLAVVILQAAPIRKLEDLLPTRYPPDHELVYWCTYANQCDFCWECVH",
                       "GICRNRIQADWPVIHQNDWIINCTVSRWNGICSYYEGPRNHTDHQMDCANPTSH",
                       "TYPHREYMKIYERDDL", sep = "")
  minPercent <- 85
  inputProt <- seqinr::s2c(unknownProt)
  protData <- matchedProteins(inputProt, dataset = protData, minPercent)

  expect_equal(nrow(protData), length(inputProt))
})


test_that("get vector with all amino acid letters converted into numbers", {
  randomProtein <- c("A", "M", "N", "L")
  randomProteinNum <- chrToNum(randomProtein)
  proteinNum <- as.numeric(c(1, 18, 19, 4))

  expect_equal(randomProteinNum, proteinNum)
})

test_that("test compareAndUpdate works", {
  unknownTest <- c(1, 2, 3, 4, 5)
  knownTest <- c(1, 2, 3, 6, 7)
  emptyFrame <- data.frame(position = 1:length(unknownTest))
  emptyFrame[["known"]] <- NA
  knownCount = 2
  updated <- compareAndUpdate(unknownTest, knownTest, emptyFrame, knownCount)

  expect_equal(updated[1, 2], 0)
  expect_equal(updated[5, 2], 7)
})
