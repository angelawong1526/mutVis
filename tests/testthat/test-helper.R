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


test_that("matched works", {
  unknownProt <- paste("MAVLILVLLAVVILQAAPIRKLEDLLPTRYPPDHELVYWCTYANQCDFCWECVH",
                       "GICRNRIQADWPVIHQNDWIINCTVSRWNGICSYYEGPRNHTDHQMDCANPTSH",
                       "TYPHREYMKIYERDDL", sep = "")
  minPercent <- 85
  inputProt <- seqinr::s2c(unknownProt)
  protData <- matched(inputProt, dataset = protData, minPercent)

  expect_equal(ncol(protData), (length(inputProt) + 1))
  expect_equal(protData[1, 1], protData[2, 1])
})


test_that("get empty data frame", {
  randomVec <- c(1, 2, 3, 4)
  newDF <- emptyDF(randomVec)

  expect_equal(ncol(newDF), (length(randomVec) + 1))
})


test_that("get vector with all amino acid letters converted into numbers", {
  randomProtein <- c("A", "M", "N", "L")
  randomProteinNum <- c2n(randomProtein)
  proteinNum <- as.numeric(c(1, 18, 19, 4))

  expect_equal(randomProteinNum, proteinNum)
})
