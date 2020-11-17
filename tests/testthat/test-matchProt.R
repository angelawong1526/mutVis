context("Find best match for unknown protein")
library("mutVis")

test_that("find best match", {
  unknownProt <- paste("MAVLILVLLAVVILQAAPIRKLEDLLPTRYPPDHELVYWCTYANQCDFCWECVH",
                       "GICRNRIQADWPVIHQNDWIINCTVSRWNGICSYYEGPRNHTDHQMDCANPTSH",
                       "TYPHREYMKIYERDDL", sep = "")
  minPercent <- 85
  result <- matchProt(unknownProt, minMatch = minPercent)

  expect_is(result, "ggplot")
})
