#' Find percentage of matching amino acids
#'
#' A helper function that calculates the percentage of TRUE after comparing two
#' vectors
#'
#' @param protSeq Chr, A character vector of unknown protein sequence
#' @param knownProt Chr, A character vector of a known protein sequence
#'
#' @return Numerical, The percentage of TRUE after comparing or 0 if the vectors are of different lengths
#' @examples
#' testing1 <- c("a", "b", "c", "d")
#' testing2 <- c("a", "b", "e", "f")
#' match <- mutVis:::findPercentage(testing1, testing2)
#' match
#'
findPercentage <- function(protSeq, knownProt) {
  # Find percentage of AA that match
  if (length(protSeq) == length(knownProt)) {
    # get a logical vector of match
    matchLogi <- (protSeq == knownProt)
    # find sum of TRUE
    matchSum <- sum(matchLogi)
    # find percent of TRUE
    matchPercent <- matchSum / length(protSeq)
    return(matchPercent)
  }
  else {
    return(0)
  }
}


#' Find best match proteins with unknown protein
#'
#' A helper function that generates a dataframe listing the amino acid
#' differences in numeric form between the input and the known proteins
#'
#' @param protSeq Chr, The character vector of the input protein sequence
#' @param dataset The dataset in .fasta format that contains the protein sequences of known proteins and their names
#' @param minMatch Numerical, the minimum percent of matching
#'
#' @return A dataframe specifying where the input protein differs from the known proteins
#' @examples
#' inputProt <- paste("MAVLILVLLAVVILQAAPIRKLEDLLPTRYPPDHELVYWCTYANQCDFCWECVH",
#' "GICRNRIQADWPVIHQNDWIINCTVSRWNGICSYYEGPRNHTDHQMDCANPTSHTYPHREYMKIYERDDL",
#' sep = "")
#' unknown1 <- seqinr::s2c(inputProt)
#' minMatch <- 98
#' result <- mutVis:::matchedProteins(unknown1, protData, minMatch)
#' result
#'
#' @references
#' Charif, D. and Lobry, J.R. (2007). seqinr. https://cran.r-project.org/web/packages/seqinr/index.html
#'
#' @import seqinr
#'
#'
matchedProteins <- function(protSeq, dataset = protData, minMatch) {

  # create data frame with protSeq (one column for each amino acid position)
  # last column of data frame is for names of known protein
  protDF <- data.frame(position = 1:length(protSeq))

  # get only the names from dataset
  protNames <- getName(dataset)

  # unknown input protein sequence in numeric form
  protSeqNum <- chrToNum(protSeq)

  # to keep track of the number of known sequences that has been matched
  # the number represents the column index in the protein dataframe
  # the first column in the dataframe is the unknown protein sequence positions
  knownCount <- 2

  for (i in seq_along(protNames)) {
    # get sequence of known protein
    knownProt <- dataset[[protNames[i]]]
    knownProtSeq <- getSequence.SeqFastaAA(knownProt)
    # find percentage of match
    matchPercent <- findPercentage(protSeq, knownProtSeq)

    if ((matchPercent * 100) >= minMatch) {
      # numeric form of known protein sequence
      knownProtNum <- chrToNum(knownProtSeq)
      # add new column for known sequence
      protDF[[protNames[i]]] <- NA
      # compare unknown with known sequence and record differences in data frame
      protDF <- compareAndUpdate(protSeqNum, knownProtNum, protDF, knownCount)
      # update count
      knownCount <- knownCount + 1
    }
  }
  return(protDF)
}


#' Convert protein sequence into numeric form
#'
#' A helper function that converts amino acid letters ("A, G, I, L, P, V, F, W, Y, D,
#' E, R, H, K, S, T, C, M, N, Q") into numbers in that order starting from 1
#'
#' @param protVec Chr, a character vector of amino acid letters
#'
#' @return Numerical, A numerical vector
#' @examples
#' tempVec <- c("A", "C", "G")
#' numericForm <- mutVis:::chrToNum(tempVec)
#' numericForm
#'
chrToNum <- function(protVec) {
  newResult <- vector()
  proteins <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
                19, 20)
  names(proteins) <- s2c("AGILPVFWYDERHKSTCMNQ")
  proteinDF <- data.frame(as.list(proteins))
  for (i in seq_along(protVec)) {
    if (protVec[i] %in% s2c("AGILPVFWYDERHKSTCMNQ")) {
      newResult <- c(newResult, proteinDF[[protVec[i]]])
    }
    else {
      newResult <- c(newResult, NA)
    }
  }
  return(newResult)
}


#' compare numeric and update dataframe
#'
#' A helper function that modifies the input data frame
#'
#' @param unknown Numerical, A numerical vector of the unknown protein sequence
#' @param known Numerical, A numerical vector of the known protein sequence
#' @param protFrame The data frame that needs to be updated
#' @param knownCount Numerical, the number of known proteins that has been compared
#'
#' @return An undated version of input dataframe
#' @examples
#' unknownTest <- c(1, 2, 3, 4, 5)
#' knownTest <- c(1, 2, 3, 6, 7)
#' emptyFrame <- data.frame(position = 1:length(unknownTest))
#' emptyFrame[["known"]] <- NA
#' knownCount = 2
#' mutVis:::compareAndUpdate(unknownTest, knownTest, emptyFrame, knownCount)
#'
compareAndUpdate <- function(unknown, known, protFrame, knownCount) {
  tempFrame <- protFrame
  for (i in seq_along(unknown)) {
    # if the amino acid number of unknown is different from known sequence
    if (unknown[i] != known[i]) {
      # update the number under known sequence column
      tempFrame[i, knownCount] <- known[i]
    }
    # if they are the same, add 0 under the position
    else {
      tempFrame[i, knownCount] <- 0
    }
  }
  return(tempFrame)
}

# [END]
