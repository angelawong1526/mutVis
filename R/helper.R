#' Find percentage of matching amino acids
#'
#' A function that calculates the percentage of TRUE after comparing two vectors
#'
#' @param protSeq Chr, A character vector of unknown protein sequence
#' @param knownProt Chr, A character vector of a known protein sequence
#'
#' @return Numerical, The percentage of TRUE after comparing or 0 if the vectors are of different lengths
#'
#' @export
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
#' A function that generates a dataframe listing the amino acid differences in
#' numeric form between the input and the known proteins
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
#' result <- matched(unknown1, protData, minMatch)
#' result
#'
#' @export
#' @import seqinr
#'
#'
matched <- function(protSeq, dataset = protData, minMatch) {

  # create data frame with protSeq (one column for each amino acid position)
  # last column of data frame is for names of known protein
  protDF <- emptyDF(protSeq)

  # get only the names from dataset
  protNames <- getName(dataset)

  # unknown input protein sequence in numeric form
  protSeqNum <- c2n(protSeq)

  # to keep track of the number of known sequences that has been matched
  knownCount <- 1

  for (i in seq_along(protNames)) {
    # get sequence of known protein
    knownProt <- dataset[[protNames[i]]]
    knownProtSeq <- getSequence.SeqFastaAA(knownProt)
    # find percentage of match
    matchPercent <- findPercentage(protSeq, knownProtSeq)

    if ((matchPercent * 100) >= minMatch) {
      # numeric form of known protein sequence
      knownProtNum <- c2n(knownProtSeq)
      # add known sequence name to last column data frame (knownProtein)
      protDF[knownCount, ncol(protDF)] <- protNames[i]
      # compare unknown with known sequence and record differences in data frame
      protDF <- compareAndUpdate(protSeqNum, knownProtNum, protDF, knownCount)
      # update count
      knownCount <- knownCount + 1
    }
  }
  return(protDF)
}


#' Create empty dataframe
#'
#' A function that creates an empty dataframe (one character per column)
#'
#' @param protVector Chr, a character vector
#'
#' @return a dataframe with length(protVector) + 1 columns and 0 rows
#'
#' @export
#'
emptyDF <- function(protVector) {
  newDF <- data.frame()
  for (i in seq_along(protVector)) {
    protPos <- paste("pos", i, sep = "")
    newDF[[protPos]] <- vector()
  }
  newDF[["knownProtein"]] <- vector()
  return(newDF)
}


#' Convert protein sequence into numeric form
#'
#' A function that converts amino acid letters ("A, G, I, L, P, V, F, W, Y, D,
#' E, R, H, K, S, T, C, M, N, Q") into numbers in that order starting from 1
#'
#' @param protVec Chr, a character vector of amino acid letters
#'
#' @return Numerical, A numerical vector
#'
#' @export
#'
c2n <- function(protVec) {
  newResult <- vector()
  for (i in seq_along(protVec)) {
    if (protVec[i] == "A") {
      newResult <- c(newResult, 1)
    }
    else if (protVec[i] == "G") {
      newResult <- c(newResult, 2)
    }
    else if (protVec[i] == "I") {
      newResult <- c(newResult, 3)
    }
    else if (protVec[i] == "L") {
      newResult <- c(newResult, 4)
    }
    else if (protVec[i] == "P") {
      newResult <- c(newResult, 5)
    }
    else if (protVec[i] == "V") {
      newResult <- c(newResult, 6)
    }
    else if (protVec[i] == "F") {
      newResult <- c(newResult, 7)
    }
    else if (protVec[i] == "W") {
      newResult <- c(newResult, 8)
    }
    else if (protVec[i] == "Y") {
      newResult <- c(newResult, 9)
    }
    else if (protVec[i] == "D") {
      newResult <- c(newResult, 10)
    }
    else if (protVec[i] == "E") {
      newResult <- c(newResult, 11)
    }
    else if (protVec[i] == "R") {
      newResult <- c(newResult, 12)
    }
    else if (protVec[i] == "H") {
      newResult <- c(newResult, 13)
    }
    else if (protVec[i] == "K") {
      newResult <- c(newResult, 14)
    }
    else if (protVec[i] == "S") {
      newResult <- c(newResult, 15)
    }
    else if (protVec[i] == "T") {
      newResult <- c(newResult, 16)
    }
    else if (protVec[i] == "C") {
      newResult <- c(newResult, 17)
    }
    else if (protVec[i] == "M") {
      newResult <- c(newResult, 18)
    }
    else if (protVec[i] == "N") {
      newResult <- c(newResult, 19)
    }
    else if (protVec[i] == "Q") {
      newResult <- c(newResult, 20)
    }
    else {
      newResult <- c(newResult, NA)
    }
  }
  return(newResult)
}


#' compare numeric and update dataframe
#'
#' A function that modifies the input data frame
#'
#' @param unknown Numerical, A numerical vector of the unknown protein sequence
#' @param known Numerical, A numerical vector of the known protein sequence
#' @param protFrame The data frame that needs to be updated
#' @param knownCount Numerical, the number of known proteins that has been compared
#'
#' @return An undated version of input dataframe
#'
#' @export
#'
compareAndUpdate <- function(unknown, known, protFrame, knownCount) {
  tempFrame <- protFrame
  for (i in seq_along(unknown)) {
    # if the amino acid number of unknown sequence is different from known sequence
    if (unknown[i] != known[i]) {
      # add the amino acid number of known
      tempFrame[knownCount, i] <- known[i]
    }
    # if they are the same, add 0 under the position
    else {
      tempFrame[knownCount, i] <- 0
    }
  }
  return(tempFrame)
}

# [END]
