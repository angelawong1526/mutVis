#' Find the most plausible protein sequence matches of unknown input and plot
#' the amino acid differences
#'
#' A function that lists the best matched protein sequences, compares the
#' candidate sequence with the unknown input, and plots the differences
#'
#' @param unknownProt String, The unknown protein sequence.
#' @param dataset The dataset that stores the information of known proteins with two columns, name and sequence.
#' @param minMatch The minimum percentage of same amino acid with unknown (default is 85%)
#'
#' @return A graph of all the amino acid differences between input and known proteins, with the position on input protein
#'
#' @examples
#' unknownProt <- paste("MAVLILVLLAVVILQAAPIRKLEDLLPTRYPPDHELVYWCTYANQCDFCWECVH",
#' "GICRNRIQADWPVIHQNDWIINCTVSRWNGICSYYEGPRNHTDHQMDCANPTSHTYPHREYMKIYERDDL",
#' sep = "")
#' minPercent <- 85
#' result <- matchProt(unknownProt, minMatch = minPercent)
#' result
#'
#' @export
#' @import ggplot2
#' @import reshape
#' @import seqinr
#'
#'
matchProt <- function(unknownProt, dataset = protData, minMatch = 85) {
  # the function uses dataset in package as default package
  # convert string into vector
  protSeq <- seqinr::s2c(unknownProt)
  # get match information with all protein sequences in dataset
  # Format: one row for each amino acid position and last row for known protein name
  # number of rows depends on the length of input protein sequence
  matchData <- matchedProteins(protSeq, dataset, minMatch)

  ################### Visualization ###################

  # graph scatter plot
  meltMatchData <- reshape::melt(matchData, id.vars = 'position',
                                 variable_name = 'knownProteins')
  protPlot <- ggplot2::ggplot(meltMatchData, aes(x = position, y = value,
                                                 col = knownProteins)) +
    geom_point() +
    labs(x = "Positions on Input", y = "Amino Acid Numeric Form") +
    ggtitle("Input Protein vs. Best Matches") + theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    facet_grid(knownProteins ~ .)
  return(protPlot)

}

# [END]
