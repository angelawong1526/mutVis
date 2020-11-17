#' protData
#'
#' A dataset containing 29 proteins used as comparisons
#'
#' @source \url{http://uniprot.org}
#'
#' @format A list generated from seqinr::read.fasta(file = "uniprot_sprot.fasta", seqtype = "AA") containing the known protein information:
#' \describe{
#'   \item{name}{protein name}
#'   \item{SeqFastaAA}{the sequence of the protein and information}
#'
#'   ...
#' }
#' @examples
#' \dontrun{
#'  protData
#' }
#'
"protData"
