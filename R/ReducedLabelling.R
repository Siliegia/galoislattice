###############################################################
#' Get Reduced Labelling of Galois Lattice
#' 
#' Reduces full label of Galois lattice to a specific reduced label
#' 
#' @param GaloisGraph the Galois Graph from do_galois_lattice with full label
#' @param OriginalGraph the original two-mode graph used for the galois lattice
#' 
#' 
#' @return igraph object, a Galois Lattice with Reduced Label
#' 
#' @details function can only be used for a full labeled galois lattice
#'
#' @import igraph 
#' 
#' @seealso \code{\link{galois_layout}} for correct hierarchical plots and 
#' \code{\link{do_dominance_tree}} for extracting positional dominance from a galois lattice
#' 
#' @examples 
#' M=matrix(c(1,1,1,0,0,0,
#' 0,0,0,1,1,1,
#' 1,0,0,1,0,0,
#' 1,1,0,1,0,1),nrow=6)
#' colnames(M) <- c("A", "B", "C", "D")
#' rownames(M) <- as.character(1:6)
#' Galois <- do_galois_lattice(M)
#' Galois <- do_full_label(Galois, M)
#' Galois <- do_reduced_label(Galois, M)
#' 
#' @export
#' 


do_reduced_label <- function(GaloisGraph, OriginalGraph){ # actual function to reduce the label
  if (!is.igraph(OriginalGraph)){ OriginalGraph <- graph.incidence(OriginalGraph)}
  L <- V(GaloisGraph)$name
  L <- lapply(L,strsplit,", ")
  L <- lapply(L, unlist)
  Newlabel1 <- lapply(L,ReduceGrlabel,OriginalGraph,"a")
  Newlabel2 <- lapply(L,ReduceGrlabel,OriginalGraph,"b")
  out5 <- vector("list", length(L))
  for (i in 1:length(Newlabel1)){
    out5[[i]] <- toString(c(Newlabel1[[i]],Newlabel2[[i]]))
  }
  if (out5[[1]] == ""){
  out5[[1]] <- "0"}
  if (out5[[length(out5)]] == ""){
  out5[[length(out5)]] <- "00"}
  V(GaloisGraph)$name <- out5
  return(GaloisGraph)
}

################################################Auxilliary Function

ReduceGrlabel <- function(L,Graph,m){ #names1 <- colnames of M, names2 <- rownames of M
  out=c()
  names <- V(Graph)$name
  if (m == "a"){
    names1 <- names[V(Graph)$type]
    names2 <- names[V(Graph)$type == FALSE]}
  else{names2 <- names[V(Graph)$type]
  names1 <- names[V(Graph)$type == FALSE]}
  L2 <- intersect(L,names1)
  if (length(L2) > 0){
    for (i in 1:length(L2)){
      N <- neighborhood(Graph,1,L2[i])
      Z <- setdiff(N[[1]],which(names==L2[i]))
      if ( is.element(FALSE,is.element(names[Z],intersect(L,names2)))== FALSE){
        out <- c(out,L2[i])
      }}}
  return(out)
}

