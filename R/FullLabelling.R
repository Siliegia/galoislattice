###############################################################
#' Get full Labeling of Galois Lattice
#' 
#' Adds to the resulting label of Galois lattice, the full label of all nodes
#' 
#' @param GaloisGraph the Galois Graph from do_galois_lattice
#' @param OriginalGraph the original two-mode graph used for the galois lattice
#' 
#' 
#' @return igraph object, a Galois Lattice with Full Label
#'
#' @import igraph
#' 
#' @seealso \code{\link{do_reduced_label}} for reduced label galois lattice 
#' and \code{\link{galois_layout}} for correct hierarchical plots
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
#' 
#' @export
#' 


do_full_label <- function(GaloisGraph, OriginalGraph){
  if (!is.igraph(OriginalGraph)){ OriginalGraph <- graph.incidence(OriginalGraph)}
  Ziel <- V(GaloisGraph)$name
  Ziel <- lapply(Ziel,strsplit,", ")
  Ziel <- lapply(Ziel,unlist)
  Grlabel <- lapply(Ziel,combining,OriginalGraph)
  Grlabel <- lapply(Grlabel,toString)
  V(GaloisGraph)$name <- Grlabel
  return(GaloisGraph)
}

######################################################################Auxilliary Functions
pairing <- function(L,Graph){ #finds the neighbours of elements
  N=list()
  for (i in 1:length(L)){
    out <- neighborhood(Graph,1,L[i])
    for (k in 1:length(out)){
      N[[length(N)+1]]=out[[k]]
    }
  }
  return(N)
}

combining <- function(L,Graph){ #finds the intersection of all neighbours 
  label <- V(Graph)$name
  out <- unique(c(L,label[Reduce(intersect,pairing(L,Graph))]))
  return(out)
}




