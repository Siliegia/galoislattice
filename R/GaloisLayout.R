###############################################################
#' Layout for plotting a Galois lattice
#' 
#' orders the nodes of a Galois lattice according to their hierarchical position
#' 
#' @param X a Galois lattice, as the output of do_galois_lattice
#' 
#' 
#' @return matrix, the layout to use in plot for the galois lattice
#' 
#' 
#' @import igraph 
#' 
#' @seealso \code{\link{do_dominance_tree}} for extracting positional dominance from a galois lattice
#' 
#' @examples 
#' M=matrix(c(1,1,1,0,0,0,
#' 0,0,0,1,1,1,
#' 1,0,0,1,0,0,
#' 1,1,0,1,0,1),nrow=6)
#' colnames(M) <- c("A", "B", "C", "D")
#' rownames(M) <- as.character(1:6)
#' Galois <- do_galois_lattice(M)
#' plot(Galois, layout = galois_layout(Galois))
#' 
#' 
#' @export
#' 


galois_layout <- function(X){
  n <- length(V(X)$name)
  d <- distances(X, v = c(V(X)[1],V(X)[n]) , to = V(X))
  d1 <- as.vector(d[1,]/(d[1,]+d[2,]))*max(d[1,])
  l <- layout.fruchterman.reingold(X)
  z <- c(0,1)
  a <- sum((z)*(l[n,]-l[1,]))
  a1 <- sqrt(sum((z)^2))
  a2 <- sqrt(sum((l[n,]-l[1,])^2))
  alpha <- acos(a/(a1*a2))
  if(l[n,1] < l[1,1]){R <- matrix(c(cos(alpha), -sin(alpha), sin(alpha), cos(alpha)),nrow = 2)
  }else{R <- t(matrix(c(cos(alpha), -sin(alpha), sin(alpha), cos(alpha)),nrow = 2))}
  TR <- t(R%*%t(l))
  d2 <- TR[,1]

  Galois.layout <- cbind(d2,d1)
  return(Galois.layout)
}