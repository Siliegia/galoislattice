###############################################################
#' Create a Galois lattice
#' 
#' Creates a Galois lattice for a two mode Graph, with labeling of chosen mapping
#' 
#' @param X a igraph object of a two mode network, or matrix
#' @param directed TRUE/FALSE depending on wether the output Galois lattice should be directed
#' @param by "col","row","best", depending if the result should be using the colnames, rownames 
#' or the most time efficient option
#' @param label "partly","full","reduced", depending if the result should have partly labeled nodes as chosen
#' with "by" or the full label or an reduced labeling approach
#' 
#' @return igraph object, a Galois Lattice
#'
#' @import igraph
#' 
#' @seealso \code{\link{do_full_label}} for full label galois lattice
#' and \code{\link{galois_layout}} for correct hierarchical plots and 
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
#' 
#' 
#' @export



do_galois_lattice <- function(X, directed = FALSE, by = "best", label = "partly"){
  Z <- X
  if(is.igraph(X)){
    X <- as.matrix(get.incidence(X))
  }
  for(i in 1:dim(X)[1]){
    X[i,][X[i,]!=1] <- NA
    X[i,][!is.na(X[i,]==1)] <- rmNA(colnames(X)[X[i,]==1])
  }
  if (by == "best" && dim(X)[2] < dim(X)[1]){X <- t(X)}
  if (by == "row") {X <- X}
  if (by == "col") {X <- t(X)}
  
  input <- list(X) # define input for findeventcomb function
  input[[2]] <- make_empty_graph(1, directed = directed) 
  V(input[[2]])$name = toString(colnames(X))
  
  Galois <- findeventcomb(input) # use function with this input
  Galois_graph <- simplify(Galois[[2]])
  
  input2 <- list(t(X)) # define input for the transposed matrix in case the empty set exist and wasnt found
  input2[[2]] <- make_empty_graph(1, directed = FALSE)
  V(input[[2]])$name = toString(colnames(t(X)))
  erg <- findfirst(input2) # do first step of findeventcomb to find edges between empty node and the others
  erg[1:2] <- NULL
  auxmaxComb <- unlist(erg)
  
  if (sum(V(Galois_graph)$name == toString((colnames(t(X))))) == 0){ # insert node and edges if they dont exist
    Galois_graph <- add.vertices(Galois_graph,1,name = toString(colnames(t(X))))
    for (i in 1:length(auxmaxComb)){
      Galois_graph <- add.edges(Galois_graph,c(names(auxmaxComb[i]),toString(colnames(t(X)))))
    }
  }
  
  if (label == "full"){
    Galois_graph <- do_full_label(Galois_graph, Z)
  }else if (label == "reduced"){
    Galois_graph <- do_full_label(Galois_graph, Z)
    Galois_graph <- do_reduced_label(Galois_graph, Z)
  }
  return(Galois_graph)
}


#########################################################Axillary-functions to obtain Galois Lattices

size <- function(x) length(x[!is.na(x)])
rmNA <- function(x) x[!is.na(x)]

missing <- function(M2,maxComb){ #finds missing colnames
  inother=c()
  for (i in 1:length(maxComb)){
    M3 <- M2[,which(is.na(M2[maxComb[i],])),drop=FALSE]
    Subs <- which(colSums(matrix(apply(M3,1,function(x) is.na(x)),ncol=dim(M3)[1]))==dim(M3)[2])
    inother <- c(inother, Subs)
    inother <- setdiff(inother, maxComb[i])
  }
  inother=unique(inother)
  names(inother) <- rownames(M2[inother,,drop=FALSE])
  return(inother)}

findfirst <- function(erg){ #looks for maximal rowsum as long as there are missing colnames
  M2 <- as.matrix(erg[[1]])
  maxComb <- apply(M2, 1, size)
  maxComb <- which(maxComb == max(maxComb))
  #print(maxComb)
  erg[[length(erg)+1]] <- maxComb
  inother <- missing(M2,maxComb)
  comp <- rownames(M2)
  test <- setdiff(comp,union(names(inother),names(maxComb)))
  newM <- M2[-c(maxComb,inother),,drop = FALSE]
  if (length(test)>0){ 
    aux <- findfirst(list(newM))
    if (length(aux)>1) {
      aux <- aux[2:length(aux)]
      for(i in 1:length(aux)){
        erg[[length(erg)+1]]<-aux[[i]]
      }
    }}
  return(erg)}

############################################################################Gallois Lattice Algorithm

findeventcomb <- function(result){ #actual Algorithm finding the Galloislattice
  erg <- findfirst(result)
  erg[1:2] <- NULL
  M2 <- as.matrix(result[[1]])
  auxmaxComb <- unlist(erg)
  maxComb <- match(names(auxmaxComb),rownames(M2))
  names(maxComb) <- names(auxmaxComb)
  test <- colnames(M2)
  if(!(dim(M2)[1]==0 & dim(M2)[2]==0) & any(!is.na(M2)) & length(test) > 1){
    for(i in 1:length(maxComb)){
      #print(i)
      #message( rownames(M2)[maxComb[i]] , " : ",paste0(colnames(M2)[which(!is.na(M2[maxComb[i],]))], collapse = ","))
      vt <- toString(colnames(M2))
      labelM2=c(colnames(M2)[which(!is.na(M2[maxComb[i],]))])
      if (sum(V(result[[2]])$name == toString(labelM2)) != 0 && toString(labelM2) != vt){
        result[[2]] <- add.edges(result[[2]],c(toString(colnames(M2)),toString(labelM2)))
      }else{
        if(toString(labelM2) == vt){
          result[[2]] <- add.edges(result[[2]],c(toString(colnames(M2)),toString(labelM2)))
        }else{
        result[[2]] <- add.vertices(result[[2]],1,name = list(toString(labelM2)))
        result[[2]] <- add.edges(result[[2]],c(toString(colnames(M2)),toString(labelM2)))}
        if(dim(M2)[2]!=1) {
          newM <- M2[-maxComb[i],colnames(M2)[which(!is.na(M2[maxComb[i],]))],drop = FALSE]
          auxR <- list(newM)
          auxR[[2]] <- result[[2]]
          aux_result <- findeventcomb(auxR)
          if (length(V(aux_result[[2]]))>1) {
            result[[2]] <- union(result[[2]],aux_result[[2]],byname = TRUE)
        }
      }
    }}
  }
  return(result)
}








