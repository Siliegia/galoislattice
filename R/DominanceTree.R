###############################################################
#' Find dominace Tree
#' 
#' Finds the positional dominance between two nodes, by finding all shortest path between the nodes in a 
#' galois lattice
#' 
#' @param graph a Galois lattice of which the dominance should be found
#' @param from the node from where to start the path search
#' @param to the node to which the shortest path should be found
#' @param nodes the labels of those nodes for which one is interested in knowing the dominace relation
#' for example the names of all affiliations
#' 
#' 
#' @return igraph object, a Tree describing the dominace between nodes
#' 
#' @details 
#' The algorithm should be used with a directed galois lattice, e.g. G <- do_galois_lattice(X, directed = TRUE).
#' The algorithm returns the positional dominance of the original graph, if it is applied on the
#' REDUCED label of the galois lattice. A Galois lattice has two possible directions and by using either of them
#' the positional dominance for actors and affiliations can be calculated, but once a direction is chosen
#' from and to nodes have to be chosen appropriately.
#'
#' @seealso \code{\link{do_galois_lattice}} for constructing the according input graph
#' 
#' @import igraph
#' @importFrom utils head tail
#' 
#' @examples 
#' M=matrix(c(1,1,1,0,0,0,
#' 0,0,0,1,1,1,
#' 1,0,0,1,0,0,
#' 1,1,0,1,0,1),nrow=6)
#' colnames(M) <- c("A", "B", "C", "D")
#' rownames(M) <- as.character(1:6)
#' Galois <- do_galois_lattice(M, directed = TRUE, label = "reduced")
#' T <- do_dominance_tree(Galois,as.character(1:6))
#' plot(T)
#' 
#' @export
#' 

do_dominance_tree <- function(graph, nodes, from = names(head(V(graph),n=1)),to = names(tail(V(graph),n=1))){
  nodes <- graph$match.name[match(nodes, table = graph$match.name[,2]),1]
  
  if(is.character(from)){
    from <- unlist(V(graph)$l.name[match(from, table = V(graph)$name)])}
  if(is.character(to)){
    to <- unlist(V(graph)$l.name[match(to, table = V(graph)$name)])}
  
  V(graph)$name <- V(graph)$l.name 
  
  res <- all_simple_paths(graph,from = from, to = to)
  test <- make_empty_graph(n=1,directed= TRUE)
  V(test)$name <- from
  Tree <- lapply(res,do_tree,nodes = nodes, test = test, graph = graph)
  bigTree <- Reduce(union,Tree)
  
  if (!is.element(names(head(V(graph),n=1)),nodes)){
    bigTree <- delete_vertices(bigTree, names(head(V(graph),n=1)))}
  if (!is.element(names(tail(V(graph),n=1)),nodes)){
    bigTree <- delete_vertices(bigTree, names(tail(V(graph),n=1)))}
  
  L.name <- V(bigTree)$name
  Lnew.name <- graph$match.name[match(L.name, graph$match.name[,1]),2]
  
  V(bigTree)$name <- Lnew.name
  
  return(bigTree)
}

############################################## Auxilliary Function
do_tree <- function(L,nodes,test, graph){
  L2 <- intersect(unlist(strsplit(as.character(names(L)),", ")),c(nodes,names(head(V(graph),n=1)),
                                                                  names(tail(V(graph),n=1))))
  for (i in 2:length(L2)){
    test <- add.vertices(test,1, name = L2[i])
    test <- add.edges(test,c(L2[i-1],L2[i]))
  }
  return(test)
}

