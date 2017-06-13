[![Travis-CI Build Status](https://travis-ci.org/Siliegia/galoislattice.svg?branch=master)](https://travis-ci.org/Siliegia/galoislattice)

# galoislattice
A R package to create Galois lattices (Concept Lattice) for two-mode networks and analyze them. By using a reduced label for directed galois lattices,
the positional dominance of the original network can be extracted. 

This package depends on the R package igraph.

## Installation

You can install galoislattice from github with:

```R
# install.packages("devtools")
devtools::install_github("Siliegia/galoislattice")
```

## Example

Here you can see how to use ```galoislattice``` on a two-mode igraph object. As an example we use the well known southern women dataset   ```southernwomenG```, which is in the package.

```R
data("southernwomenG")

# create a Galois Lattice with a reduced label
Galois <- do_galois_lattice(southernwomenG, label = "reduced")

# plot the Galois Lattice with the according layout
plot(Galois, layout = galois_layout(Galois))

# create a Galois Lattice to extract positional dominance
G <- do_galois_lattice(southernwomenG, label = "reduced", directed = TRUE)
# extracting the dominance relations for the women
Tree <- do_dominance_tree(G, nodes = V(southernwomenG)$name[V(southernwomenG)$type==0])
plot(Tree)
```
The ```do_galois_lattice``` function returns an igraph object, which is the Galois lattice formed from the two-mode network. ```galois_layout``` will return the level of positional dominance on the y-axis and ```do_dominance_tree``` extracts the positional dominance from the directed reduced labeled Galois Lattice. 
