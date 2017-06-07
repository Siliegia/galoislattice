pkgname <- "galoislattice"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('galoislattice')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("do_full_label")
### * do_full_label

flush(stderr()); flush(stdout())

### Name: do_full_label
### Title: Get full Labeling of Galois Lattice
### Aliases: do_full_label

### ** Examples

M=matrix(c(1,1,1,0,0,0,
0,0,0,1,1,1,
1,0,0,1,0,0,
1,1,0,1,0,1),nrow=6)
colnames(M) <- c("A", "B", "C", "D")
rownames(M) <- as.character(1:6)
Galois <- do_galois_lattice(M)
Galois <- do_full_label(Galois, M)




cleanEx()
nameEx("do_galois_lattice")
### * do_galois_lattice

flush(stderr()); flush(stdout())

### Name: do_galois_lattice
### Title: Create a Galois lattice
### Aliases: do_galois_lattice

### ** Examples

M=matrix(c(1,1,1,0,0,0,
0,0,0,1,1,1,
1,0,0,1,0,0,
1,1,0,1,0,1),nrow=6)
colnames(M) <- c("A", "B", "C", "D")
rownames(M) <- as.character(1:6)
Galois <- do_galois_lattice(M)





cleanEx()
nameEx("do_reduced_label")
### * do_reduced_label

flush(stderr()); flush(stdout())

### Name: do_reduced_label
### Title: Get Reduced Labelling of Galois Lattice
### Aliases: do_reduced_label

### ** Examples

M=matrix(c(1,1,1,0,0,0,
0,0,0,1,1,1,
1,0,0,1,0,0,
1,1,0,1,0,1),nrow=6)
colnames(M) <- c("A", "B", "C", "D")
rownames(M) <- as.character(1:6)
Galois <- do_galois_lattice(M)
Galois <- do_full_label(Galois, M)
Galois <- do_reduced_label(Galois, M)




cleanEx()
nameEx("galois_layout")
### * galois_layout

flush(stderr()); flush(stdout())

### Name: galois_layout
### Title: Layout for plotting a Galois lattice
### Aliases: galois_layout

### ** Examples

M=matrix(c(1,1,1,0,0,0,
0,0,0,1,1,1,
1,0,0,1,0,0,
1,1,0,1,0,1),nrow=6)
colnames(M) <- c("A", "B", "C", "D")
rownames(M) <- as.character(1:6)
Galois <- do_galois_lattice(M)
Galois <- do_full_label(Galois, M)
Galois <- do_reduced_label(Galois, M)
plot(Galois, layout = galois_layout(Galois))





### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
