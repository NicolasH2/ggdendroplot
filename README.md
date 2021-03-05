# ggdendroplot
Draws highly modifiable dendrograms for [ggplot2](https://ggplot2.tidyverse.org/). ggdendroplot takes a data.frame or matrix as an input, where the columns are to be clustered (by their rows). ggdendroplot then uses the dist and hclust function from R stats to generate a cluster matrix and an object that contains data about the hierarchical clustering. The data is then vizualized using ggplot2's geom_path layer.

# Installation
Install the ggdendroplot package from the git repository:
``` r
devtools::install_github("solatar/dendroplot")
```

# Default dendrogram
Load the package, create your first brace in ggplot:
``` r
library(ggbrace)
library(ggplot2)

#a test data.frame
df <- matrix(rnorm(128), ncol = 8)
colnames(df) <- paste0("a",seq(ncol(df)))

ggplot() + geom_dendro(df)
```
<img src="readme_files/dendro_down.png"/>

Change the order:
``` r
ggplot() + geom_dendro(df, xlim=c(3,0))
```
<img src="readme_files/dendro_down_flipped.png"/>

Change the orientation:
``` r
ggplot() + geom_dendro(df, pointing="side")
```
<img src="readme_files/dendro_left.png"/>

Change the placement by defining xlim and ylim. With this you can also invert the graph if the first number of xlim or ylim is higher than the second:
``` r
ggplot() + geom_dendro(df, ylim=c(3,0))
```
<img src="readme_files/dendro_up.png"/>

# Custom dendrogram
You can change the dendrogram in the same way that you would also change a geom_path object. Specifically you can change color, size, linetype and lineend. 
Possible options for linetype are: solid (default), dotted, dotdash, twodash, dashed, longdash, blank.
``` r
ggplot() + geom_dendro(df, size=2, color="blue", linetype="dashed")
```
<img src="readme_files/dendro_custom.png"/>

The lineend arguement introduces suttle changes, effecting only how the ends of the lines look.
Possible options are: butt (default), square, round.
``` r
ggplot() + geom_dendro(df, size=4, lineend="round")
```
<img src="readme_files/dendro_custom2.png"/>
