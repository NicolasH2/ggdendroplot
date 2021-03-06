# ggdendroplot
An [R](https://www.r-project.org) package that draws highly modifiable dendrograms in [ggplot2](https://ggplot2.tidyverse.org/). The dendrogram can easily be modified and added to an existing ggplot object. ggdendroplot takes as an input the output of the R [stats](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/stats-package.html) function hclust(). It vizualizes the clustering using ggplot2's geom_path layers.

# Installation
Install the ggdendroplot package from the git repository:
``` r
devtools::install_github("solatar/ggdendroplot")
```

# Default dendrogram
We build a random example matrix, called df. We use the functions dist and hclust from base R to get hclust objects. We cluster rows (rowclus) and columns (colclus) individually. You can change the distance matrix and also clustering algorithm by checking out the respective functions' help pages (?dist and ?hclust).
Then we can directly take one of these clusterings and vizualize a dendrogram from it.
``` r
library(ggdendroplot)
library(ggplot2)

#a test data.frame, with columns drawing values from 2 different standard distributions
df <- matrix(c(rnorm(64, mean=0), rnorm(64, mean=1)), ncol = 8, dimnames=list(
  rownames=paste0("trait",seq(16)),
  colnames=paste0("sample",seq(8))
))

#perform hierarchical clustering
rowclus <- hclust(dist( df ))    #cluster the rows
colclus <- hclust(dist( t(df) )) #cluster the columns

ggplot() + geom_dendro(colclus)
```
<img src="readme_files/dendro_down.png"/>

Often, we dont't just want a dendrogram, but also a heatmap. ggdendroplot provides the function hmReady, which takes the original table and the clustering you made. It uses [reshape2](https://cran.r-project.org/web/packages/reshape2/index.html) to output a ready-to-plot data.frame. This data.frame has columns x and y for coordinates, and a value column for the color in the heatmap. It also has the columns rowid and variable, which contain the row and column names of the original table. We can supply colclus or rowclus or both to get a dataframe that is clustered accordingly.

Here we only use the column clustering (colclus) as a simple example.

``` r
hm <- hmReady(df, colclus=colclus, rowclus=rowclus)

hmplot <- ggplot() + 
  geom_tile(data=hm, aes(x=x, y=y, fill=value)) +
  theme(axis.text.x=element_text(angle=45, hjust=1))

print(hmplot)
```
<img src="readme_files/dendro_heatmap.png"/>

When we simply add the dendrogram to the plot, we see that it is not in the correct place. We can move it up by specifying the ylim arguement.
``` r
hmplot + geom_dendro(colclus)
hmplot + geom_dendro(colclus, ylim=c(17,20))
```
<img src="readme_files/dendro_heatmap23.png"/>

We can add a second dendrogram that shows the clustering of the rows. For that we have to speficy that it is pointing sideways.
``` r
hmplot + 
  geom_dendro(colclus, ylim=c(17, 20)) +
  geom_dendro(rowclus, xlim=c(8.5, 10), pointing="side")
```
<img src="readme_files/dendro_heatmap4.png"/>

# Custom dendrogram

Reverse the order or direction. This happens when you set the limits so that the first limit number (here: 3) is higher than the second (here: 0).
In the following example case we reverse the order because we change xlim, while the pointing arguement is in its default state "updown" (you would also achieve a order reversal by changing ylim while pointing="side"). Note that now, the dendrogram will not line up with your heatmap and will give you a false impression, which is why this reversal is only possible when you set the failsafe arguement to FALSE.
``` r
ggplot() + geom_dendro(colclus, xlim=c(3,0), failsafe=FALSE)
```
<img src="readme_files/dendro_down_flipped.png"/>

When we change ylim while pointing=updown", we reverse the direction, which is less problematic (the same for changing xlim while pointing="side").
``` r
ggplot() + geom_dendro(colclus, ylim=c(3,0))
```
<img src="readme_files/dendro_up.png"/>

You can disable that geom_dendro displays the sample names:
``` r
ggplot() + geom_dendro(colclus, axis.labels = F)
```
<img src="readme_files/dendro_nolabels.png"/>

You can change the dendrogram in the same way that you would also change a geom_path object. Specifically you can change color, size, linetype and lineend. 
Possible options for linetype are: solid (default), dotted, dotdash, twodash, dashed, longdash, blank.
``` r
ggplot() + geom_dendro(colclus, size=2, color="blue", linetype="dashed")
```
<img src="readme_files/dendro_custom.png"/>

The lineend arguement introduces suttle changes, effecting only how the ends of the lines look.
Possible options are: butt (default), square, round.
``` r
ggplot() + geom_dendro(colclus, size=4, lineend="round")
```
<img src="readme_files/dendro_custom2.png"/>
