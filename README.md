# Save `BumpyMatrix` objects to file

|Environment|Status|
|---|---|
|[BioC-release](https://bioconductor.org/packages/release/bioc/html/alabaster.bumpy.html)|[![Release OK](https://bioconductor.org/shields/build/release/bioc/alabaster.bumpy.svg)](http://bioconductor.org/checkResults/release/bioc-LATEST/alabaster.bumpy/)|
|[BioC-devel](https://bioconductor.org/packages/devel/bioc/html/alabaster.bumpy.html)|[![Devel OK](https://bioconductor.org/shields/build/devel/bioc/alabaster.bumpy.svg)](http://bioconductor.org/checkResults/devel/bioc-LATEST/alabaster.bumpy/)|


The **alabaster.bumpy** package implements methods for saving and loading [`BumpyMatrix`](https://bioconductor.org/packages/BumpyMatrix) objects under the **alabaster** framework.
It provides a language-agnostic method for serializing data in bumpy matrices, most typically as assays of a `SummarizedExperiment`. 
To get started, install the package and its dependencies from [Bioconductor](https://bioconductor.org/packages/alabaster.bumpy):

```r
# install.packages("BiocManager")
BiocManager::install("alabaster.bumpy")
```

Let's create a `BumpyMatrix` object:

```r
library(BumpyMatrix)
library(S4Vectors)
df <- DataFrame(x=runif(100), y=runif(100))
f <- factor(sample(letters[1:20], nrow(df), replace=TRUE), letters[1:20])
mat <- BumpyMatrix(split(df, f), c(5, 4))
```

We save it to file:

```{r}
library(alabaster.bumpy)
tmp <- tempfile()
saveObject(mat, tmp)
```

And then load it back:

```{r}
roundtrip <- readObject(tmp)
class(roundtrip)
## [1] "BumpyDataFrameMatrix"
## attr(,"package")
## [1] "BumpyMatrix"
```

