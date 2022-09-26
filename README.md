# Save `BumpyMatrix` objects to file

The **alabaster.bumpy** package implements methods for saving and loading [`BumpyMatrix`](https://bioconductor.org/packages/BumpyMatrix) objects under the **alabaster** framework.
It provides a language-agnostic method for serializing data in arrays or abstractions thereof.
To get started, install the package and its dependencies from GitHub:

```r
devtools::install_github("ArtifactDB/alabaster.schemas")
devtools::install_github("ArtifactDB/alabaster.base")
devtools::install_github("ArtifactDB/alabaster.bumpy")
```

Let's create a `BumpyMatrix` object:

```r
library(BumpyMatrix)
library(S4Vectors)
df <- DataFrame(x=runif(100), y=runif(100))
f <- factor(sample(letters[1:20], nrow(df), replace=TRUE), letters[1:20])
mat <- BumpyMatrix(split(df, f), c(5, 4))
```

Saving it to file involves calling `stageObject`:

```{r}
library(alabaster.bumpy)
tmp <- tempfile()
dir.create(tmp)
meta <- stageObject(mat, tmp, "bumpy")
meta[["$schema"]]
## [1] "bumpy_data_frame_matrix/v1.json"
```

```{r}
roundtrip <- loadObject(meta, tmp)
class(roundtrip)
## [1] "BumpyDataFrameMatrix"
## attr(,"package")
## [1] "BumpyMatrix"
```

