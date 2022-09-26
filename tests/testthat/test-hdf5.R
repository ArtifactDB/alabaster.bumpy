# This tests the saving and loading of content in and out of HDF5
# library(testthat); library(alabaster.bumpy); source("setup.R"); source("test-hdf5.R")

library(alabaster.base)
library(BumpyMatrix)
library(S4Vectors)

df <- DataFrame(x=runif(100), y=runif(100))
f <- factor(sample(letters[1:20], nrow(df), replace=TRUE), letters[1:20])
out <- S4Vectors::split(df, f)
mat <- BumpyMatrix(out, c(5, 4))
useBumpyHDF5(TRUE)

test_that("staging and loading works with a HDF5 backend", {
    tmp <- tempfile()
    dir.create(tmp)
    meta <- stageObject(mat, tmp, "bumpy")
    expect_identical(as.character(meta[["$schema"]]), "bumpy_data_frame_matrix/v1.json")

    meta.path <- meta$bumpy_data_frame_matrix$concatenated$resource$path
    expect_match(meta.path, ".h5$")
    meta.path <- paste0(meta.path, ".json")

    meta2 <- jsonlite::fromJSON(file.path(tmp, meta.path), simplifyVector=FALSE)
    X <- loadDataFrame(meta2, tmp)
    expect_equal(X, unlist(mat, use.names=FALSE))

    restored <- loadBumpyDataFrameMatrix(meta, tmp)
    expect_identical(mat, restored)
})

