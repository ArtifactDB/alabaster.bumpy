# library(testthat); library(alabaster.bumpy); source("setup.R"); source("test-BumpyDataFrameMatrix.R")

library(BumpyMatrix)
library(alabaster.base)
library(S4Vectors)
df <- DataFrame(x=runif(100), y=runif(100))
f <- factor(sample(letters[1:20], nrow(df), replace=TRUE), letters[1:20])
out <- S4Vectors::unname(S4Vectors::split(df, f))
mat <- BumpyMatrix(out, c(5, 4))

test_that("saving a BumpyDataFrameMatrix works as expected", {
    tmp <- tempfile()
    dir.create(tmp)

    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_equal(readObject(tmp), mat)
})

test_that("saving a BumpyDataFrameMatrix with names works as expected", {
    dimnames(mat) <- list(LETTERS[1:5], 4:1)

    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_equal(readObject(tmp), mat)
})

test_that("saving a sparse BumpyDataFrameMatrix works as expected", {
    proxy <- Matrix::sparseMatrix(i=1:5, j=1:5, x=1:5)
    smat <- BumpyMatrix(out[1:5], proxy=proxy)

    tmp <- tempfile()
    saveObject(smat, tmp)
    expect_equal(readObject(tmp), smat)
})
