# This tests the BumpyDataFrameMatrix loader.
# library(testthat); library(alabaster.bumpy); source("setup.R"); source("test-BumpyDataFrameMatrix.R")

library(BumpyMatrix)
library(alabaster.base)
library(S4Vectors)
df <- DataFrame(x=runif(100), y=runif(100))
f <- factor(sample(letters[1:20], nrow(df), replace=TRUE), letters[1:20])
out <- S4Vectors::split(df, f)
mat <- BumpyMatrix(out, c(5, 4))
useBumpyHDF5(FALSE)

test_that("staging a BumpyDataFrameMatrix works as expected", {
    tmp <- tempfile()
    dir.create(tmp)

    meta <- stageObject(mat, tmp, "bumpy")

    expect_identical(as.character(meta[["$schema"]]), "bumpy_data_frame_matrix/v1.json")
    X <- read.csv(file.path(tmp, meta$bumpy_data_frame_matrix$concatenated$resource$path))
    expect_equal(X, as.data.frame(unlist(mat, use.names=FALSE)))

    Y <- read.csv(file.path(tmp, meta$path))
    expect_equal(Y$number, as.vector(lengths(mat)))

    expect_true(is.null(meta$bumpy_matrix$row_names))
    expect_true(is.null(meta$bumpy_matrix$column_names))

    # Writing the metadata works as expected.
    expect_error(.writeMetadata(meta, dir=tmp), NA)
    expect_true(file.exists(file.path(tmp, "bumpy/groups.csv.gz.json")))

    roundtrip <- loadBumpyDataFrameMatrix(meta, tmp)
    expect_equal(mat, roundtrip)
})

test_that("staging a BumpyDataFrameMatrix with names works as expected", {
    dimnames(mat) <- list(LETTERS[1:5], 4:1)

    tmp <- tempfile()
    dir.create(tmp)
    meta <- stageObject(mat, tmp, "bumpy")
    .writeMetadata(meta, tmp)

    expect_true(!is.null(meta$bumpy_matrix$row_names))
    expect_identical(rownames(mat), read.csv(file.path(tmp, meta$bumpy_matrix$row_names$resource$path))[,1])

    expect_true(!is.null(meta$bumpy_matrix$column_names))
    expect_identical(colnames(mat), as.character(read.csv(file.path(tmp, meta$bumpy_matrix$column_names$resource$path))[,1]))

    roundtrip <- loadBumpyDataFrameMatrix(meta, tmp)
    expect_equal(mat, roundtrip)
})

test_that("staging a BumpyDataFrameMatrix with internal names works as expected", {
    names(out) <- sprintf("Object%s", seq_along(out))
    mat <- BumpyMatrix(out, c(5, 4))

    tmp <- tempfile()
    dir.create(tmp)
    meta <- stageObject(mat, tmp, "bumpy")
    .writeMetadata(meta, tmp)

    stuff <- read.csv(file.path(tmp, "bumpy/groups.csv.gz"))
    expect_identical(stuff[,1], names(out))

    roundtrip <- loadBumpyDataFrameMatrix(meta, tmp)
    expect_equal(mat, roundtrip)
})

proxy <- Matrix::sparseMatrix(i=1:5, j=1:5, x=1:5)
smat <- BumpyMatrix(out[1:5], proxy=proxy)

test_that("staging a sparse BumpyDataFrameMatrix works as expected", {
    tmp <- tempfile()
    dir.create(tmp)
    meta <- stageObject(smat, tmp, "bumpy")
    .writeMetadata(meta, tmp)

    expect_identical(as.character(meta[["$schema"]]), "bumpy_data_frame_matrix/v1.json")
    X <- read.csv(file.path(tmp, meta$bumpy_data_frame_matrix$concatenated$resource$path))
    expect_equal(X, as.data.frame(unlist(smat, use.names=FALSE)))

    Y <- read.csv(file.path(tmp, meta$path))
    expect_equal(Y$number, unname(lengths(out[1:5])))

    # Writing the metadata works as expected.
    expect_error(.writeMetadata(meta, dir=tmp), NA)
    expect_true(file.exists(file.path(tmp, "bumpy/groups.csv.gz.json")))

    roundtrip <- loadBumpyDataFrameMatrix(meta, tmp)
    expect_equal(smat, roundtrip)
})
