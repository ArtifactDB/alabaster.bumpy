#' Save a BumpyDataFrameMatrix to disk
#'
#' Save a \linkS4class{BumpyDataFrameMatrix} to its on-disk representation.
#' 
#' @param x A BumpyDataFrameMatrix object.
#' @inheritParams alabaster.base::saveObject
#' 
#' @return \code{x} is saved into \code{path} and \code{NULL} is invisibly returned.
#'
#' @author Aaron Lun
#' @examples
#' # Mocking up a BumpyMatrix.
#' library(BumpyMatrix)
#' library(S4Vectors)
#' df <- DataFrame(x=runif(100), y=runif(100))
#' f <- factor(sample(letters[1:20], nrow(df), replace=TRUE), letters[1:20])
#' out <- S4Vectors::split(df, f)
#' mat <- BumpyMatrix(out, c(5, 4))
#'
#' # Saving it:
#' tmp <- tempfile()
#' saveObject(mat, tmp)
#' 
#' @export
#' @rdname saveBumpyDataFrameMatrix
#' @aliases stageObject,BumpyDataFrameMatrix-method
#' @importFrom BumpyMatrix undim
#' @importFrom Matrix which
#' @import methods rhdf5 alabaster.base
setMethod("saveObject", "BumpyDataFrameMatrix", function(x, path, ...) {
    dir.create(path, showWarnings=FALSE)

    ud <- undim(x)
    tryCatch({
        altSaveObject(unlist(ud, use.names=FALSE), path=file.path(path, "concatenated"), ...)
    }, error=function(e) {
        stop("failed to stage the underlying DataFrame in a ", class(x)[1], "\n  - ", e$message)
    })

    fhandle <- H5Fcreate(file.path(path, "partitions.h5"))
    on.exit(H5Fclose(fhandle), add=TRUE, after=FALSE)
    ghandle <- H5Gcreate(fhandle, "bumpy_data_frame_array")
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)

    h5_write_vector(ghandle, "dimensions", x=dim(x), type="H5T_NATIVE_UINT32")
    h5_write_vector(ghandle, "lengths", x=lengths(ud), type="H5T_NATIVE_UINT32")

    if (length(ud) != prod(dim(x))) { # i.e., it's sparse.
        nonzero <- which(x@proxy != 0, arr.ind = TRUE)
        ihandle <- H5Gcreate(ghandle, "indices")
        on.exit(H5Gclose(ihandle), add=TRUE, after=FALSE)
        h5_write_vector(ihandle, "0", x=nonzero[,1] - 1L, type="H5T_NATIVE_UINT32")
        h5_write_vector(ihandle, "1", x=nonzero[,2] - 1L, type="H5T_NATIVE_UINT32")
    }

    nm <- dimnames(x)
    if (!is.null(nm) && !all(vapply(nm, is.null, TRUE))) {
        nhandle <- H5Gcreate(ghandle, "names")
        on.exit(H5Gclose(nhandle), add=TRUE, after=FALSE)
        for (i in seq_along(nm)) {
            if (!is.null(nm[[i]])) {
                h5_write_vector(nhandle, as.character(i-1L), x=nm[[i]])
            }
        }
    }

    saveObjectFile(path, "bumpy_data_frame_array", list(bumpy_data_frame_array=list(version="1.0")))
    invisible(NULL)
})

##########################
##### OLD STUFF HERE #####
##########################

#' @export
#' @importFrom S4Vectors DataFrame
setMethod("stageObject", "BumpyDataFrameMatrix", function(x, dir, path, child=FALSE) {
    dir.create(file.path(dir, path))

    df.meta <- tryCatch({
        meta <- (function() {
            if (useBumpyHDF5()) {
                prev <- .saveDataFrameFormat("hdf5")
                on.exit(.saveDataFrameFormat(prev))
            }
            .stageObject(unlist(x, use.names=FALSE), dir=dir, path=file.path(path, "concatenated"))
        })()
        .writeMetadata(meta, dir=dir)
    }, error=function(e) {
        stop("failed to stage the underlying DataFrame in a ", class(x)[1], "\n  - ", e$message)
    })

    meta <- list()
    all.dims <- dimnames(x)
    if (!is.null(all.dims)) {
        names(all.dims) <- c("row", "column")
        for (d in 1:2) {
            if (!is.null(all.dims[[d]])) {
                p <- file.path(path, names(all.dims)[d])
                n <- paste0(names(all.dims)[d], "_names")
                df <- DataFrame(X=all.dims[[d]])
                colnames(df) <- n

                name.meta <- tryCatch({
                    dn.meta <- .stageObject(df, dir=dir, path=p)
                    .writeMetadata(dn.meta, dir=dir)
                }, error=function(e) {
                    stop("failed to stage ", sub("_", " ", n), " in a ", class(x)[1], "\n  - ", e$message)
                })
                meta[[n]] <- list(resource=name.meta)
            }
        }
    }

    # Actually writing the groupings.
    current <- which(x@proxy != 0, arr.ind = TRUE)
    dfl <- undim(x)
    coords <- data.frame(row = current[,1], column = current[,2], number = lengths(dfl))
    meta$object_count <- nrow(coords)

    if (!is.null(names(dfl))) {
        coords <- cbind(data.frame(names=names(dfl)), coords)
        meta$object_names <- TRUE
    }

    gpath <- file.path(path, "groups.csv.gz")
    .quickWriteCsv(coords, path=file.path(dir, gpath))
    meta$compression <- "gzip"

    list(
        `$schema`="bumpy_data_frame_matrix/v1.json",
        path=gpath,
        is_child=child,
        `array`=list(
            dimensions=as.list(dim(x))
        ),
        bumpy_matrix=meta,
        bumpy_data_frame_matrix=list(
            concatenated=list(resource=df.meta)
        )
    )
})

