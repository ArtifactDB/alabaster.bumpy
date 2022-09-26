#' Stage BumpyDataFrameMatrix objects
#'
#' Staging method for \linkS4class{BumpyDataFrameMatrix} objects.
#' See \code{?\link{stageObject}} for more details.
#' 
#' @param x A BumpyDataFrameMatrix object.
#' @inheritParams alabaster.base::stageObject
#' 
#' @return
#' A named list of metadata, to be used by \code{\link{.writeMetadata}}.
#'
#' @author Aaron Lun
#' 
#' @seealso
#' \code{\link{useBumpyHDF5}}, to control whether the concatenated data frame is saved as a CSV or HDF5 file.
#' @export
#' @rdname stageMatrix-BumpyDataFrameMatrix
#' @importFrom BumpyMatrix undim
#' @importFrom S4Vectors DataFrame
#' @importFrom alabaster.base .saveDataFrameFormat stageObject .stageObject .quickWriteCsv .writeMetadata
#' @importFrom Matrix which
#' @import methods
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
            dimensions=dim(x)
        ),
        bumpy_matrix=meta,
        bumpy_data_frame_matrix=list(
            concatenated=list(resource=df.meta)
        )
    )
})

