hdf5.env <- new.env()
hdf5.env$use <- FALSE 

#' Save BumpyMatrix data to HDF5
#'
#' Use HDF5 for the underlying data frame, i.e., obtained after \code{\link{unlist}}ing the \linkS4class{BumpyMatrix}.
#' This is less intuitive than a CSV but preserves the precision of floating-point numbers.
#'
#' @param use Logical scalar indicating whether to save in HDF5.
#'
#' @return 
#' If \code{use} is missing, a logical scalar is returned indicating whether data should be saved in HDF5.
#'
#' If \code{use} is provided, it is used to set the corresponding flag globally.
#' The previous value of the flag is returned invisibly.
#'
#' @examples
#' useBumpyHDF5()
#'
#' old <- useBumpyHDF5(FALSE)
#' useBumpyHDF5()
#'
#' # Setting it back.
#' useBumpyHDF5(old)
#' @export
useBumpyHDF5 <- function(use) {
    previous <- hdf5.env$use
    if (!missing(use)) {
        hdf5.env$use <- use
        invisible(previous)
    } else {
        previous
    }
}
