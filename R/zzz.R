.onLoad <- function(libname, pkgname) {
    registerReadObjectFunction("bumpy_data_frame_array", readBumpyDataFrameMatrix)
}

.onUnload <- function(libname, pkgname) {
    registerReadObjectFunction("bumpy_data_frame_array", NULL)
}
