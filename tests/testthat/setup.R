round_trip_se <- function(se) {
    mae <- MultiAssayExperiment(list(rnaseq=se), sampleMap=DataFrame(assay="rnaseq", primary=colnames(se), colname=colnames(se)))

    mae <- annotateDataset(mae,
        title="This is my MultiAssayExperiment",
        description="There are many others like it. But this is mine.",
        authors=c("luna", "michafla")
    )

    tmp <- tempfile()
    saveDataset(mae, dir=tmp, stage.only=TRUE)
    meta <- jsonlite::fromJSON(file.path(tmp, "experiment-1", "experiment.json"), simplifyVector=FALSE)
    loadExperiment(meta, dir=tmp)
}
