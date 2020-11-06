#' Update input
#'
#' bioBakeryR workflow takes input fastq file information as a file path (in Google bucket)
#' with a list of all of the read1 files. This file must have the full paths to all of the
#' files and is only expected to include the read1 files (not those for read2). The names
#' for each of the samples will be computed based on the read pair identifier and the input
#' file extension provided. For example a file named SAMPLE1.R1.fastq.gz would have a sample
#' name of "SAMPLE1", a read1 identifier of ".R1". and an extension of ".fastq.gz".
#' It is expected that each sample with have two files (one file for each read of the pair).
#'
#' @import AnVIL
#' @import httr
#'
#' @param accountEmail Email linked to Terra account
#' @param inputPath A file path (in Google bucket) with a list of all of the read1 files.
#' @param inputMetadataPath A file path (in Google bucket) with a metadata table
#' @param projectName Name of the billing project
#' @param workspaceName Name of the workspace
#'
#' @examples
#' input <- "gs://fc-07ee4ddc-5b5b-46f6-bed7-809aa14bb012/IBDMDB/ibdmdb_file_list.txt"
#' inputMeta <- "gs://fc-07ee4ddc-5b5b-46f6-bed7-809aa14bb012/IBDMDB/ibdmdb_demo_metadata.txt"
#' updateInput(accountEmail = "shbrief@gmail.com",
#'             inputPath = input,
#'             inputMetadataPath = inputMeta,
#'             projectName = "waldronlab-terra-rstudio",
#'             workspaceName = "mtx_workflow_biobakery_ver3")
#'
updateInput <- function(accountEmail, inputPath, inputMetadataPath,
                        projectName, workspaceName) {
    gcloud_account <- accountEmail
    terra <- Terra()

    dir <- system.file("extdata", package = "bioBakeryR")
    inputJson <- RJSONIO::fromJSON(file.path(dir, "wtx.json"))
    inputJson$workflowMTX.inputRead1Files <- inputPath
    if (exists(inputMetadataPath)) {
        inputJson$workflowMTX.inputMetadataFile <- inputMetadataPath
    }

    terra$overwriteWorkspaceMethodConfig(  ##################### issue with 2 same argument names
        workspaceNamespace = projectName,
        workspaceName = workspaceName,
        configNamespace = "mtx_workflow_biobakery_version3",
        configName = "mtx_workflow_biobakery_version3",
        inputs = as.list(inputJson)
    )
}
