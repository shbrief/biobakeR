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
#' To generate a file to use as input for InputRead1Files, follow the [Terra instructions](https://support.terra.bio/hc/en-us/articles/360033353952-Creating-a-list-file-of-reads-for-input-to-a-workflow),
#' adding to command #2 the InputRead1Identifier and the InputExtension. For example
#' with InputRead1Identifier = ".R1" and InputExtension = ".fastq.gz" command #2 would
#' now be \code{gsutil ls gs:/your_data_Google_bucket_id/ | grep ".fastq.gz" | grep ".R1" > ubams.list}.
#' Also since for this workflow we are looking for fastq or fastq.gz input files you might
#' change the name of the file list in this command from \code{ubams.list} to \code{fastq_list.txt}.
#'
#' @import AnVIL
#' @import httr
#' @importFrom jsonlite unbox
#'
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#' @param workspaceName Name of the workspace
#' @param AdapterType The type of adapter to filter. Available options are
#' "NexteraPE", "TruSeq2", and "TruSeq3".
#' @param ProjectName The name of the sequencing project. The final output report
#' and zip archive will use this name (only alphanumeric characters allowed).
#' @param InputExtension The extension for all of the input files. Default is \code{.fastq.gz}.
#' @param InputRead1Identifier The identifier in the file name for those files
#' that are read1. Default is \code{.R1}.
#' @param InputRead2Identifier The identifier in the file name for those files
#' that are read2. Default is \code{.R2}.
#' @param InputRead1Files A file path (in google bucket) with a list of all of
#' the read1 files. This file must have the full paths to all of the files and
#' is only expected to include the read1 files (not those for read2). The names
#' for each of the samples will be computed based on the read pair identifier and
#' the input file extension provided. For example a file named \code{SAMPLE1.R1.fastq.gz}
#' would have a sample name of "SAMPLE1", a read1 identifier of ".R1". and an
#' extension of ".fastq.gz". It is expected that each sample with have two files
#' (one file for each read of the pair).
#' @param InputMetadataFile (optional) A file path (in google bucket) with a metadata
#' table. This file is used with the visualization task to annotate the figures with
#' metadata. Default is \code{NULL}.
#'
#' @examples
#' input <- "gs://fc-07ee4ddc-5b5b-46f6-bed7-809aa14bb012/IBDMDB/ibdmdb_file_list.txt"
#' inputMeta <- "gs://fc-07ee4ddc-5b5b-46f6-bed7-809aa14bb012/IBDMDB/ibdmdb_demo_metadata.txt"
#' updateInput(accountEmail = "shbrief@gmail.com",
#'             billingProjectName = "waldronlab-terra-rstudio",
#'             workspaceName = "mtx_workflow_biobakery_ver3",
#'             AdapterType = "NexteraPE",
#'             ProjectName = "ibdmdb_test",
#'             InputExtension = ".fastq.gz",
#'             InputRead1Identifier = "_R1",
#'             InputRead2Identifier = "_R2",
#'             InputRead1Files = input,
#'             InputMetadataFile = inputMeta)
#'
#'
updateInput <- function(accountEmail, billingProjectName, workspaceName,
                        AdapterType,
                        ProjectName,
                        InputExtension,
                        InputRead1Identifier,
                        InputRead2Identifier,
                        InputRead1Files,
                        InputMetadataFile = NULL) {
    gcloud_account(accountEmail)
    gcloud_project(billingProjectName)
    terra <- Terra()

    required <- c(AdapterType,
                  ProjectName,
                  InputExtension,
                  InputRead1Identifier,
                  InputRead2Identifier,
                  InputRead1Files)
    missed <- lapply(required, missing)
    if (any(missed)) {stop("Required input(s) are missing.")}

    ## Import input template
    dir <- system.file("extdata", package = "bioBakeryR")
    inputJson <- RJSONIO::fromJSON(file.path(dir, "wtx.json"))

    ## Update inputs
    inputJson$workflowMTX.AdapterType <- AdapterType
    inputJson$workflowMTX.ProjectName <- ProjectName
    inputJson$workflowMTX.InputExtension <- InputExtension
    inputJson$workflowMTX.InputRead1Identifier <- InputRead1Identifier
    inputJson$workflowMTX.InputRead2Identifier <- InputRead2Identifier
    inputJson$workflowMTX.inputRead1Files <- InputRead1Files

    ## Update metadata
    if (!is.null(InputMetadataFile)) {
        inputJson$workflowMTX.InputMetadataFile <- InputMetadataFile
    }

    terra$overwriteWorkspaceMethodConfig(  ##################### issue with 2 same argument names
        workspaceNamespace = billingProjectName,
        workspaceName = workspaceName,
        configNamespace = "",
        configName = "",
        namespace = "mtx_workflow_biobakery_version3",
        name = "mtx_workflow_biobakery_version3",
        inputs = as.list(inputJson),
        workspaceName = empty_object()
    )
}
