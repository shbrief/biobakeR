#' Update input
#'
#' bioBakeryR workflow takes input fastq file information as a file path (in
#' Google bucket) with a list of all of the read1 files. This file must have
#' the full paths to all of the files and is only expected to include the read1
#' files (not those for read2). The names for each of the samples will be
#' computed based on the read pair identifier and the input file extension
#' provided. For example a file named SAMPLE1.R1.fastq.gz would have a sample
#' name of "SAMPLE1", a read1 identifier of ".R1". and an extension of
#' ".fastq.gz". It is expected that each sample with have two files (one file
#' for each read of the pair).
#'
#' To generate a file to use as input for InputRead1Files, follow the
#' [Terra instructions](https://support.terra.bio/hc/en-us/articles/360033353952-Creating-a-list-file-of-reads-for-input-to-a-workflow),
#' adding to command #2 the InputRead1Identifier and the InputExtension. For
#' example with InputRead1Identifier = ".R1" and InputExtension = ".fastq.gz"
#' command #2 would now be \code{gsutil ls gs:/your_data_Google_bucket_id/ | grep ".fastq.gz" | grep ".R1" > ubams.list}.
#' Also since for this workflow we are looking for fastq or fastq.gz input
#' files you might change the name of the file list in this command from
#' \code{ubams.list} to \code{fastq_list.txt}.
#'
#' @import AnVIL
#' @import httr
#' @import RunTerraWorkflow
#' @importFrom jsonlite unbox
#'
#' @param workspaceName Name of the workspace
#' @param ProjectName The name of the sequencing project. The final output
#' report and zip archive will use this name (only alphanumeric characters
#' allowed).
#' @param InputRead1Files A file path (in google bucket) with a list of all of
#' the read1 files. This file must have the full paths to all of the files and
#' is only expected to include the read1 files (not those for read2). The names
#' for each of the samples will be computed based on the read pair identifier
#' and the input file extension provided. For example a file named
#' \code{SAMPLE1.R1.fastq.gz} would have a sample name of "SAMPLE1", a read1
#' identifier of ".R1". and an extension of ".fastq.gz". It is expected that
#' each sample with have two files (one file for each read of the pair).
#' @param InputMetadataFile (optional) A file path (in google bucket) with a
#' metadata table. This file is used with the visualization task to annotate
#' the figures with metadata. Default is \code{NULL}.
#' @param AdapterType The type of adapter to filter. Available options are
#' "NexteraPE", "TruSeq2", and "TruSeq3".
#' @param InputExtension The extension for all of the input files. Default is
#' \code{.fastq.gz}.
#' @param InputRead1Identifier The identifier in the file name for those files
#' that are read1. Default is \code{.R1}.
#' @param InputRead2Identifier The identifier in the file name for those files
#' that are read2. Default is \code{.R2}.
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#'
#' @export
biobakery_updateInput <- function(workspaceName,
                                  ProjectName,
                                  InputRead1Files,
                                  InputMetadataFile = NULL,
                                  AdapterType = "NexteraPE",
                                  InputExtension = ".fastq.gz",
                                  InputRead1Identifier = "_R1",
                                  InputRead2Identifier = "_R2",
                                  accountEmail = gcloud_account(), 
                                  billingProjectName = gcloud_project()) {
    ## Setup gcloud account/project
    RunTerraWorkflow::setCloudEnv(accountEmail = accountEmail, 
                                  billingProjectName = billingProjectName,
                                  message = FALSE)
    rawls <- Rawls()

    ## Import template
    dir <- system.file("extdata", package = "biobakeR")
    template <- readRDS(file.path(dir, "wtx_input_template.rds"))

    ## Update inputs
    makeString <- function(x) paste0('"', x, '"')
    template$workflowMTX.AdapterType <- makeString(AdapterType)
    template$workflowMTX.ProjectName <- makeString(ProjectName)
    template$workflowMTX.inputExtension <- makeString(InputExtension)
    template$workflowMTX.inputRead1Identifier <- makeString(InputRead1Identifier)
    template$workflowMTX.inputRead2Identifier <- makeString(InputRead2Identifier)
    template$workflowMTX.inputRead1Files <- makeString(InputRead1Files)

    ## Format inputs
    inputList <- jsonlite::unbox(as.data.frame(template))

    ## Update metadata
    if (!is.null(InputMetadataFile)) {
        template$workflowMTX.InputMetadataFile <- InputMetadataFile
    }

    ## Other metadata - not for update
    currentInput <- RunTerraWorkflow::currentInput(workspaceName,
                                                   inputOnly = FALSE)
    methodRepoMethod <- jsonlite::unbox(as.data.frame(currentInput$methodRepoMethod))
    methodConfigVersion <- jsonlite::unbox(currentInput$methodConfigVersion)
    deleted <- jsonlite::unbox(currentInput$deleted)

    ## API call
    biobakeR_name <- "mtx_workflow_biobakery_version3"
    resp <- rawls$overwrite_method_configuration(
        workspaceNamespace = billingProjectName,
        workspaceName = workspaceName,
        configNamespace = biobakeR_name,
        configName = biobakeR_name,
        .__body__ = list(namespace = biobakeR_name,
                         name = biobakeR_name,
                         inputs = inputList,
                         outputs = empty_object,
                         methodRepoMethod = methodRepoMethod,
                         methodConfigVersion = methodConfigVersion,
                         deleted = deleted)
    )

    ## API call status
    if (resp$status_code == 200) {
        "Input information is succesfully updated."
    } else {"Updating inputs is failed."}
}
