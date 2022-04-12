#' Check the current input arguments
#'
#' @import RunTerraWorkflow
#' @import AnVIL
#'
#' @param workspaceName Name of the workspace
#' @param allConfig Under the default (\code{FALSE}), the file path to the
#' input files list and the paths to input files will be returned. If it's
#' set to \code{TRUE}, the whole method configuration will be returned.
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#'
#' @export
biobakery_currentInput <- function(workspaceName,
                                   allConfig = FALSE,
                                   accountEmail = gcloud_account(),
                                   billingProjectName = gcloud_project()) {

    ## Setup gcloud account/project
    RunTerraWorkflow::setCloudEnv(accountEmail = accountEmail,
                                  billingProjectName = billingProjectName,
                                  message = FALSE)

    parsed <- RunTerraWorkflow::currentInput(workspaceName, inputOnly = FALSE)

    ## Return the whole method configuration
    if (isTRUE(allConfig)) {
        return(parsed)
    } else {
        ## Return only the input part of the method configuration
        res <- parsed$inputs$workflowMTX.inputRead1Files
        if (nzchar(res)) {
            res <- gsub("\"", "", res)
            structure(list(inputListPath = res,
                           inputFilePath = AnVIL::gsutil_cat(res)))
        } else {
            return("Input files are not provided.")
        }
    }
}
