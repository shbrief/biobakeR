#' Check the current input arguments
#'
#' @import RunTerraWorkflow
#' @import AnVIL
#'
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#' @param workspaceName Name of the workspace
#' @param inputOnly Under the default (\code{TRUE}), the file path to the input
#' files list and the paths to input files will be returned. If it's set to \code{FALSE},
#' the whole method configuration will be returned.
#'
#' @export
biobakery_currentInput <- function(accountEmail, billingProjectName, workspaceName,
                                   inputOnly = TRUE) {

    parsed <- RunTerraWorkflow::currentInput(accountEmail,
                                             billingProjectName,
                                             workspaceName,
                                             inputOnly = FALSE) # grab all the configuration info

    ## Return the whole method configuration
    if (isFALSE(inputOnly)) {return(parsed)}

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
