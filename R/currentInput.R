#' Check the current input arguments
#'
#' @import AnVIL
#' @import httr
#'
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#' @param workspaceName Name of the workspace
#' @param allInputs Under the default (\code{FALSE}), the file path to the input
#' files list and the paths to input files will be returned. If it's set to \code{TRUE}
#' all the input information will be returned.
#'
#' @examples
#' currentInput(accountEmail = "shbrief@gmail.com",
#'              billingProjectName = "waldronlab-terra-rstudio",
#'              workspaceName = "mtx_workflow_biobakery_ver3")
#'
#' @export
currentInput <- function(accountEmail, billingProjectName, workspaceName,
                         allInputs = FALSE) {
    gcloud_account <- accountEmail
    terra <- Terra()

    # status <- terra$status()
    # if (status$status_code != 200) {
    #     stop()
    # }

    resp <- terra$getWorkspaceMethodConfig(
        workspaceNamespace = billingProjectName,
        workspaceName = workspaceName,
        configNamespace = "mtx_workflow_biobakery_version3",
        configName = "mtx_workflow_biobakery_version3"
    )
    if (http_type(resp) != "application/json") {
        stop("API did not return json", call. = FALSE)
    }

    ## Parse the output
    parsed <- jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"),
                                 simplifyVector = FALSE)

    if (http_error(resp)) {
        stop(sprintf("Terra API request failed [%s]\n%s\n<%s>",
                     status_code(resp),
                     parsed$message,
                     parsed$documentation_url),
             call. = FALSE)
    }

    if (isTRUE(allInputs)) {return(parsed)}
    else {
        res <- parsed$inputs$workflowMTX.inputRead1Files
        res <- gsub("\"", "", res)
        structure(list(inputListPath = res,
                       inputFilePath = gsutil_cat(res)))
    }
}
