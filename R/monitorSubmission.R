#' Check the status of submitted jobs
#' 
#' @import AnVIL
#' 
#' @param accountEmail Email linked to Terra account
#' @param billingProject Name of the billing project
#' @param workspaceName Name of the workspace
#' @param mostRecentOnly Under the default (\code{TRUE}), the status of only the 
#' lastest submission is returned. If it's set to \coce{FALSE}, all the submission
#' status information will be returned in a data frame.
#' 
#' @return A list or a data frame containing the submission information, such as
#' submission status, submission date, and submission Id.
#' 
#' @example
#' monitorSubmittion(accountEmail = "shbrief@gmail.com", 
#'                   billingProject = "waldronlab-terra-rstudio", 
#'                   workspaceName = "mtx_workflow_biobakery_ver3")
#' 
#' @export
monitorSubmittion <- function(accountEmail, billingProject, workspaceName, 
                              mostRecentOnly = TRUE) {
    gcloud_account <- accountEmail
    terra <- Terra()
    
    resp <- terra$listSubmissions(workspaceNamespace = billingProject, 
                                  workspaceName = workspaceName)
    
    if (resp$status_code == 404) {stop("Workspace not found.")}
    if (resp$status_code == 505) {stop("Internet error.")}
    
    ## Parse the output
    parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
    
    if (isTRUE(mostRecentOnly)) {
        res <- parsed[[1]]
        structure(list(status = res$status,
                       submissionData = res$submissionDate,
                       submissionId = res$submissionId))
    } else {
        res <- sapply(parsed, function(x) {
            structure(list(status = x$status,
                           submissionData = x$submissionDate,
                           submissionId = x$submissionId))
        })
        return(as.data.frame(res))
    }
}