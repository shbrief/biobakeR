#' Check the status of submitted jobs
#'
#' @import AnVIL
#' @import dplyr
#'
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#' @param workspaceName Name of the workspace
#' @param mostRecentOnly Under the default (\code{TRUE}), the status of only the
#' lastest submission is returned. If it's set to \code{FALSE}, all the submission
#' status information will be returned in a data frame.
#'
#' @return A list or a data frame containing the submission information, such as
#' submission status, submission date, and submission Id.
#'
#' @examples
#' monitorSubmission(accountEmail = "shbrief@gmail.com",
#'                   billingProjectName = "waldronlab-terra-rstudio",
#'                   workspaceName = "mtx_workflow_biobakery_ver3")
#'
#' @export
monitorSubmission <- function(accountEmail, billingProjectName, workspaceName,
                              mostRecentOnly = TRUE) {
    gcloud_account <- accountEmail
    terra <- Terra()
    status <- terra$status

    resp <- terra$listSubmissions(workspaceNamespace = billingProjectName,
                                  workspaceName = workspaceName)

    if (resp$status_code == 404) {stop("Workspace not found.")}
    if (resp$status_code == 505) {stop("Internet error.")}

    ## Parse the output
    parsed <- jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"),
                                 simplifyVector = FALSE)
    res <- sapply(parsed, function(x) {
        structure(list(status = x$status,
                       submissionData = x$submissionDate,
                       submissionId = x$submissionId))
    })

    if (length(res) == 0) {
        stop("There is no previously submitted job.", call. = FALSE)
    }

    if (isFALSE(mostRecentOnly)) {
        return(as.data.frame(res))
    } else {
        ordered <- res["submissionData",] %>% as.character
        ordered <- order(ordered, decreasing = TRUE)
        recent_res <- res[,ordered][,1]
        return(recent_res)
    }
}
