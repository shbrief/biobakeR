#' Abort Terra Submission
#'
#' @import AnVIL
#'
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#' @param workspaceName Name of the workspace
#' @param submissionId Submission ID
#'
#' @export
abortSubmission <- function(accountEmail, billingProjectName, workspaceName,
                            submissionId) {
    gcloud_account <- accountEmail
    terra <- Terra()

    resp <- terra$abortSubmission(workspaceNamespace = billingProjectName,
                                  workspaceName = workspaceName,
                                  submissionId = submissionId)

    if (resp$status_code == 204) {print("Workflow is succesfully aborted.")}
    if (resp$status_code == 401) {print("You are not authorized to access.")}
    if (resp$status_code == 404) {print("Submission is not found.")}
    if (resp$status_code == 500) {print("Internet Error.")}
}
