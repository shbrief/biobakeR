#' Check the status of submitted jobs
#'
#' @import AnVIL
#' @import dplyr
#'
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#' @param workspaceName Name of the workspace
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
monitorSubmission <- function(accountEmail, billingProjectName, workspaceName) {

    gcloud_account(accountEmail)
    gcloud_project(billingProjectName)
    terra <- Terra()
    status <- terra$status

    res <- avworkflow_jobs(namespace = billingProjectName,
                           name = workspaceName)

    if (length(res) == 0) {
        stop("There is no previously submitted job.", call. = FALSE)
    } else {return(res)}
}
