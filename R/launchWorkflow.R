#' Launch workflow on Terra
#'
#' @import AnVIL
#'
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#' @param workspaceName Name of the workspace
#'
#' @examples
#' launchWorkflow(accountEmail = "shbrief@gmail.com",
#'                billingProjectName = "waldronlab-terra-rstudio",
#'                workspaceName = "mtx_workflow_biobakery_ver3")
#'
#' @export
launchWorkflow <- function(accountEmail, billingProjectName, workspaceName) {
    gcloud_account <- accountEmail
    terra <- Terra()
    status_resp <- terra$status()

    resp <- terra$createSubmission(
        workspaceNamespace = billingProjectName,
        workspaceName = workspaceName,
        methodConfigurationNamespace = "mtx_workflow_biobakery_version3",
        methodConfigurationName = "mtx_workflow_biobakery_version3",
        useCallCache = TRUE)

    if (resp$status_code == 201) {"Workflow is succesfully launched."}
    else {"Workflow luanching is failed."}
}
