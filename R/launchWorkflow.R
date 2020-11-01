#' Launch workflow on Terra
#'
#' @import AnVIL
#'
#' @param accountEmail Email linked to Terra account
#' @param billingProject Name of the billing project
#' @param workspaceName Name of the workspace
#'
#' @example
#' launchWorkflow(accountEmail = "shbrief@gmail.com",
#'                billingProject = "waldronlab-terra-rstudio",
#'                workspaceName = "mtx_workflow_biobakery_ver3")
#'
#' @export
launchWorkflow <- function(accountEmail, billingProject, workspaceName) {
    gcloud_account <- accountEmail
    terra <- Terra()

    resp <- terra$createSubmission(
        workspaceNamespace = billingProject,
        workspaceName = workspaceName,
        methodConfigurationNamespace = "mtx_workflow_biobakery_version3",
        methodConfigurationName = "mtx_workflow_biobakery_version3",
        useCallCache = TRUE)

    if (resp$status_code == 201) {"Workflow is succesfully launched."}
    else {"Workflow luanching is failed."}
}
