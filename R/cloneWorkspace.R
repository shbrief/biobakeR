#' Clone bioBakery workflow template
#'
#' @param accountEmail Email linked to Terra account
#' @param projectName Name of the billing project
#' @param workspaceName Name of the workspace
#'
#' @examples
#'cloneWorkspace(accountEmail = "shbrief@gmail.com",
#'               projectName = "waldronlab-terra-rstudio",
#'               workspaceName = "mtx_workflow_biobakery_ver3")
#'
#' @export
cloneWorkspace <- function(accountEmail, projectName, workspaceName) {
    gcloud_account <- accountEmail
    terra <- Terra()

    resp <- terra$cloneWorkspace(
        workspaceNamespace = "waldronlab-terra-rstudio",
        workspaceName = "mtx_workflow_biobakery_ver3",
        namespace = projectName,
        name = workspaceName,
        attributes = AnVIL::empty_object
    )

    if (resp$status_code == 201) {print("Workspace is successfully cloned")}
    if (resp$status_code == 400) {print("Unable to create resources for workspace")}
    if (resp$status_code == 404) {print("Source workspace not found")}
    if (resp$status_code == 409) {print("Your workspaceName already exists. Try a new one.")}
    if (resp$status_code == 500) {print("Internet server error")}
}
