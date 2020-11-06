#' Display the head of output tsv file
#'
#' @param x Output file name
#' @param n The number of rows to return. Default is 6.
#' @param accountEmail Email linked to Terra account
#' @param projectName Name of the billing project
#' @param workspaceName Name of the workspace
#' @param submissionId Submission Id. If it's not provided, the most recent
#' submission id will be used.
#'
#' @export
tableHead <- function(x, n = 6, accountEmail, projectName, workspaceName,
                      submissionId = NULL) {
    gcloud_account(accountEmail)
    gcloud_project(projectName)

    avworkspace_namespace(projectName)
    avworkspace_name(workspaceName)

    outputs <- avworkflow_files(submissionId = submissionId)

    output_ind <- which(outputs$file == x)
    output_path <- outputs$path[output_ind]
    res <- read.table(gsutil_pipe(output_path), sep = "\t")
    head(res, n)
}
