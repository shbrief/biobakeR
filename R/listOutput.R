#' List outputs
#'
#' @import AnVIL
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#' @param workspaceName Name of the workspace
#' @param submissionId Submission Id. If it's not provided, the most recent
#' submission id will be used.
#' @param keyword A keyword in the output file name. Under the default \code{NULL},
#' names of all the output files with \code{.tsv} extension will be returned.
#'
#' @export
listOutput <- function(accountEmail, billingProjectName, workspaceName,
                       submissionId = NULL, keyword = NULL) {
    gcloud_account(accountEmail)
    gcloud_project(billingProjectName)

    avworkspace_namespace(billingProjectName)
    avworkspace_name(workspaceName)

    outputs <- avworkflow_files(submissionId = submissionId)

    if (!is.null(keyword)) {
        ind <- grep(keyword, outputs$file)
        res <- outputs$file[ind]
    } else {
        ind <- grep(".tsv", outputs$file)
        res <- outputs$file[ind]
    }
    return(res)
}
