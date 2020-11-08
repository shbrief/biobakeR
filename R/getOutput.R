#' Download output
#'
#'
#' @examples
#' fname <- "wmgx_report.pdf"
#'
#'
getOutput <- function(accountEmail, billingProjectName, workspaceName,
                      submissionId = NULL, keyword = NULL, dest_dir = ".") {
    gcloud_account(accountEmail)
    gcloud_project(billingProjectName)

    avworkspace_namespace(billingProjectName)
    avworkspace_name(workspaceName)

    if (is.null(keyword)) {stop("You should provide keyword argument.")}

    outputs <- avworkflow_files(submissionId = submissionId)

    ind <- grep(keyword, outputs$file)
    res <- outputs$path[ind]

    gsutil_cp(res, destination = dest_dir)
}
