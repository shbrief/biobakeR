#' List outputs
#'
#' @import AnVIL
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#' @param workspaceName Name of the workspace
#' @param submissionId Submission Id. If it's not provided, the most recent
#' submission id will be used.
#' @param keyword A character string containing a regular exrpession to be matched
#' in the output file name. Under the default \code{NULL}, the names of all the output
#' files with \code{.tsv} extension will be returned.
#' @param all A logical. If it's set to \code{TRUE}, \code{keyword} argument will
#' be ignored and all the outputs from the workflow, including log files, will be
#' returned. Default is (\code{FALSE}).
#'
#' @return A tibble with four columns
#' \itemize{
#'     \item file : character() 'base name' of the file in the bucket.
#'     \item workflow : character() name of the workflow the file is associated with.
#'     \item task : character() name of the task in the workflow that generated the file.
#'     \item path : charcter() full path to the file in the google bucket.
#' }
#'
#' @export
listOutput <- function(accountEmail, billingProjectName, workspaceName,
                       submissionId = NULL, keyword = NULL, all = FALSE) {
    gcloud_account(accountEmail)
    gcloud_project(billingProjectName)

    avworkspace_namespace(billingProjectName)
    avworkspace_name(workspaceName)

    outputs <- avworkflow_files(submissionId = submissionId)

    if (isTRUE(all)) {return(outputs); stop()}   # all outputs including log

    if (!is.null(keyword)) {
        ind <- grep(keyword, outputs$file)
        res <- outputs[ind,,drop=FALSE]   # keyword-containing output files
    } else {
        ind <- grep(".tsv", outputs$file)
        res <- outputs[ind,,drop=FALSE]    # .tsv files
    }
    return(res)
}
