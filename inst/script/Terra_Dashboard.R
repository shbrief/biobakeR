## Collect Terra Dashboard information

workspaceNamespace <- "waldronlab-terra-rstudio"
workspaceName <- "mtx_workflow_biobakery_ver3"
terra <- Terra()

resp <- terra$getWorkspace(workspaceNamespace, workspaceName)
parsed <- suppressMessages(jsonlite::fromJSON(content(resp, "text"),
                                              simplifyVector = FALSE))

desc <- parsed$workspace$attributes$description
last_modified <- parsed$workspace$lastModified
date <- substr(last_modified, 1, 10)

writeLines(date, "~/data2/bioBakeryR/inst/extdata/date_of_last_update.txt")
saveRDS(desc, file = "~/data2/bioBakeryR/inst/extdata/desc.rds")
