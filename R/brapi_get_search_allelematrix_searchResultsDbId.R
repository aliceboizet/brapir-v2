#' @title
#' get /search/allelematrix/\{searchResultsDbId\}
#'
#' @description
#' Get the results of a `AlleleMatrix` search request
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param searchResultsDbId character; required: TRUE; Unique identifier which
#'    references the search results
#'
#' @details Returns the result of the advanced searching for the `AlleleMatrix`
#'    resource.
#'
#' @return data.frame
#'
#' @author J.-F. Rami
#'
#'
#' @family brapi-genotyping
#' @family Allele Matrix
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' # Saved or Asynchronous Search Response Example
#' out <-
#'  brapi_post_search_allelematrixs(con = con,
#'                             callSetDbIds = c("callset01",
#'                                              "callset02")
#'                                              )
#' searchResultsDbId <- out$searchResultsDbId
#' brapi_get_search_allelematrix_searchResultsDbId(con = con,
#'                                             searchResultsDbId = searchResultsDbId)
#' }
#'
#' @export
brapi_get_search_allelematrix_searchResultsDbId <- function(con = NULL,
                                                        searchResultsDbId = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "searchResultsDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                          callPath = "/search/allelematrix/{searchResultsDbId}",
                                          reqArgs = "searchResultsDbId",
                                          packageName = "BrAPI-Genotyping",
                                          callVersion = 2.0)

  try({
    ## Make the call and receive the response
    resp <- brapirv2:::brapi_GET(url = callurl, usedArgs = usedArgs)
    ## Check call status
    while (httr::status_code(resp) == 202) {
      Sys.sleep(5)
      resp <- brapirv2:::brapi_GET(url = callurl, usedArgs = usedArgs)
      status <- jsonlite::fromJSON(httr::content(x = resp,
                                                 as = "text",
                                                 encoding = "UTF-8"))[["metadata"]][["status"]]
      if (length(status) != 0) {
        brapirv2:::brapi_message(msg = paste0(status[["message"]], "\n"))
      }
    }
    if (httr::status_code(resp) == 200) {
      ## Extract the content from the response object in human readable form
      cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
      ## Convert the content object into a data.frame
      out <- fromJSON(cont)
    }
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_get_search_allelematrix_searchResultsDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
