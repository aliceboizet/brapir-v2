#' @title
#' post /search/allelematrix
#'
#' @description
#' Submit a search request for a Allele Matrix.
#'
#' Use this endpoint to retrieve a two dimensional matrix of genotype data. The response structure is based on the VCF format, but the search and filter parameters give the ability to slice and merge data sets. This allows the user to return the subset of data they are interested in, without having to download the entire genotype file.
#'
#' Each row of data (outer array) corresponds to a variant definition, and each column (inner array) corresponds to a callSet.
#'
#' Search requests allow a client to send a complex query for data. However, the server may not respond with the search results immediately. If a server needs more time to process the request, it might respond with a `searchResultsDbId`. Use the corresponding `GET /search/allelematrix/{searchResultsDbId}` to retrieve the results of the search.
#'
#' Review the Search Services documentation for additional implementation details.
#'
#' @param callSetDbIds vector of type character; required: FALSE; A list of IDs which uniquely identify `CallSets` within the given database server
#' @param dataMatrixAbbreviations vector of type character; required: FALSE; `dataMatrixAbbreviations` is a comma seperated list of abbreviations (ie 'GT', 'RD' etc). This list controls which data matrices are returned in the response.
#' @param dataMatrixNames vector of type character; required: FALSE; `dataMatrixNames` is a list of names (ie 'Genotype', 'Read Depth' etc). This list controls which data matrices are returned in the response.
#' @param expandHomozygotes logical; required: FALSE; Should homozygotes be expanded (true) or collapsed into a single occurrence (false)
#' @param germplasmDbIds vector of type character; required: FALSE; A list of IDs which uniquely identify `Germplasm` within the given database server
#' @param germplasmNames vector of type character; required: FALSE; A list of human readable `Germplasm` names
#' @param germplasmPUIs vector of type character; required: FALSE; A list of permanent unique identifiers associated with `Germplasm`
#' @param pagination a list; required: FALSE; Pagination for the matrix see details
#' @param positionRanges vector of type character; required: FALSE; The postion range to search   Uses the format "<chrom>:<start>-<end>" where <chrom> is the chromosome name, <start> is  the starting position of the range, and <end> is the ending position of the range
#' @param preview logical; required: FALSE; Default Value = false   If 'preview' is set to true, then the server should only return the lists of 'callSetDbIds',  'variantDbIds', and 'variantSetDbIds'. The server should not return any matrix data. This is intended to be a preview and give the client a sense of how large the matrix returned will be   If 'preview' is set to false or not set (default), then the server should return all the matrix data as requested.
#' @param sampleDbIds vector of type character; required: FALSE; A list of IDs which uniquely identify `Samples` within the given database server
#' @param sepPhased string; required: FALSE; The string used as a separator for phased allele calls.
#' @param sepUnphased string; required: FALSE; The string used as a separator for unphased allele calls.
#' @param unknownString string; required: FALSE; The string used as a representation for missing data.
#' @param variantDbIds vector of type character; required: FALSE; A list of IDs which uniquely identify `Variants` within the given database server
#' @param variantSetDbIds vector of type character; required: FALSE; A list of IDs which uniquely identify `VariantSets` within the given database server
#'
#' @details
#'
#' @return list
#'
#' @author J.-F. Rami
#'
#' @export
#' @family brapi-genotyping
#' @family Allele Matrix
#'
#' @examples
brapi_post_search_allelematrix <- function(con = NULL,
                                           callSetDbIds='',
                                           dataMatrixAbbreviations='',
                                           dataMatrixNames='',
                                           germplasmDbIds='',
                                           germplasmNames='',
                                           germplasmPUIs='',
                                           pagination=list(list(dimension="variants",
                                                                page=0,
                                                                pageSize=1000),
                                                           list(dimension="callsets",
                                                                page=0,
                                                                pageSize=100)),
                                           positionRanges='',
                                           sampleDbIds='',
                                           variantDbIds='',
                                           variantSetDbIds='',
                                           expandHomozygotes=TRUE,
                                           preview=FALSE,
                                           sepPhased='',
                                           sepUnphased='',
                                           unknownString='') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
                                           callPath = "/search/allelematrix",
                                           reqArgs = "",
                                           packageName = "BrAPI-Genotyping",
                                           callVersion = 2.0)
  ## Build the Body
  callbody <- brapirv2:::brapi_POST_callBody(usedArgs = usedArgs,
                                             reqArgs = "")

  try({
    ## Make the call and receive the response
    resp <- brapirv2:::brapi_POST(url = callurl, body = callbody, usedArgs = usedArgs)
    ## Message about call status
    if (httr::status_code(resp) == 200) {
      message(paste0("Immediate Response.", "\n"))
    } else if (httr::status_code(resp) == 202) {
      message(paste0("Saved or Asynchronous Response has provided a searchResultsDbId.", "\n"))
      message(paste0("Use the GET /search/allelematrix/{searchResultsDbId} call to retrieve the paginated output.", "\n"))
    } else {
      stop(paste0("The POST /search/allelematrix call resulted in Server status, ", httr::http_status(resp)[["message"]]))
    }
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Do not convert the content object into a data.frame
    #out <- brapirv2:::brapi_result2df(cont, usedArgs)
    # return as a list instead to get pagination information
    out <- fromJSON(cont)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_search_allematrix")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
