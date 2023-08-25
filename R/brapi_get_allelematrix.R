#' Title
#' get /allelematrix
#'
#' @description
#' Use this endpoint to retrieve a two dimensional matrix of genotype data. The response structure is based on the VCF file format, but the search and filter parameters give the ability to slice and merge data sets. This allows the user to return the subset of data they are interested in, without having to download the entire genotype file.Each row of data (outer array) corresponds to a variant definition, and each column (inner array) corresponds to a callSet.
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param dimensionVariantPage integer; required: FALSE; The requested page number for the Variant dimension of the matrix
#' @param dimensionVariantPageSize integer; required: FALSE; The requested page size for the Variant dimension of the matrix
#' @param dimensionCallSetPage integer; required: FALSE; The requested page number for the CallSet dimension of the matrix
#' @param dimensionCallSetPageSize integer; required: FALSE; The requested page size for the CallSet dimension of the matrix
#' @param preview logical; required: FALSE; Default Value = false  If 'preview' is set to true, then the server should return with the "dataMatrices" field as null or empty. All other data fields should be returned normally.  This is intended to be a preview and give the client a sense of how large the matrix returned will be  If 'preview' is set to false or not set (default), then the server should return all the matrix data as requested.
#' @param dataMatrixNames character; required: FALSE; "dataMatrixNames" is a comma seperated list of names (ie 'Genotype, Read Depth' etc). This list controls which data matrices are returned in the response.   This maps to a FORMAT field in the VCF file standard.
#' @param dataMatrixAbbreviations character; required: FALSE; "dataMatrixAbbreviations" is a comma seperated list of abbreviations (ie 'GT, RD' etc). This list controls which data matrices are returned in the response.   This maps to a FORMAT field in the VCF file standard.
#' @param positionRange character; required: FALSE; The postion range to search   Uses the format "contig:start-end" where "contig" is the chromosome or contig name, "start" is   the starting position of the range, and "end" is the ending position of the range   Example: CRHOM_1:12000-14000
#' @param germplasmDbId character; required: FALSE; Use this parameter to only return results associated with the given `Germplasm` unique identifier.   Use `GET /germplasm` to find the list of available `Germplasm` on a server.
#' @param germplasmName character; required: FALSE; Use this parameter to only return results associated with the given `Germplasm` by its human readable name.   Use `GET /germplasm` to find the list of available `Germplasm` on a server.
#' @param germplasmPUI character; required: FALSE; Use this parameter to only return results associated with the given `Germplasm` by its global permanent unique identifier.   Use `GET /germplasm` to find the list of available `Germplasm` on a server.
#' @param callSetDbId character; required: FALSE; The ID which uniquely identifies a `CallSet` within the given database server
#' @param variantDbId character; required: FALSE; The ID which uniquely identifies a `Variant` within the given database server
#' @param variantSetDbId character; required: FALSE; The ID which uniquely identifies a `VariantSet` within the given database server
#' @param expandHomozygotes logical; required: FALSE; Should homozygotes be expanded (true) or collapsed into a single occurrence (false)
#' @param unknownString character; required: FALSE; The string to use as a representation for missing data
#' @param sepPhased character; required: FALSE; The string to use as a separator for phased allele calls
#' @param sepUnphased character; required: FALSE; The string to use as a separator for unphased allele calls
#'
#' @return
#' @export
#'
#' @examples
#'
brapi_get_allelematrix <- function(con, callSetDbId='',
                                   dataMatrixAbbreviations='',
                                   dataMatrixNames='',
                                   dimensionCallSetPage=0,
                                   dimensionCallSetPageSize=100,
                                   dimensionVariantPage=0,
                                   dimensionVariantPageSize=1000,
                                   expandHomozygotes=TRUE,
                                   germplasmDbId='',
                                   germplasmName='',
                                   germplasmPUI='',
                                   positionRange='',
                                   preview=FALSE,
                                   sepPhased='',
                                   sepUnphased='',
                                   unknownString='',
                                   variantDbId='',
                                   variantSetDbId=''){
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                          callPath = "/allelematrix",
                                          reqArgs = "",
                                          packageName = "BrAPI-Germplasm",
                                          callVersion = 2.0)
  try({
    ## Make the call and receive the response
    resp <- brapirv2:::brapi_GET(url = callurl, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Do not convert the content object into a data.frame
    #out <- brapirv2:::brapi_result2df(cont, usedArgs)
    # return as a list instead to get pagination information
    out <- fromJSON(cont)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_get_allelematrix")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
