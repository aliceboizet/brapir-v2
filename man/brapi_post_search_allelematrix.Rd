% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brapi_post_search_allelematrix.R
\name{brapi_post_search_allelematrix}
\alias{brapi_post_search_allelematrix}
\title{post /search/allelematrix}
\usage{
brapi_post_search_allelematrix(
  con = NULL,
  callSetDbIds = "",
  dataMatrixAbbreviations = "",
  dataMatrixNames = "",
  germplasmDbIds = "",
  germplasmNames = "",
  germplasmPUIs = "",
  pagination = list(list(dimension = "variants", page = 0, pageSize = 1000),
    list(dimension = "callsets", page = 0, pageSize = 100)),
  positionRanges = "",
  sampleDbIds = "",
  variantDbIds = "",
  variantSetDbIds = "",
  expandHomozygotes = TRUE,
  preview = FALSE,
  sepPhased = "",
  sepUnphased = "",
  unknownString = ""
)
}
\arguments{
\item{callSetDbIds}{vector of type character; required: FALSE; A list of IDs which uniquely identify \code{CallSets} within the given database server}

\item{dataMatrixAbbreviations}{vector of type character; required: FALSE; \code{dataMatrixAbbreviations} is a comma seperated list of abbreviations (ie 'GT', 'RD' etc). This list controls which data matrices are returned in the response.}

\item{dataMatrixNames}{vector of type character; required: FALSE; \code{dataMatrixNames} is a list of names (ie 'Genotype', 'Read Depth' etc). This list controls which data matrices are returned in the response.}

\item{germplasmDbIds}{vector of type character; required: FALSE; A list of IDs which uniquely identify \code{Germplasm} within the given database server}

\item{germplasmNames}{vector of type character; required: FALSE; A list of human readable \code{Germplasm} names}

\item{germplasmPUIs}{vector of type character; required: FALSE; A list of permanent unique identifiers associated with \code{Germplasm}}

\item{pagination}{a list; required: FALSE; Pagination for the matrix see details}

\item{positionRanges}{vector of type character; required: FALSE; The postion range to search   Uses the format "<chrom>:<start>-<end>" where <chrom> is the chromosome name, <start> is  the starting position of the range, and <end> is the ending position of the range}

\item{sampleDbIds}{vector of type character; required: FALSE; A list of IDs which uniquely identify \code{Samples} within the given database server}

\item{variantDbIds}{vector of type character; required: FALSE; A list of IDs which uniquely identify \code{Variants} within the given database server}

\item{variantSetDbIds}{vector of type character; required: FALSE; A list of IDs which uniquely identify \code{VariantSets} within the given database server}

\item{expandHomozygotes}{logical; required: FALSE; Should homozygotes be expanded (true) or collapsed into a single occurrence (false)}

\item{preview}{logical; required: FALSE; Default Value = false   If 'preview' is set to true, then the server should only return the lists of 'callSetDbIds',  'variantDbIds', and 'variantSetDbIds'. The server should not return any matrix data. This is intended to be a preview and give the client a sense of how large the matrix returned will be   If 'preview' is set to false or not set (default), then the server should return all the matrix data as requested.}

\item{sepPhased}{string; required: FALSE; The string used as a separator for phased allele calls.}

\item{sepUnphased}{string; required: FALSE; The string used as a separator for unphased allele calls.}

\item{unknownString}{string; required: FALSE; The string used as a representation for missing data.}
}
\value{
list
}
\description{
Submit a search request for a Allele Matrix.

Use this endpoint to retrieve a two dimensional matrix of genotype data. The response structure is based on the VCF format, but the search and filter parameters give the ability to slice and merge data sets. This allows the user to return the subset of data they are interested in, without having to download the entire genotype file.

Each row of data (outer array) corresponds to a variant definition, and each column (inner array) corresponds to a callSet.

Search requests allow a client to send a complex query for data. However, the server may not respond with the search results immediately. If a server needs more time to process the request, it might respond with a \code{searchResultsDbId}. Use the corresponding \code{GET /search/allelematrix/{searchResultsDbId}} to retrieve the results of the search.

Review the Search Services documentation for additional implementation details.
}
\details{

}
\seealso{
Other brapi-genotyping: 
\code{\link{brapi_get_allelematrix}()},
\code{\link{brapi_get_callsets_callSetDbId_calls}()},
\code{\link{brapi_get_callsets_callSetDbId}()},
\code{\link{brapi_get_callsets}()},
\code{\link{brapi_get_calls}()},
\code{\link{brapi_get_maps_mapDbId_linkagegroups}()},
\code{\link{brapi_get_maps_mapDbId}()},
\code{\link{brapi_get_maps}()},
\code{\link{brapi_get_markerpositions}()},
\code{\link{brapi_get_references_referenceDbId_bases}()},
\code{\link{brapi_get_references_referenceDbId}()},
\code{\link{brapi_get_referencesets_referenceSetDbId}()},
\code{\link{brapi_get_referencesets}()},
\code{\link{brapi_get_references}()},
\code{\link{brapi_get_samples_sampleDbId}()},
\code{\link{brapi_get_samples}()},
\code{\link{brapi_get_search_allelematrix_searchResultsDbId}()},
\code{\link{brapi_get_search_calls_searchResultsDbId}()},
\code{\link{brapi_get_search_callsets_searchResultsDbId}()},
\code{\link{brapi_get_search_markerpositions_searchResultsDbId}()},
\code{\link{brapi_get_search_references_searchResultsDbId}()},
\code{\link{brapi_get_search_referencesets_searchResultsDbId}()},
\code{\link{brapi_get_search_samples_searchResultsDbId}()},
\code{\link{brapi_get_search_variants_searchResultsDbId}()},
\code{\link{brapi_get_search_variantsets_searchResultsDbId}()},
\code{\link{brapi_get_variants_variantDbId_calls}()},
\code{\link{brapi_get_variants_variantDbId}()},
\code{\link{brapi_get_variantsets_variantSetDbId_callsets}()},
\code{\link{brapi_get_variantsets_variantSetDbId_calls}()},
\code{\link{brapi_get_variantsets_variantSetDbId_variants}()},
\code{\link{brapi_get_variantsets_variantSetDbId}()},
\code{\link{brapi_get_variantsets}()},
\code{\link{brapi_get_variants}()},
\code{\link{brapi_get_vendor_orders_orderId_plates}()},
\code{\link{brapi_get_vendor_orders_orderId_results}()},
\code{\link{brapi_get_vendor_orders_orderId_status}()},
\code{\link{brapi_get_vendor_orders}()},
\code{\link{brapi_get_vendor_specifications}()},
\code{\link{brapi_post_samples}()},
\code{\link{brapi_post_search_callsets}()},
\code{\link{brapi_post_search_calls}()},
\code{\link{brapi_post_search_markerpositions}()},
\code{\link{brapi_post_search_referencesets}()},
\code{\link{brapi_post_search_references}()},
\code{\link{brapi_post_search_samples}()},
\code{\link{brapi_post_search_variantsets}()},
\code{\link{brapi_post_search_variants}()},
\code{\link{brapi_post_variantsets_extract}()},
\code{\link{brapi_put_samples_sampleDbId}()}

Other Allele Matrix: 
\code{\link{brapi_get_allelematrix}()},
\code{\link{brapi_get_search_allelematrix_searchResultsDbId}()}
}
\author{
J.-F. Rami
}
\concept{Allele Matrix}
\concept{brapi-genotyping}
