% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brapi_post_search_callsets.R
\name{brapi_post_search_callsets}
\alias{brapi_post_search_callsets}
\title{post /search/callsets}
\usage{
brapi_post_search_callsets(
  con = NULL,
  callSetDbIds = "",
  callSetNames = "",
  germplasmDbIds = "",
  germplasmNames = "",
  page = 0,
  pageSize = 1000,
  sampleDbIds = "",
  sampleNames = "",
  variantSetDbIds = ""
)
}
\arguments{
\item{con}{list; required: TRUE; BrAPI connection object}

\item{callSetDbIds}{vector of type character; required: FALSE; The CallSet to
search for specified as unique database callset identifier(s).; default:
"", when using multiple values supply as c("value1",
"value2").}

\item{callSetNames}{vector of type character; required: FALSE; Only return
call sets with these names (case-sensitive, exact match).; default:
"", when using multiple values supply as c("value1",
"value2").}

\item{germplasmDbIds}{vector of type character; required: FALSE; Vector of
database identifiers which uniquely identify germplasm to search for;
default: "", when using multiple values supply as
c("value1", "value2").}

\item{germplasmNames}{vector of type character; required: FALSE; List of
human readable names to identify germplasm to search for; default:
"", when using multiple values supply as c("value1",
"value2").}

\item{page}{integer; required: FALSE; Used to request a specific page of data
to be returned. The page indexing starts at 0 (the first page is
\code{page = 0}). Default is \code{0}.}

\item{pageSize}{integer; required: FALSE; The size of the pages to be
returned. Default is \code{1000}.}

\item{sampleDbIds}{vector of type character; required: FALSE; Return only
call sets generated from the provided unique database Biosample
identifiers; default: "", when using multiple values supply as
c("value1", "value2").}

\item{sampleNames}{vector of type character; required: FALSE; Return only
call sets generated from the provided Biosample human readable names.;
default: "", when using multiple values supply as
c("value1", "value2").}

\item{variantSetDbIds}{vector of type character; required: FALSE; The unique
VariantSet database identifier(s) to search for; default: "",
when using multiple values supply as c("value1",
"value2").}
}
\value{
data.frame
}
\description{
Submit a search request for \code{CallSets}
}
\details{
Submit a search request for \code{CallSets}. Function will return either
the search results (Status 200 for an immediate response) or a
\code{searchResultsDbId} (Status 202 for both a saved and an asynchronous
search).
}
\examples{
\dontrun{
con <- brapi_db()$testserver
con[["token"]] <- "YYYY"

# Immediate Response Example
brapi_post_search_callsets(con = con,
                           page = 0,
                           pageSize = 1000)

# Saved or Asynchronous Search Response Example
brapi_post_search_callsets(con = con,
                           callSetDbIds = c("callset01",
                                            "callset02"),
                           page = 0,
                           pageSize = 1000,
                           sampleDbIds = c("sample3",
                                           "sample2"),
                           variantSetDbIds = c("variantset1"))
}

}
\references{
\href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Call\%20Sets/post_search_callsets }{BrAPI SwaggerHub}
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
\code{\link{brapi_post_search_allelematrix}()},
\code{\link{brapi_post_search_calls}()},
\code{\link{brapi_post_search_markerpositions}()},
\code{\link{brapi_post_search_referencesets}()},
\code{\link{brapi_post_search_references}()},
\code{\link{brapi_post_search_samples}()},
\code{\link{brapi_post_search_variantsets}()},
\code{\link{brapi_post_search_variants}()},
\code{\link{brapi_post_variantsets_extract}()},
\code{\link{brapi_put_samples_sampleDbId}()}

Other Call Sets: 
\code{\link{brapi_get_callsets_callSetDbId_calls}()},
\code{\link{brapi_get_callsets_callSetDbId}()},
\code{\link{brapi_get_callsets}()},
\code{\link{brapi_get_search_callsets_searchResultsDbId}()}
}
\author{
Maikel Verouden
}
\concept{Call Sets}
\concept{brapi-genotyping}
