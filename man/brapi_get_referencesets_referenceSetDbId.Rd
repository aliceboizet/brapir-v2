% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brapi_get_referencesets_referenceSetDbId.R
\name{brapi_get_referencesets_referenceSetDbId}
\alias{brapi_get_referencesets_referenceSetDbId}
\title{get /referencesets/\{referenceSetDbId\}}
\usage{
brapi_get_referencesets_referenceSetDbId(con = NULL, referenceSetDbId = "")
}
\arguments{
\item{con}{list; required: TRUE; BrAPI connection object}

\item{referenceSetDbId}{character; required: TRUE; The unique database
identifier of the \code{ReferenceSet} to be retrieved.}
}
\value{
data.frame
}
\description{
Gets a \code{ReferenceSet} by ID.
}
\details{
Gets a \code{ReferenceSet} by ID.
}
\examples{
\dontrun{
con <- brapi_db()$testserver
brapi_get_referencesets_referenceSetDbId(con = con,
                                         referenceSetDbId = "reference_set1")
}

}
\references{
\href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Reference\%20Sets/get_referencesets__referenceSetDbId_ }{BrAPI SwaggerHub}
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

Other Reference Sets: 
\code{\link{brapi_get_referencesets}()},
\code{\link{brapi_get_search_referencesets_searchResultsDbId}()},
\code{\link{brapi_post_search_referencesets}()}
}
\author{
Maikel Verouden
}
\concept{Reference Sets}
\concept{brapi-genotyping}
