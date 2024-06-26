% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brapi_get_markerpositions.R
\name{brapi_get_markerpositions}
\alias{brapi_get_markerpositions}
\title{get /markerpositions}
\usage{
brapi_get_markerpositions(
  con = NULL,
  mapDbId = "",
  linkageGroupName = "",
  variantDbId = "",
  minPosition = as.integer(NA),
  maxPosition = as.integer(NA),
  page = 0,
  pageSize = 1000
)
}
\arguments{
\item{con}{list; required: TRUE; BrAPI connection object}

\item{mapDbId}{character; required: FALSE; unique database identifier of a
map.}

\item{linkageGroupName}{character; required: FALSE; The chromosome identifier
or the generic linkage group identifier if the chromosome is not
applicable.}

\item{variantDbId}{character; required: FALSE; The unique database identifier
for a marker.}

\item{minPosition}{integer; required: FALSE; The minimum position.}

\item{maxPosition}{integer; required: FALSE; The maximum position.}

\item{page}{integer; required: FALSE; Used to request a specific page of data
to be returned. The page indexing starts at 0 (the first page is
\code{page = 0}). Default is \code{0}.}

\item{pageSize}{integer; required: FALSE; The size of the pages to be
returned. Default is \code{1000}.}
}
\value{
data.frame
}
\description{
Get marker position info
}
\details{
Get marker position information, based on Map, Linkage Group, and
Marker Identifier.
}
\examples{
\dontrun{
con <- brapi_db()$testserver
brapi_get_markerpositions(con = con)
}

}
\references{
\href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Genome\%20Maps/get_markerpositions }{BrAPI SwaggerHub}
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

Other Genome Maps: 
\code{\link{brapi_get_maps_mapDbId_linkagegroups}()},
\code{\link{brapi_get_maps_mapDbId}()},
\code{\link{brapi_get_maps}()},
\code{\link{brapi_get_search_markerpositions_searchResultsDbId}()},
\code{\link{brapi_post_search_markerpositions}()}
}
\author{
Maikel Verouden
}
\concept{Genome Maps}
\concept{brapi-genotyping}
