#' Helper function for parsing the result part of the response content into a
#' flattened data.frame object
#'
#' resultList can consist of:
#' 1) Single Response (master): In this type of response, the result field is an
#'    object describing a single record of data. There should NOT be a "data"
#'    array/element in this case and pagination does not apply. A Single Response
#'    is used when you are requesting or modifying a single, specific data object
#'    by its DbId.
#' 2) List Response (detail): In this type of response, the result object only
#'    contains a "data" array/element, which is an arbitrarily long array of
#'    objects of the same type. Pagination request parameters and pagination
#'    metadata fields apply to this data array.
#' 3) Special cases (master/detail: mix of Single and List response (parse "data"
#'    element and repeat single response part to match dimensions).
#'
#' @noRd
#' @keywords internal
#' @importFrom utils as.relistable read.csv read.delim tail
#' @importFrom plyr empty
brapi_result2df <- function(cont, usedArgs) {
  ## Helper functions
  jointDetail <- function(detailDat, colName) {
    detailDatCol <- data.frame(detailDat[[colName]],
                               stringsAsFactors = FALSE)
    if (!colName %in% c("taxonIds", "seasons", "studies")) {
      names(detailDatCol) <- paste(colName, names(detailDatCol), sep = ".")
    }
    detailDat[[colName]] <- NULL
    if (nrow(detailDatCol) > 0) {
      detailDat <-  detailDat[rep(seq_len(nrow(detailDat)), each = nrow(detailDatCol)), ]
      df <- cbind(detailDat, detailDatCol)
    } else {# nrow(detailDatCol) == 0
      df <- detailDat
    }
    row.names(df) <- seq_len(nrow(df))
    df <- jsonlite::flatten(df)
    return(df)
  }

  ## Process result to data.frame
  if ("Accept" %in% names(usedArgs) && usedArgs[["Accept"]] != "application/json") {
    ## three possibilities "application/csv", "application/tsv" and "application/flapjack"
    switch(usedArgs[["Accept"]],
           "text/csv" = {
             dat <- read.csv(file = textConnection(cont),
                             stringsAsFactors = FALSE)
             colnames(dat) <- gsub(pattern = "\\.",
                                   replacement = ":",# or "|",
                                   x = colnames(dat))},
           "text/tsv" = {
             dat <- read.delim(file = textConnection(cont),
                               stringsAsFactors = FALSE)
             colnames(dat) <- gsub(pattern = "\\.",
                                   replacement = ":",# or "|",
                                   x = colnames(dat))},
           "application/flapjack" = {
             dat <- read.delim(file = textConnection(cont),
                               stringsAsFactors = FALSE)})
  } else {
    ## Parse JSON content into a list that consists of a metadata and result
    ## element
    contList <- jsonlite::fromJSON(txt = cont)
    ## Use only the result element from the content list (contList)
    resultList <- contList[["result"]]
    if (contList$metadata$pagination$totalCount>0 || "searchResultsDbId"%in%names(resultList)){
    ## Solve Issue that get_serverinfo has "calls" instead of "data"
    if ("calls" %in% names(resultList)) {
      names(resultList)[which(names(resultList) == "calls")] <- "data"
    }
    ## Determine payload variable value
    if ("data" %in% names(resultList)) {
      payload <- ifelse(test = length(resultList) == 1,
                        yes = "detail",
                        no =  "master/detail")
    } else {
      payload <- "master"
    }
    ## Handle and process payload variable
    switch(payload,
           "master" = {
           if (all(lengths(resultList) <= 1)) {
             ## use only lengths == 1
             master <- as.data.frame(resultList[lengths(resultList) == 1],
                                     stringsAsFactors = FALSE)
             dat <- master
           } else {
             master <- as.data.frame(resultList[lengths(resultList) == 1],
                                     stringsAsFactors = FALSE)
             tempmaster <- list()
             for (l1name in names(which(lengths(resultList) > 1))) {
               switch(class(resultList[[l1name]]),
                      "character" = {
                        tempmaster[[l1name]] <- paste(resultList[[l1name]],
                                                      collapse = ", ")
                      },
                      "data.frame" = {
                        for (i in seq_len(nrow(resultList[[l1name]]))) {
                          for (j in seq_along(resultList[[l1name]])) {
                            tempmaster[[paste(l1name,
                                              colnames(resultList[[l1name]][j]),
                                              i,
                                              sep = ".")]] <- resultList[[l1name]][i, j]
                          }
                        }
                      },
                      "list" = {
                        if (!l1name %in% c("coordinates", "imageLocation")) {# a list which may contain GEOJSON coordinates
                          if ("geoCoordinates" %in% names(resultList[[l1name]])) {
                            geoCoordList <- resultList[[l1name]]$geoCoordinates
                            resultList[[l1name]]$geoCoordinates <- NULL
                          }
                          templist <- as.list(data.frame(t(as.matrix(unlist(as.relistable(resultList[[l1name]])))),
                                                         stringsAsFactors = FALSE))
                          names(templist) <- paste(l1name, names(templist), sep = ".")
                          tempmaster <- c(tempmaster, templist)
                        } else {# when dealing with GeoJSON coordinates
                          master[[paste(l1name,
                                        "geometry",
                                        "type",
                                        sep = ".")]] <-
                            resultList[[l1name]]$geometry$type
                          switch(class(resultList[[l1name]]$geometry$coordinates),
                                 ## Point geometry
                                 "numeric" = {
                                   master[[paste(l1name,
                                                 "geometry",
                                                 "coordinates",
                                                 sep = ".")]][[1]] <-
                                     resultList[[l1name]]$geometry$coordinates
                                 },
                                 ## Polygon geometry with only exterior ring
                                 "array" = {
                                   temparray <- resultList[[l1name]]$geometry$coordinates
                                   master[[paste(l1name,
                                                 "geometry",
                                                 "coordinates",
                                                 sep = ".")]][[1]] <-
                                     matrix(data = temparray,
                                            nrow = prod(dim(temparray)[1:2]),
                                            ncol = dim(temparray)[3])
                                 },
                                 ## Polygon geometry with exterior ring and possibly multiple interior rings
                                 "list" = {
                                   master[[paste(l1name,
                                                 "geometry",
                                                 "coordinates",
                                                 sep = ".")]][1] <-
                                     resultList[[l1name]]$geometry$coordinates
                                 })
                          master[[paste(l1name, "type", sep = ".")]] <-
                            resultList[[l1name]]$type
                        }
                      })
             }
             if (length(master) == 0) {# master is empty
               dat <- as.data.frame(tempmaster, stringsAsFactors = FALSE)
             } else if (length(tempmaster) == 0) {# master has values and tempmaster is empty
               dat <- master
             } else {# master and tempmaster have values
               dat <- cbind(master, as.data.frame(tempmaster, stringsAsFactors = FALSE))
             }
             if (exists("geoCoordList")) {
               dat[[paste("observationUnitPosition",
                          "geoCoordinates",
                          "geometry",
                          "type",
                          sep = ".")]] <-
               geoCoordList$geometry$type
             switch(class(geoCoordList$geometry$coordinates),
                    ## Point geometry
                    "numeric" = {
                      dat[[paste("observationUnitPosition",
                                 "geoCoordinates",
                                 "geometry",
                                 "coordinates",
                                 sep = ".")]][[1]] <-
                        geoCoordList$geometry$coordinates
                    },
                    ## Polygon geometry with only exterior ring
                    "array" = {
                      temparray <- geoCoordList$geometry$coordinates
                      dat[[paste("observationUnitPosition",
                                 "geoCoordinates",
                                 "geometry",
                                 "coordinates",
                                 sep = ".")]][[1]] <-
                        matrix(data = temparray,
                               nrow = prod(dim(temparray)[1:2]),
                               ncol = dim(temparray)[3])
                    },
                    ## Polygon geometry with exterior ring and possibly multiple interior rings
                    "list" = {
                      dat[[paste("observationUnitPosition",
                                 "geoCoordinates",
                                 "geometry",
                                 "coordinates",
                                 sep = ".")]][1] <-
                        geoCoordList$geometry$coordinates
                    })
             dat[[paste("observationUnitPosition",
                        "geoCoordinates ",
                        "type",
                        sep = ".")]] <-
               geoCoordList$type
             }
           }
         },
           "detail" = {
           if (inherits(resultList[["data"]], what = "data.frame")) {
             detail <- as.data.frame(x = jsonlite::flatten(resultList[["data"]],
                                                           recursive = TRUE),
                                     stringsAsFactors = FALSE)
           } else {
             detail <- as.data.frame(x = resultList[["data"]],
                                     stringsAsFactors = FALSE)
           }
           namesListCols <- names(which(sapply(X = detail,
                                               FUN = inherits,
                                               what = "list")))
           for (colName in namesListCols) {
             ## list of data.frame
             if (all(sapply(X = detail[[colName]],
                            FUN = inherits,
                            what = "data.frame"))) {
               ## Determine rows with non-empty data.frames
               nonEmptyDetailRows <- which(lengths(detail[[colName]]) != 0)
               ## Create an empty data.frame with columnnames
               tempColNames <- names(jointDetail(detail[nonEmptyDetailRows[1], ], colName))
               tempDetail <- data.frame(matrix(ncol = length(tempColNames), nrow = 0))
               names(tempDetail) <- tempColNames
               nrows <- nrow(detail)
               if (nrows >= 1) {
                 for (i in seq_len(nrows)) {
                   if (i %in% nonEmptyDetailRows) {
                     nextRow <- jointDetail(detail[i, ], colName)
                     if (nrow(tempDetail) == 0) {
                       tempDetail <- nextRow
                     } else {
                       tempDetail <- dplyr::bind_rows(tempDetail, nextRow)
                     }
                     rm(nextRow)
                   } else {# i %in% emptyDetailRows
                     partRowDetail <- detail[i, -c(which(names(detail) == colName))]
                     extColNames <- setdiff(names(tempDetail), names(partRowDetail))
                     partRowExt <- data.frame(matrix(data = NA, nrow = 1, ncol = length(extColNames)))
                     names(partRowExt) <- extColNames
                     nextRow <- dplyr::bind_cols(partRowDetail, partRowExt)
                     if (nrow(tempDetail) == 0) {
                       tempDetail <- nextRow
                     } else {
                       tempDetail <- dplyr::bind_rows(tempDetail, nextRow)
                     }
                     rm(partRowDetail, partRowExt, extColNames, nextRow)
                   }
                 }
               }
               tempDetail[[colName]] <- NULL
               ## Remove duplicated rows
               tempDetail <- tempDetail[!duplicated(tempDetail), ]
               ## Row renumbering
               rownames(tempDetail) <- seq_len(nrow(tempDetail))
             }
             ## list of character
             if (all(sapply(X = detail[[colName]],
                            FUN = inherits,
                            what = "character"))) {
               detail[[colName]] <- sapply(X = detail[[colName]],
                                           FUN = paste,
                                           collapse = "; ")
             }
             ## list of list
             if (all(sapply(X = detail[[colName]],
                            FUN = inherits, what = "list"))) {
               if (all(lengths(detail[[colName]]) == 0)) {
                 ## list of empty lists
                 detail[[colName]] <- NULL
               }
             }
             ## list of geometry.coordinates
             if (all(tail(strsplit(colName, split = "\\.")[[1]], n = 2) %in% c("geometry", "coordinates"))) {
               tempCoord <- detail[[colName]]
               for (i in 1:length(tempCoord)) {
                 if (inherits(x = tempCoord[[i]], what = "array")) {# exterior ring only
                   tempCoord[[i]] <- matrix(tempCoord[[i]], prod(dim(tempCoord[[i]])[1:2]), dim(tempCoord[[i]])[3])
                 } else {# "numeric" point, "list of matrix" polygon with exterior and inner ring(s)
                   next()
                 }
               }
               detail[[colName]] <- tempCoord
             }
             if (exists("tempDetail")) {
               detail <- tempDetail
               rm(tempDetail)
             }
           }
           dat <- detail
         },
           "master/detail" = {##headerRow! e.g. /search/observationtables/{searchResultsDbId}
           ## Process master part
           masterList <- resultList[!names(resultList) %in% "data"]
           if (all(c("headerRow", "observationVariables") %in% names(masterList))) {
             headerRow <- c(masterList[["headerRow"]],
                            paste(masterList[["observationVariables"]][["observationVariableDbId"]],
                                  masterList[["observationVariables"]][["observationVariableName"]],
                                  sep = "|"))
           } else {
             if (!all(lengths(masterList) <= 1)) {# some masterList elements lengths > 1
               master <- masterList[lengths(masterList) == 1]
               tempmaster <- list()
               for (l1name in names(which(lengths(masterList) > 1))) {
                 switch(class(masterList[[l1name]]),
                        "character" = {
                          tempmaster[[l1name]] <- paste(masterList[[l1name]],
                                                        collapse = ", ")
                        },
                        "data.frame" = {
                          for (i in seq_len(nrow(masterList[[l1name]]))) {
                            for (j in seq_along(masterList[[l1name]])) {
                              tempmaster[[paste(l1name,
                                                colnames(masterList[[l1name]][j]),
                                                i,
                                                sep = ".")]] <- masterList[[l1name]][i, j]
                            }
                          }
                        },
                        "list" = {
                          templist <- as.list(data.frame(t(as.matrix(unlist(as.relistable(masterList[[l1name]])))),
                                                         stringsAsFactors = FALSE))
                          names(templist) <- paste(l1name, names(templist), sep = ".")
                          tempmaster <- c(tempmaster, templist)
                        })
               }
               if (length(master) == 0) {
                 master <- tempmaster
               } else {
                 master <- c(master, tempmaster)
               }
             } else {# masterList elements all length <= 1
               master <-  masterList[lengths(masterList) == 1]
             }
           }
           ## Process detail part
           if (inherits(resultList[["data"]], what = "data.frame")) {
             detail <- as.data.frame(x = jsonlite::flatten(resultList[["data"]],
                                                           recursive = TRUE),
                                     stringsAsFactors = FALSE)
           } else {
             detail <- as.data.frame(x = resultList[["data"]],
                                     stringsAsFactors = FALSE)
           }
           if (exists("headerRow")) {
             colnames(detail) <- headerRow
             namesListCols <- names(detail)
           } else if (!plyr::empty(detail)) {
             namesListCols <- names(which(sapply(X = detail,
                                                 FUN = inherits,
                                                 what = "list")))
           } else {
             namesListCols <- character(length = 0L)
           }
           for (colName in namesListCols) {
             ## list of data.frame
             if (all(sapply(X = detail[[colName]],
                            FUN = inherits,
                            what = "data.frame"))) {
               ## Determine rows with non-empty data.frames
               nonEmptyDetailRows <- which(lengths(detail[[colName]]) != 0)
               ## Create an empty data.frame with columnnames
               tempColNames <- names(jointDetail(detail[nonEmptyDetailRows[1], ], colName))
               tempDetail <- data.frame(matrix(ncol = length(tempColNames), nrow = 0))
               names(tempDetail) <- tempColNames
               nrows <- nrow(detail)
               if (nrows >= 1) {
                 for (i in seq_len(nrows)) {
                   if (i %in% nonEmptyDetailRows) {
                     nextRow <- jointDetail(detail[i, ], colName)
                     if (nrow(tempDetail) == 0) {
                       tempDetail <- nextRow
                     } else {
                       tempDetail <- dplyr::bind_rows(tempDetail, nextRow)
                     }
                     rm(nextRow)
                   } else {# i %in% emptyDetailRows
                     partRowDetail <- detail[i, -c(which(names(detail) == colName))]
                     extColNames <- setdiff(names(tempDetail), names(partRowDetail))
                     partRowExt <- data.frame(matrix(data = NA, nrow = 1, ncol = length(extColNames)))
                     names(partRowExt) <- extColNames
                     nextRow <- dplyr::bind_cols(partRowDetail, partRowExt)
                     if (nrow(tempDetail) == 0) {
                       tempDetail <- nextRow
                     } else {
                       tempDetail <- dplyr::bind_rows(tempDetail, nextRow)
                     }
                     rm(partRowDetail, partRowExt, extColNames, nextRow)
                   }
                 }
               }
               tempDetail[[colName]] <- NULL
               ## Remove duplicated rows
               tempDetail <- tempDetail[!duplicated(tempDetail), ]
               ## Row renumbering
               rownames(tempDetail) <- seq_len(nrow(tempDetail))
             }
             ## list of character
             if (all(sapply(X = detail[[colName]],
                            FUN = inherits,
                            what = "character"))) {
               detail[[colName]] <- sapply(X = detail[[colName]],
                                           FUN = paste,
                                           collapse = "; ")
             }
             ## list of list
             if (all(sapply(X = detail[[colName]],
                            FUN = inherits,
                            what = "list"))) {
               if (all(lengths(detail[[colName]]) == 0)) {
                 ## list of empty lists
                 detail[[colName]] <- NULL
               }
             }
             if (exists("tempDetail")) {
               detail <- tempDetail
               rm(tempDetail)
             }
           }
           if (exists("headerRow")) {
             dat <- detail
           } else {
             if (!plyr::empty(detail)) {
               dat <- cbind(as.data.frame(master, stringsAsFactors = FALSE), detail)
             } else {
               master[["data"]] <- as.character(NA)
               dat <- as.data.frame(master, stringsAsFactors = FALSE)
             }
             if ("resultList[[\"data\"]]" %in% colnames(dat)) {
               colnames(dat)[colnames(dat) == "resultList[[\"data\"]]"] <- "data"
             }
           }
         })
    } else {
      dat <- data.frame()
    }
    if (("metadata" %in% names(contList)) && !is.null(contList[["metadata"]])) {
      if (("pagination" %in% names(contList[["metadata"]])) &&
          !is.null(contList[["metadata"]][["pagination"]])) {
        attr(dat, "pagination") <- contList[["metadata"]][["pagination"]]
      }
    }
  }
  return(dat)
}
