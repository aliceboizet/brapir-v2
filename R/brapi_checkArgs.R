#' Helper function for checking required and used arguments
#'
#' @author Maikel Verouden
#'
#' @noRd
#' @keywords internal
brapi_checkArgs <- function(usedArgs, reqArgs) {
  ## Remove con list (connection object) from used arguments
  usedArgs[["con"]] <- NULL

  ## Check for the required arguments
  if (reqArgs != "") {
    ## Split when there is more than one required argument
    if (grepl(pattern = ", ", x = reqArgs)) {
      reqArgs <- strsplit(x = reqArgs, split = ", ")[[1]]
    }
    if (!all(reqArgs %in% names(usedArgs))) {
      stop('Required argument(s): "', paste(reqArgs[!reqArgs %in% names(usedArgs)], collapse = ", "), '" is/are undefined!')
    }
    reqArgs <- usedArgs[c(reqArgs)]
    for (i in names(reqArgs)) {
      ## Check if required argument is of type character
      if (!is.character(reqArgs[[i]])) {
        stop('Required argument: "', i, '" should be of type character, e.g. ', i, ' = "text".')
      }
      ## Check if required argument has more than zero characters
      if (!all(nchar(reqArgs[[i]]) > 0)) {
        stop('Required argument: "', i, '" should at least have length one.')
      }
      if (i == "Accept" && !(usedArgs[[i]] %in% c("application/json", "text/csv", "text/tsv", "application/flapjack"))) {
        stop('Required argument: "', i, '" should be one of: "application/json"|"text/csv"|"text/tsv"|"application/flapjack".')
      }
    }
    ## Delete required arguments from used arguments
    usedArgs[names(reqArgs)] <- NULL
  }

  ## Argument matching for certain character strings with limited options
  reqMatch <- c("dataType",
                "format",
                "listType",
                "sampleType",
                "sortBy",
                "sortOrder")
  if (any(reqMatch %in% names(usedArgs))) {
    reqMatch <- reqMatch[reqMatch %in% names(usedArgs)]
    for (i in reqMatch) {
      switch(i,
             "dataType" = {
               brapirv2:::brapi_matchArg(arg = usedArgs[[i]],
                                       choices =  c("",
                                                    "application/json",
                                                    "text/csv",
                                                    "text/tsv",
                                                    "application/flapjack"))},
             "format" = {
               brapirv2:::brapi_matchArg(arg = usedArgs[[i]],
                                       choices =  c(as.character(NA),
                                                    "csv",
                                                    "tsv",
                                                    "flapjack"))},
             "listType" = {
               brapirv2:::brapi_matchArg(arg = usedArgs[[i]],
                                       choices =  c("",
                                                    "germplasm",
                                                    "markers",
                                                    "observations",
                                                    "observationUnits",
                                                    "observationVariables",
                                                    "programs",
                                                    "samples",
                                                    "studies",
                                                    "trials"))},
             "sampleType" = {
               brapirv2:::brapi_matchArg(arg = usedArgs[[i]],
                                       choices = c("",
                                                   "DNA",
                                                   "RNA",
                                                   "Tissue"))},
             "sortBy" = {
               brapirv2:::brapi_matchArg(arg = usedArgs[[i]],
                                       choices =  c("",
                                                    "endDate",
                                                    "locationDbId",
                                                    "programDbId",
                                                    "programName",
                                                    "seasonDbId",
                                                    "startDate",
                                                    "studyDbId",
                                                    "studyLocation",
                                                    "studyName",
                                                    "studyTypeDbId",
                                                    "trialDbId",
                                                    "trialName"))},
             "sortOrder" = {
               brapirv2:::brapi_matchArg(arg = usedArgs[[i]],
                                       choices =  c("",
                                                    "asc",
                                                    "ASC",
                                                    "desc",
                                                    "DESC"))})
    }
    ## Delete matched arguments from used arguments
    usedArgs[reqMatch] <- NULL
  }

  ## Check if there are still used arguments
  if (length(usedArgs) > 0) {
    ## Checking used arguments
    for (i in names(usedArgs)) {
      ## Check for arguments which are of type integer
      if (i %in% c("column", "decimalPlaces", "end", "listSize", "numberOfSamples","page", "pageSize", "dimensionCallSetPage", "dimensionCallSetPageSize", "dimensionVariantPage", "dimensionVariantPageSize","plateIndex", "start", "year") || grepl(pattern = "(max)|(Max)|(min)|(Min)|(imageFileSize)|(imageHeight)|(imageWidth)", x = i)) {
        if (!is.numeric(usedArgs[[i]])) {
          stop('Argument: "', i, '" should be of type integer.')
        }
        if (i %in% c("decimalPlaces", "listSize", "plateIndex") && usedArgs[[i]] < 0) {
          stop('Argument: "', i, '" should be >= 0.')
        }
        if (i == "page" && usedArgs[[i]] < 0) {
          stop('Argument: "', i, '" should be >= 0 (0 is the default, meaning page number 1).')
        }
        if (i == "pageSize" && usedArgs[[i]] < 1) {
          stop('Argument: "', i, '" should be > 0.')
        }
        if (grepl(pattern = "(min)|(max)", x = tolower(i)) && ifelse(is.na(usedArgs[[i]]), FALSE, usedArgs[[i]] < 0)) {
          stop('Argument: "', i, '" should be >= 0.')
        }
        if (i %in% c("decimalPlaces", "listSize", "numberOfSamples", "page", "pageSize", "plateIndex", "year")) {
          usedArgs[[i]] <- NULL
        }
        next()
      }
      ## Check for arguments which are of type logical
      if (i %in% c("active", "expandHomozygotes", "includeObservations",
                   "includeSiblings", "includeSynonyms", "isDerived", "preview")) {
        if (!is.logical(usedArgs[[i]])) {
          stop('Argument: "', i, '" should be of type logical e.g. NA, TRUE or FALSE.')
        }
        usedArgs[[i]] <- NULL
        next()
      }
      ## Check for arguments which are of type list
      if (i %in% c("additionalInfo", "coordinates", "experimentalDesign",
                   "growthFacility", "imageLocation", "lastUpdate",
                   "method", "observationUnitPosition", "ontologyReference",
                   "requiredServiceInfo", "scale", "season",
                   "trait", "validValues", "pagination")) {
        if (!is.list(usedArgs[[i]])) {
          stop('Argument: "', i, '" should be provided as a list, see the help page on how the list should be constructed.')
        }
        usedArgs[[i]] <- NULL
        next()
      }
      ## Check for arguments which are of type data.frame or supplied as empty character vector of length 1
      if (i %in% c("contacts", "dataLinks", "datasetAuthorships",
                   "environmentParameters", "externalReferences",
                   "observationLevelRelationships", "observationLevels",
                   "publications", "treatments")) {
        if (!(is.character(usedArgs[[i]]) && usedArgs[[i]] == "" || is.data.frame(usedArgs[[i]]))) {
          stop('Argument: "', i, '" should be supplied as an empty character or as a data.frame, see the help page on how the data.frame should be constructed.')
        }
        usedArgs[[i]] <- NULL
        next()
      }
      ## Check any other argument to be of type character
      if (!is.character(usedArgs[[i]])) {
        stop('Argument: "', i, '" should be of type character e.g. "text".')
      }
      usedArgs[[i]] <- NULL
    }
    ## Check, when both min and max are present in used arguments and both are
    ## not NA, that min is smaller than or equal to max
    if (ifelse(all(c("min", "max") %in% names(usedArgs)),
               (!is.na(usedArgs[["min"]]) && !is.na(usedArgs[["max"]])),
               FALSE)) {
      if (!usedArgs[["min"]] <= usedArgs[["max"]]) {
        stop('Argument: "min" can never be larger than argument: "max".')
      }
    }
    if (ifelse(all(c("imageFileSizeMin", "imageFileSizeMax") %in% names(usedArgs)),
               (!is.na(usedArgs[["imageFileSizeMin"]]) && !is.na(usedArgs[["imageFileSizeMax"]])),
               FALSE)) {
      if (!usedArgs[["imageFileSizeMin"]] <= usedArgs[["imageFileSizeMax"]]) {
        stop('Argument: "imageFileSizeMin" can never be larger than argument: "imageFileSizeMax".')
      }
    }
    if (ifelse(all(c("imageHeightMin", "imageHeightMax") %in% names(usedArgs)),
               (!is.na(usedArgs[["imageHeightMin"]]) && !is.na(usedArgs[["imageHeightMax"]])),
               FALSE)) {
      if (!usedArgs[["imageHeightMin"]] <= usedArgs[["imageHeightMax"]]) {
        stop('Argument: "imageHeightMin" can never be larger than argument: "imageHeightMax".')
      }
    }
    if (ifelse(all(c("imageWidthMin", "imageWidthMax") %in% names(usedArgs)),
               (!is.na(usedArgs[["imageWidthMin"]]) && !is.na(usedArgs[["imageWidthMax"]])),
               FALSE)) {
      if (!usedArgs[["imageWidthMin"]] <= usedArgs[["imageWidthMax"]]) {
        stop('Argument: "imageWidthMin" can never be larger than argument: "imageWidthMax".')
      }
    }
  }
}
