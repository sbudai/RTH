
.RTHGetAllPages <- function(url) {
  token <- get("token", envir = cacheEnv)
  r <- httr::GET(url, httr::add_headers(prefer = "respond-async", Authorization = token))
  httr::stop_for_status(r)
  rc <- httr::content(r, "parsed", "application/json", encoding = "UTF-8")
  # Check if there is a next link
  if (!is.null(rc[["@odata.nextlink"]])) {
    # Call the function again, using next link.
    nurl <- rc[["@odata.nextlink"]]
    b <- .RTHGetAllPages(nurl)
    # Merge the result
    for (i in 1:length(b[["value"]])) {
      rc[["value"]][[length(rc[["value"]]) + 1]] <- b[["value"]][[i]]
    }
  }
  # Remove next link to avoid confusion
  rc[["@odata.nextlink"]] <- NULL
  return(rc)
}

#' Request authentication token
#' @param uname DSS username
#' @param pword DSS password
#' 
#' @return An authentication token that must be applied to all requests
#' 
#' @export
RTHLogin <- function(uname, pword) {
  cacheEnv <- new.env()
  url <- "https://selectapi.datascope.refinitiv.com/RestApi/v1/Authentication/RequestToken"
  b <- list(Credentials = list(Username = jsonlite::unbox(uname), Password = jsonlite::unbox(pword)))
  r <- httr::POST(url, httr::add_headers(prefer = "respond-async"), httr::content_type_json(), body = b, encode = "json")
  httr::stop_for_status(r)
  rc <- httr::content(r, as = "parsed", type = "application/json", encoding = "UTF-8")
  token <- sprintf(fmt = "Token %s", rc$value)
  RTHSetToken(token)
  return(invisible(token))
}

#' Set authentication token
#' @param token authentication token
#' 
#' @export
RTHSetToken <- function(token) {
  assign("token", value = token, envir = cacheEnv)
}

#' Retrieves a single User information.
#' @param uname DSS username
#' 
#' @return Return list of ID, Name, Phone, and Email
#' 
#' @export
RTHUserInfo <- function(uname) {
  url <- sprintf(fmt = "https://selectapi.datascope.refinitiv.com/RestApi/v1/Users/Users(%s)", uname)
  token <- get("token", envir = cacheEnv)
  r <- httr::GET(url, httr::add_headers(prefer = "respond-async", Authorization = token))
  httr::stop_for_status(r)
  httr::content(r, "parsed", "application/json", encoding = "UTF-8")
}

#' Retrieve the list of all user packages, i.e. packages to which I am entitled (for all subscriptions)
#' @return Return the list of user package Id, user package name and the corresponding subscription name
#' 
#' @export
RTHUserPackages <- function() {
  url <- "https://selectapi.datascope.refinitiv.com/RestApi/v1/StandardExtractions/UserPackages"
  .RTHGetAllPages(url)
}

#' List all user package deliveries (data files) for one package.
#' @param PackageId User package Id. Usually from RTHUserPackages()
#' 
#' @return Return the list of user package delivery Id
#' 
#' @export
RTHUserPackageDeliveriesByPackageId <- function(packageId) {
  url <- sprintf(fmt = "https://selectapi.datascope.refinitiv.com/RestApi/v1/StandardExtractions/UserPackageDeliveryGetUserPackageDeliveriesByPackageId(PackageId='%s')", packageId)
  .RTHGetAllPages(url)
}

#' List all user package deliveries (data files) for one package.
#' @param subscriptionId id of the subscription
#' @param fromDate fromDate
#' @param toDate toDate
#' 
#' @return Returns the list of deliveries by date range
#' 
#' @export
RTHUserPackageDeliveriesByDateRange <- function(subscriptionId, fromDate, toDate) {
  url <- sprintf(fmt = "https://selectapi.datascope.refinitiv.com/RestApi/v1/StandardExtractions/UserPackageDeliveryGetUserPackageDeliveriesByDateRange(SubscriptionId='%s,FromDate=%s,ToDate=%s')", subscriptionId, fromDate, toDate)
  .RTHGetAllPages(url)
}

#' Get the user package delivery and save it to disk
#' @param packageDeliveryId User package delivery Id
#' @param path Path to content to.
#' @param overwrite Will only overwrite ex
#' 
#' @export
RTHGetUserPackageDeliveries <- function(packageDeliveryId, path, overwrite = FALSE, aws = FALSE) {
  url <- sprintf(fmt = "https://selectapi.datascope.refinitiv.com/RestApi/v1/StandardExtractions/UserPackageDeliveries('%s')/$value", packageDeliveryId)
  token <- get("token", envir = cacheEnv)
  r <- httr::GET(url, httr::add_headers(prefer = "respond-async", Authorization = token), if (aws) {
    httr::add_headers("X-Direct-Download" = "true")
  }, httr::config(http_content_decoding = 0, followlocation = 0), httr::write_disk(path, overwrite), httr::progress())
  if (httr::status_code(r) == 302) {
    r2 <- httr::GET(r$headers$location, httr::add_headers(prefer = "respond-async"), httr::config(http_content_decoding = 0, followlocation = 0), httr::write_disk(path, overwrite), httr::progress())
    httr::stop_for_status(r2)
    return(r2)
  }
  httr::stop_for_status(r)
  return(r)
}

#' Search for historical instruments given an instrument identifier.
#' Return instruments may be currently active, or inactive 'historical only' instruments.
#' @param identifier Instrument identifier
#' @param startDateTime The range's start date and time. The format is yyyy-mm-ddThh:mm:ss.sssZ
#' @param endDateTime The range's end date and time. The format is yyyy-mm-ddThh:mm:ss.sssZ
#' @param identifierType The type of identifier. Supported types are Ric, Isin, Cusip, Sedol. Search will look for the identifier in all supported types when not specified.
#' @param resultsBy Determines what information is returned for each found RIC. By RIC: Returns information purely based on the RIC history. By Entity: Returns information based on the entity associated with the RIC on the end date of the Range. This will cause RIC rename history to be returned. Defaults to searching by RIC when not specified.
#' 
#' @export
RTHHistoricalSearch <- function(identifier, startDateTime, endDateTime, identifierType = NULL, resultsBy = NULL) {
  url <- "https://selectapi.datascope.refinitiv.com/RestApi/v1/Search/HistoricalSearch"
  b <- list(
    Request = list(
      Identifier = identifier,
      Range = list(
        Start = startDateTime,
        End = endDateTime
      )
    )
  )
  identifierTypeArg <- match.arg(identifierType, c(NULL, "Ric", "Isin", "Cusip", "Sedol"))
  if (!is.null(identifierType)) {
    b[["Request"]][["IdentifierType"]] <- jsonlite::unbox(identifierTypeArg)
  }
  resultsByArg <- match.arg(resultsBy, c(NULL, "Ric", "Entity"))
  if (!is.null(identifierType)) {
    b[["Request"]][["ResultsBy"]] <- jsonlite::unbox(resultsByArg)
  }
  token <- get("token", envir = cacheEnv)
  r <- httr::POST(url, httr::add_headers(prefer = "respond-async", Authorization = token), httr::content_type_json(), body = b, encode = "json")
  httr::warn_for_status(r)
  httr::content(r, "parsed", "application/json", encoding = "UTF-8")
}

#' Retrieve FID reference history events for a set of RICs in a specified date range.
#' Returns Collection Of ReferenceHistoryResult
#' @param ricList The RIC identifiers to return reference history for.
#' @param startDateTime The range's start date and time. The format is yyyy-mm-ddThh:mm:ss.sssZ
#' @param endDateTime The range's end date and time. The format is yyyy-mm-ddThh:mm:ss.sssZ
#' 
#' @export
RTHReferenceHistory <- function(ricList, startDateTime, endDateTime) {
  url <- "https://selectapi.datascope.refinitiv.com/RestApi/v1/Search/ReferenceHistory"
  b <- list(
    Request = list(
      Rics = ricList,
      Range = list(
        Start = jsonlite::unbox(startDateTime),
        End = jsonlite::unbox(endDateTime)
      )
    )
  )
  b <- jsonlite::toJSON(b)
  token <- get("token", envir = cacheEnv)
  r <- httr::POST(url, httr::add_headers(prefer = "respond-async", Authorization = token), httr::content_type_json(), body = b)
  httr::warn_for_status(r)
  httr::content(r, "parsed", "application/json", encoding = "UTF-8")
}

#' Returns a list of valid ContentFieldTypes for the report template type.
#' @param reportTemplateTypes Available template type for RTH are "TickHistoryTimeAndSales","TickHistoryMarketDepth", and "TickHistoryIntradaySummaries".
#' 
#' @export
RTHGetValidContentFieldTypes <- function(reportTemplateTypes = c("TickHistoryTimeAndSales", "TickHistoryMarketDepth", "TickHistoryIntradaySummaries")) {
  reportTemplateTypesArg <- match.arg(reportTemplateTypes)
  url <- sprintf(fmt = "https://selectapi.datascope.refinitiv.com/RestApi/v1/Extractions/GetValidContentFieldTypes(ReportTemplateType=DataScope.Select.Api.Extractions.ReportTemplates.ReportTemplateTypes'%s')", reportTemplateTypesArg)
  token <- get("token", envir = cacheEnv)
  r <- httr::GET(url, httr::add_headers(prefer = "respond-async", Authorization = token))
  httr::stop_for_status(r)
  httr::content(r, "parsed", "application/json", encoding = "UTF-8")
}

RTHRawExtractionResults <- function(jobId, path, overwrite = TRUE) {
  url <- sprintf(fmt = "https://selectapi.datascope.refinitiv.com/RestApi/v1/Extractions/RawExtractionResults('%s')/$value", jobId)
  token <- get("token", envir = cacheEnv)
  r <- httr::GET(url, httr::add_headers(prefer = "respond-async", Authorization = token), httr::config(http_content_decoding = 0), httr::write_disk(path, overwrite), httr::progress())
  httr::stop_for_status(r)
  return(r)
}

#' Performs an on demand extraction returning the raw results as a stream if the response is available in a short amount of time,
#' otherwise the server accepted the extracting and response with a monitor URL.
#' In the later case, You must poll the extraction status with RTHCheckRequestStatus.
#'
#' The result format is the native/raw result from the underlying extractor (usually csv).
#' @param b JSON request body. See REST API Reference Tree for format.
#' @param path Path to content to.
#' @param overwrite Will only overwrite existing path if TRUE.
#' 
#' @export
RTHExtractRaw <- function(b, path, overwrite = FALSE) {
  url <- "https://selectapi.datascope.refinitiv.com/RestApi/v1/Extractions/ExtractRaw"
  token <- get("token", envir = cacheEnv)
  r <- httr::POST(url, httr::add_headers(prefer = "respond-async", Authorization = token), httr::content_type_json(), body = b, encode = "json")
  if (httr::status_code(r) == 202) {
    message("The request has been accepted but has not yet completed executing asynchronously.\r\nReturn monitor URL\r\n", r$headers$location)
    return(invisible(r$headers$location))
  } else if (httr::status_code(r) == 200) {
    rc <- httr::content(r, "parsed", "application/json", encoding = "UTF-8")
    message(rc$Notes)
    return(RTHRawExtractionResults(rc$JobId, path, overwrite))
  } else {
    httr::warn_for_status(r)
    return(httr::content(r, "parsed", "application/json", encoding = "UTF-8"))
  }
}

#' Polling the extraction status.
#' On Demand extraction requests are executed as soon as possible.
#' However, There is no guarantee on the delivery time.
#' If the previous request returned a monitor URL, RTHCheckRequestStatus must be executed until it returns the result.
#' @param location The monitor URL.
#' @param path Path to content to.
#' @param overwrite Will only overwrite existing path if TRUE.
#' 
#' @export
RTHCheckRequestStatus <- function(location, path, overwrite = FALSE) {
  token <- get("token", envir = cacheEnv)
  r <- httr::GET(location, httr::add_headers(prefer = "respond-async", Authorization = token))
  if (httr::status_code(r) == 202) {
    message("The request has not yet completed executing asynchronously.\r\nPlease wait a bit and check the request status again.\r\n")
    return(invisible(r$headers$location))
  } else if (httr::status_code(r) == 200) {
    rc <- httr::content(r, "parsed", "application/json", encoding = "UTF-8")
    message(rc$Notes)
    return(RTHRawExtractionResults(rc$JobId, path, overwrite))
  } else {
    warn_for_status(r)
    return(httr::content(r, "parsed", "application/json", encoding = "UTF-8"))
  }
}


