#' Request and cache authentication token
#'
#' @param uname Refinitiv Tick History DataScope Select username
#' @param pword Refinitiv Tick History DataScope Select password
#'
#' @return An authentication token invisibly and cache it for further usage.
#'
#' @export
get_token <- function(
    uname = Sys.getenv("rth_dss_username"),
    pword = Sys.getenv("rth_dss_password")) {
  stopifnot("Valid RTH DSS username is not provided!" = uname != "")
  stopifnot("Valid RTH DSS password is not provided!" = pword != "")
  suf <- "Authentication/RequestToken"
  url <- sprintf(fmt = get(x = "api", envir = cache_env), suf)
  body <- list(
    Credentials = list(
      Username = jsonlite::unbox(uname),
      Password = jsonlite::unbox(pword)
    )
  )
  res <- httr::POST(
    url,
    httr::add_headers(prefer = "respond-async"),
    httr::content_type_json(),
    body = jsonlite::toJSON(body),
    encode = "json"
  )
  httr::stop_for_status(res)
  resc <- httr::content(
    res,
    as = "parsed",
    type = "application/json",
    encoding = "UTF-8"
  )
  assign(
    "token",
    value = sprintf(fmt = "Token %s", resc$value),
    envir = cache_env
  )
  message(
    "The requested new token has cached."
  )
  invisible(cache_env$token)
}


#' Retrieves user information
#' @param uname Refinitiv Tick History DataScope Select username
#'
#' @return Return list of UserId, UserName, Email and Phone
#'
#' @export
get_user_info <- function(uname = Sys.getenv("rth_dss_username")) {
  stopifnot("Valid RTH DSS username is not provided!" = uname != "")
  suf <- sprintf(fmt = "Users/Users(%s)", uname)
  url <- sprintf(fmt = get(x = "api", envir = cache_env), suf)
  res <- httr::GET(
    url,
    httr::add_headers(
      prefer = "respond-async",
      Authorization = get("token", envir = cache_env)
    )
  )
  httr::stop_for_status(res)
  resc <- httr::content(
    res,
    as = "parsed",
    type = "application/json",
    encoding = "UTF-8"
  )
  resc$`@odata.context` <- NULL
  purrr::compact(resc) |> tibble::as_tibble()
}


#' Retrieves all Subscriptions of the Authorized User.
#'
#' @return Returns Collection of all Subscriptions
#'
#' @export
get_all_subscriptions <- function() {
  suf <- "StandardExtractions/Subscriptions"
  url <- sprintf(fmt = get(x = "api", envir = cache_env), suf)
  res <- httr::GET(
    url,
    httr::add_headers(
      prefer = "respond-async",
      Authorization = get("token", envir = cache_env)
    )
  )
  httr::stop_for_status(res)
  resc <- httr::content(
    res,
    as = "parsed",
    type = "application/json",
    encoding = "UTF-8"
  )
  purrr::map_dfr(
    purrr::pluck(resc, "value"),
    ~ purrr::compact(.x) |>
      unlist() |>
      t() |>
      tibble::as_tibble()
  )
}


#' Retrieve the user's quota information
#'
#' Quota limitations apply at the client level.
#' All the users pertaining to a client have the same quota restrictions.
#'
#' @return Returns the quota summary for the logged in user.
#'
#' @export
get_quota_info <- function() {
  suf <- "Quota/GetQuotaInformation"
  url <- sprintf(fmt = get(x = "api", envir = cache_env), suf)
  resc <- get_all_pages(url)
  purrr::pluck(resc, "value") |>
    purrr::compact() |>
    unlist() |>
    t() |>
    tibble::as_tibble()
}


#' Retrieve the authorized and extracted RIC list
#'
#' If the quota service sees a new RIC,
#' it gets added to the authorized list,
#' unless the cummulative quota has been reached for that asset class
#'
#' @return Returns the RIC list that the user is authorized to use.
#'
#' @export
get_ric_list <- function(category_code = "Futures") {
  suf <- sprintf(
    fmt = "Quota/GetAuthorizedRicList(CategoryCode='%s')",
    category_code
  )
  url <- sprintf(fmt = get(x = "api", envir = cache_env), suf)
  resc <- get_all_pages(url)
  purrr::map_dfr(
    purrr::pluck(resc, "value"),
    ~ purrr::compact(.x) |>
      unlist() |>
      t() |>
      tibble::as_tibble()
  )
}


#' Retrieve the active jobs
#'
#' @return Returns the in flight jobs for all asynchronous requests.
#'
#' @export
get_active_jobs <- function() {
  suf <- "Jobs/JobGetActive"
  url <- sprintf(fmt = get(x = "api", envir = cache_env), suf)
  resc <- get_all_pages(url)
  purrr::map_dfr(
    purrr::pluck(resc, "value"),
    ~ purrr::compact(.x) |>
      unlist() |>
      t() |>
      tibble::as_tibble()
  )
}


#' Retrieve the completed jobs
#'
#' @return Returns the ready jobs for all asynchronous requests.
#'
#' @export
get_completed_jobs <- function() {
  suf <- "Jobs/JobGetCompleted"
  url <- sprintf(fmt = get(x = "api", envir = cache_env), suf)
  resc <- get_all_pages(url)
  purrr::map_dfr(
    purrr::pluck(resc, "value"),
    ~ purrr::compact(.x) |>
      unlist() |>
      t() |>
      tibble::as_tibble()
  )
}


#' Retrieve the extracted file details by job_id
#'
#' @param job_id the job ID of the extracted file
#'
#' @return Returns the details of the extracted file.
#'
#' @export
get_extr_file_by_job_id <- function(job_id) {
  suf <- sprintf(
    fmt = "Extractions/ExtractedFileByJobId(JobId='%s')",
    job_id
  )
  url <- sprintf(fmt = get(x = "api", envir = cache_env), suf)
  resc <- get_all_pages(url)
  purrr::map_dfr(
    purrr::pluck(resc, "value"),
    ~ purrr::compact(.x) |>
      unlist() |>
      t() |>
      tibble::as_tibble()
  )
}


#' Retrieves the instrument lists of the authenticated user
#'
#' @return the instrument lists with id, name and timestamps
#' Note that the search for the specified list name is case sensitive.
#'
#' @export
get_instr_lists <- function() {
  suf <- "Extractions/InstrumentLists"
  url <- sprintf(fmt = get(x = "api", envir = cache_env), suf)
  resc <- get_all_pages(url)
  purrr::map_dfr(
    purrr::pluck(resc, "value"),
    ~ purrr::compact(.x) |>
      unlist() |>
      t() |>
      tibble::as_tibble()
  )
}


#' Retrieves the instrument list matching the specified name
#'
#' @param instr_list_name name of the instrument list
#'
#' @return the instrument with list id, name and timestamps
#' Note that the search for the specified list name is case sensitive.
#'
#' @export
get_instr_list_details <- function(instr_list_name) {
  suf <- sprintf(
    fmt = "Extractions/InstrumentListGetByName(ListName='%s')",
    instr_list_name
  )
  url <- sprintf(fmt = get(x = "api", envir = cache_env), suf)
  resc <- get_all_pages(url)
  resc$`@odata.context` <- NULL
  purrr::compact(resc) |>
    unlist() |>
    t() |>
    tibble::as_tibble()
}


#' Retrieve the list of all packages
#'
#' @return Return the list of package id, subscription id,
#' package name and description
#'
#' @export
get_package_list <- function() {
  suf <- "StandardExtractions/Packages"
  url <- sprintf(fmt = get(x = "api", envir = cache_env), suf)
  resc <- get_all_pages(url)
  purrr::map_dfr(
    purrr::pluck(resc, "value"),
    ~ purrr::compact(.x) |>
      unlist() |>
      t() |>
      tibble::as_tibble()
  )
}


#' Search for historical instruments given an instrument identifier.
#'
#' @param identifier Instrument identifier
#' @param identifier_type The type of identifier.
#' Supported types are ChainRIC, RIC, ISIN, CUSIP, SEDOL.
#' Search will look for the identifier in all supported
#' types when not specified.
#' @param start_datetime The range's start date and time in POSIXct format.
#' @param end_datetime The range's end date and time in POSIXct format.
#' @param results_by Determines what information is returned
#' for each found RIC.
#' "Ric": Returns information purely based on the RIC history.
#' "Entity": Returns information based on the entity associated
#' with the "RIC" on the end date of the range.
#' This will cause RIC rename history to be returned.
#' Defaults to "Ric".
#'
#' @return Return instruments may be currently active, or inactive
#' 'historical only' instruments.
#'
#' @export
historical_search <- function(
    identifier = "0#N1BM:",
    identifier_type = "ChainRIC",
    start_datetime = Sys.time() - 72 * 60 * 60,
    end_datetime = Sys.time(),
    results_by = "Ric") {
  suf <- "Search/HistoricalSearch"
  url <- sprintf(fmt = get(x = "api", envir = cache_env), suf)
  identifier_type_arg <- match.arg(
    arg = identifier_type,
    choices = c(NULL, "ChainRIC", "RIC", "ISIN", "CUSIP", "SEDOL")
  )
  body <- list(
    Request = list(
      Identifier = jsonlite::unbox(identifier),
      IdentifierType = jsonlite::unbox(identifier_type_arg),
      Range = list(
        Start = jsonlite::unbox(req_posixct_format(start_datetime)),
        End = jsonlite::unbox(req_posixct_format(end_datetime))
      )
    )
  )
  res <- httr::POST(url,
    httr::add_headers(
      prefer = "respond-async",
      Authorization = get("token", envir = cache_env)
    ),
    httr::content_type_json(),
    body = jsonlite::toJSON(body),
    encode = "json",
    httr::progress()
  )
  httr::warn_for_status(res)
  resc <- httr::content(
    res,
    as = "parsed",
    type = "application/json",
    encoding = "UTF-8"
  )
  purrr::map_dfr(
    purrr::pluck(resc, "value"),
    ~ purrr::compact(.x) |>
      unlist() |>
      t() |>
      tibble::as_tibble()
  )
}


#' Retrieve FID reference history events for a set of RICs
#' in a specified date range.
#'
#' @param ric_list the RIC identifiers to return reference history for
#' @param start_datetime the range's start date and time in POSIXct format
#' @param end_datetime the range's end date and time in POSIXct format
#'
#' @return list of ReferenceHistoryResult tables
#'
#' @export
reference_history <- function(
    ric_list = c("0#N1BM:", "0#NGMM:"),
    start_datetime = Sys.time() - 72 * 60 * 60,
    end_datetime = Sys.time()) {
  suf <- "Search/ReferenceHistory"
  url <- sprintf(fmt = get(x = "api", envir = cache_env), suf)
  body <- list(
    Request = list(
      Rics = ric_list,
      Range = list(
        Start = jsonlite::unbox(req_posixct_format(start_datetime)),
        End = jsonlite::unbox(req_posixct_format(end_datetime))
      ),
      ResultsBy = jsonlite::unbox("Entity")
    )
  )
  res <- httr::POST(
    url,
    httr::add_headers(
      prefer = "respond-async",
      Authorization = get("token", envir = cache_env)
    ),
    httr::content_type_json(),
    body = jsonlite::toJSON(body),
    encode = "json",
    httr::progress()
  )
  httr::warn_for_status(res)
  resc <- httr::content(
    res,
    as = "parsed",
    type = "application/json",
    encoding = "UTF-8"
  )
  purrr::map(
    purrr::pluck(resc, "value"),
    ~ cbind(
      .x["Ric"],
      purrr::map_dfr(
        purrr::pluck(.x, "HistoryEvents"),
        tibble::as_tibble
      )
    )
  )
}


#' Retrieve the list of valid ContentFieldTypes for the report template type.
#'
#' @param report_template_type Available template type for RTH are
#' "TickHistoryTimeAndSales","TickHistoryMarketDepth",
#' and "TickHistoryIntradaySummaries".
#'
#' @return Returns a list of valid content field names for
#' the report template type.
#'
#' @export
get_valid_content_field_names <- function(report_template_type = "TickHistoryTimeAndSales") {
  report_template_type_arg <- match.arg(
    arg = report_template_type,
    choices = c(
      "TickHistoryTimeAndSales",
      "TickHistoryMarketDepth",
      "TickHistoryIntradaySummaries"
    )
  )
  suf <- sprintf(
    fmt = "Extractions/GetValidContentFieldTypes(ReportTemplateType=%s'%s')",
    "DataScope.Select.Api.Extractions.ReportTemplates.ReportTemplateTypes",
    report_template_type_arg
  )
  url <- sprintf(fmt = get(x = "api", envir = cache_env), suf)
  res <- httr::GET(
    url,
    httr::add_headers(
      prefer = "respond-async",
      Authorization = get("token", envir = cache_env)
    ),
    httr::progress()
  )
  httr::stop_for_status(res)
  resc <- httr::content(
    res,
    as = "parsed",
    type = "application/json",
    encoding = "UTF-8"
  )
  resc <- purrr::map_dfr(
    purrr::pluck(resc, "value"),
    ~ purrr::compact(.x) |>
      unlist() |>
      t() |>
      tibble::as_tibble()
  )
  attr(resc, "template_type") <- report_template_type_arg
  return(resc)
}


#' Extract Tick History Time And Sales on demand
#'
#' If the response is available instantly, it performs an
#' on demand extraction which returns the raw results.
#' Otherwise the server accepts the request and provides "monitor URL".
#' In the latter case the extraction status/end result can be polled
#' with poll_async_status() function using the "monitor URL" as
#' an argument.
#'
#' @param content_field_names set the field names of the result table
#' @param identifier the required RIC or ChainRIC value
#' @param identifier_type the corresponding type of the identifier
#' @param appl_corr_and_canc if TRUE, the result table will display the
#' original and the corrected values as well
#' @param timestamp_in timestamps will be displayed in "GmtUtc" or
#' "LocalExchangeTime"
#' @param query_start_datetime the range's start date and time in POSIXct format
#' @param query_end_datetime the range's end date and time in POSIXct format
#' @param display_source_ric if TRUE, the result table will have an extra
#' field which indicates the underlying instrument
#' @param result_path path of the prospective result file,
#' the extension must be ".csv.gz"
#' @param overwrite should overwrite existing result file TRUE/FALSE
#'
#' @return The result format is the native/raw result from the
#' underlying extractor (usually csv).
#'
#' @export
extract_rth_tas <- function(
    content_field_names = grep(
      pattern = "^Trade.+(Price|Size)",
      x = get_valid_content_field_names(
        report_template_type = "TickHistoryTimeAndSales"
      )$Name,
      value = TRUE
    ),
    identifier = "0#TFMB:",
    identifier_type = "ChainRIC",
    appl_corr_and_canc = TRUE,
    timestamp_in = "GmtUtc",
    query_start_datetime = Sys.time() - 72 * 60 * 60,
    query_end_datetime = Sys.time() - 48 * 60 * 60,
    display_source_ric = TRUE,
    result_path = fs::path_home(
      "Downloads",
      sprintf(
        fmt = "RTH_TAS_extractum_%i",
        as.integer(Sys.time())
      ),
      ext = "csv.gz"
    ),
    overwrite = FALSE) {
  suf <- "Extractions/ExtractRaw"
  url <- sprintf(fmt = get(x = "api", envir = cache_env), suf)
  body <- list(
    ExtractionRequest = list(
      `@odata.type` = jsonlite::unbox(
        paste(
          "#DataScope",
          "Select",
          "Api",
          "Extractions",
          "ExtractionRequests",
          "TickHistoryTimeAndSalesExtractionRequest",
          sep = "."
        )
      ),
      ContentFieldNames = lapply(content_field_names, jsonlite::unbox),
      IdentifierList = list(
        `@odata.type` = jsonlite::unbox(
          paste(
            "#DataScope",
            "Select",
            "Api",
            "Extractions",
            "ExtractionRequests",
            "InstrumentIdentifierList",
            sep = "."
          )
        ),
        InstrumentIdentifiers = list(
          list(
            Identifier = jsonlite::unbox(identifier),
            IdentifierType = jsonlite::unbox(identifier_type)
          )
        ),
        ValidationOptions = jsonlite::unbox(NA),
        UseUserPreferencesForValidationOptions = jsonlite::unbox(FALSE)
      ),
      Condition = list(
        MessageTimeStampIn = jsonlite::unbox(timestamp_in),
        ApplyCorrectionsAndCancellations = jsonlite::unbox(appl_corr_and_canc),
        ReportDateRangeType = jsonlite::unbox("Range"),
        QueryStartDate = jsonlite::unbox(
          req_posixct_format(query_start_datetime)
        ),
        QueryEndDate = jsonlite::unbox(
          req_posixct_format(query_end_datetime)
        ),
        DisplaySourceRIC = jsonlite::unbox(display_source_ric)
      )
    )
  )
  res <- httr::POST(
    url,
    httr::add_headers(
      prefer = "respond-async",
      Authorization = get("token", envir = cache_env)
    ),
    httr::content_type_json(),
    body = jsonlite::toJSON(body),
    encode = "json",
    httr::progress()
  )
  if (httr::status_code(res) == 202) {
    message(
      "The request has been accepted but not yet completed, ",
      "executing asynchronously.\r\nmonitor URL: \r\n",
      res$headers$location
    )
    return(invisible(res$headers$location))
  } else {
    httr::warn_for_status(res)
    resc <- httr::content(
      res,
      as = "parsed",
      type = "application/json",
      encoding = "UTF-8"
    )
    if (httr::status_code(res) == 200) {
      message(resc$Notes)
      return(
        raw_extraction(
          job_id = resc$JobId,
          path = result_path,
          overwrite = overwrite
        )
      )
    } else {
      return(resc)
    }
  }
}


#' Extract Tick History Market Depth on demand
#'
#' If the response is available instantly, it performs an
#' on demand extraction which returns the raw results.
#' Otherwise the server accepts the request and provides "monitor URL".
#' In the latter case the extraction status/end result can be polled
#' with poll_async_status() function using the "monitor URL" as
#' an argument.
#'
#' @param identifier the required RIC or ChainRIC value
#' @param identifier_type the corresponding type of the identifier
#' @param md_view market depth view, value can be:
#' "LegacyLevel2", "NormalizedLL2", "RawMarketByOrder", "RawMarketByPrice" or
#' "RawMarketMaker"
#' @param number_of_levels in case of "NormalizedLL2" view, defaults to 10
#' @param timestamp_in timestamps will be displayed in "GmtUtc" or
#' "LocalExchangeTime"
#' @param query_start_datetime the range's start date and time in POSIXct format
#' @param query_end_datetime the range's end date and time in POSIXct format
#' @param display_source_ric if TRUE, the result table will have an extra
#' field which indicates the underlying instrument
#' @param result_path path of the prospective result file,
#' the extension must be ".csv.gz"
#' @param overwrite should overwrite existing result file TRUE/FALSE
#'
#' @return The result format is the native/raw result from the
#' underlying extractor (usually csv).
#'
#' @export
extract_rth_md <- function(
    identifier = "0#TFMB:",
    identifier_type = "ChainRIC",
    timestamp_in = "GmtUtc",
    query_start_datetime = Sys.time() - 72 * 60 * 60,
    query_end_datetime = Sys.time() - 48 * 60 * 60,
    md_view = "RawMarketByPrice",
    number_of_levels = 10,
    display_source_ric = TRUE,
    result_path = fs::path_home(
      "Downloads",
      sprintf(fmt = "RTH_TAS_extractum_%i", as.integer(Sys.time())),
      ext = "csv.gz"
    ),
    overwrite = FALSE) {
  suf <- "Extractions/ExtractRaw"
  url <- sprintf(fmt = get(x = "api", envir = cache_env), suf)
  body <- list(
    ExtractionRequest = list(
      `@odata.type` = jsonlite::unbox(
        paste(
          "#DataScope",
          "Select",
          "Api",
          "Extractions",
          "ExtractionRequests",
          "TickHistoryMarketDepthExtractionRequest",
          sep = "."
        )
      ),
      IdentifierList = list(
        `@odata.type` = jsonlite::unbox(
          paste(
            "#DataScope",
            "Select",
            "Api",
            "Extractions",
            "ExtractionRequests",
            "InstrumentIdentifierList",
            sep = "."
          )
        ),
        InstrumentIdentifiers = list(
          list(
            Identifier = jsonlite::unbox(identifier),
            IdentifierType = jsonlite::unbox(identifier_type)
          )
        ),
        ValidationOptions = jsonlite::unbox(NA),
        UseUserPreferencesForValidationOptions = jsonlite::unbox(FALSE)
      ),
      Condition = list(
        View = jsonlite::unbox(md_view),
        MessageTimeStampIn = jsonlite::unbox(timestamp_in),
        ReportDateRangeType = jsonlite::unbox("Range"),
        QueryStartDate = jsonlite::unbox(
          req_posixct_format(query_start_datetime)
        ),
        QueryEndDate = jsonlite::unbox(
          req_posixct_format(query_end_datetime)
        ),
        DisplaySourceRIC = jsonlite::unbox(display_source_ric)
      )
    )
  )
  # in case of "NormalizedLL2" view let us set the NumberOfLevels value
  if (md_view == "NormalizedLL2") {
    body$ExtractionRequest$Condition$NumberOfLevels <- jsonlite::unbox(number_of_levels)
    body$ExtractionRequest$ContentFieldNames <- lapply(
      get_valid_content_field_names(report_template_type = "TickHistoryMarketDepth")$Name,
      jsonlite::unbox
    )
  }
  res <- httr::POST(
    url,
    httr::add_headers(
      prefer = "respond-async",
      Authorization = get("token", envir = cache_env)
    ),
    httr::content_type_json(),
    body = jsonlite::toJSON(body),
    encode = "json",
    httr::progress()
  )
  if (httr::status_code(res) == 202) {
    message(
      "The request has been accepted but not yet completed, ",
      "executing asynchronously.\r\nmonitor URL: \r\n",
      res$headers$location
    )
    return(invisible(res$headers$location))
  } else {
    httr::warn_for_status(res)
    resc <- httr::content(
      res,
      as = "parsed",
      type = "application/json",
      encoding = "UTF-8"
    )
    if (httr::status_code(res) == 200) {
      message(resc$Notes)
      return(
        raw_extraction(
          job_id = resc$JobId,
          path = result_path,
          overwrite = overwrite
        )
      )
    } else {
      return(resc)
    }
  }
}


#' Poll the asynchronous extraction status.
#'
#' On Demand extraction requests are executed
#' asynchronously, which means as soon as possible.
#' This command checks the readiness of the job
#' and downloads the extractum if that is ready.
#'
#' @param monitor_url the monitor URL
#' @param result_path path of the prospective result file,
#' the extension must be ".csv.gz"
#' @param overwrite should overwrite existing result file TRUE/FALSE
#'
#' @return The "monitor_url" status message and
#' the result file downloaded, if applicable.
#'
#' @export
poll_async_status <- function(
    monitor_url,
    result_path = fs::path_home(
      "Downloads",
      sprintf(
        fmt = "RTH_extractum_%i",
        as.integer(Sys.time())
      ),
      ext = "csv.gz"
    ),
    overwrite = FALSE) {
  res <- httr::GET(
    monitor_url,
    httr::add_headers(
      prefer = "respond-async",
      Authorization = get("token", envir = cache_env)
    )
  )
  if (httr::status_code(res) == 202) {
    message(
      "The request has not completed yet, still executing asynchronously.\r\n",
      "Please wait a bit and poll the request status again.\r\n"
    )
    return(invisible(res$headers$location))
  } else {
    httr::warn_for_status(res)
    resc <- httr::content(
      res,
      as = "parsed",
      type = "application/json",
      encoding = "UTF-8"
    )
    if (httr::status_code(res) == 200) {
      message(resc$Notes)
      return(
        raw_extraction(
          job_id = resc$JobId,
          path = result_path,
          overwrite = overwrite
        )
      )
    } else {
      return(resc)
    }
  }
}
