cache_env <- new.env()
assign(
  x = "api",
  value = "https://selectapi.datascope.refinitiv.com/RestApi/v1/%s",
  envir = cache_env
)


get_all_pages <- function(url) {
  res <- httr::GET(
    url,
    httr::add_headers(
      prefer = "respond-async",
      Authorization = get("token", envir = cache_env)
    ),
    httr::progress()
  )
  httr::warn_for_status(res)
  resc <- httr::content(
    res,
    as = "parsed",
    type = "application/json",
    encoding = "UTF-8"
  )
  if (!is.null(resc$error)) warning(resc$error$message)
  # If there is a next link, call the function recursively again,
  # using next link and merge the result
  if (!is.null(resc$`@odata.nextlink`)) {
    resc$value <- append(
      x = resc$value,
      values = get_all_pages(url = resc$`@odata.nextlink`)
    )
  }
  # Remove next link to avoid confusion
  resc$`@odata.nextlink` <- NULL
  return(resc)
}


req_posixct_format <- function(x) {
  if (!any(class(x) == "POSIXct")) {
    x <- lubridate::parse_date_time(
      x,
      orders = c(
        "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d",
        "%Y.%m.%d %H:%M:%S", "%Y.%m.%d %H:%M", "%Y.%m.%d",
        "%Y%m%d%H%M%S", "%Y%m%d%H%M", "%Y%m%d"
      ),
      tz = "UTC",
      quiet = TRUE
    )
    if (is.na(x)) {
      stop(
        "Only the class POSIXct or '%Y-%m-%d %H:%M:%S' ",
        "formatted text are supported by the converter."
      )
    } else {
      warning("The ", x, " value interpreted as UTC.", call. = FALSE)
    }
  }
  strftime(x, format = "%Y-%m-%dT%H:%M:%S.000Z", tz = "UTC", usetz = FALSE)
}


raw_extraction <- function(job_id, path, overwrite = TRUE) {
  suf <- sprintf(
    fmt = "Extractions/RawExtractionResults('%s')/$value",
    job_id
  )
  url <- sprintf(fmt = cache_env$api, suf)
  res <- httr::GET(
    url,
    httr::add_headers(
      prefer = "respond-async",
      Authorization = get("token", envir = cache_env)
    ),
    httr::config(http_content_decoding = 0, followlocation = 0),
    httr::write_disk(path = path, overwrite = overwrite),
    httr::progress()
  )
  httr::warn_for_status(res)
  return(invisible(res))
}
