#' #' Retrieve the list of all user packages, i.e. packages @@@@~~~~
#' #' to which the user is entitled (for all subscriptions)
#' #'
#' #' @return Return the list of user package Id, user package name
#' #' and the corresponding subscription name
#' #'
#' #' @export
#' get_user_packages <- function() {
#'     suf <- "StandardExtractions/UserPackages"
#'     url <- sprintf(fmt = cache_env$api, suf)
#'     resc <- get_all_pages(url)
#'     purrr::map_dfr(
#'         purrr::pluck(resc, "value"),
#'         ~purrr::compact(.x) |>
#'             unlist() |>
#'             t() |>
#'             tibble::as_tibble()
#'     )
#' }


#' #' List all user package deliveries (data files) for one package. @@@@~~~~
#' #'
#' #' @param PackageId User package Id. Usually from get_user_packages()
#' #'
#' #' @return Return the list of user package delivery Id
#' #'
#' #' @export
#' list_data_by_package_id <- function(package_id) {
#'     suf <- sprintf(
#'         fmt = "StandardExtractions/%s(PackageId='%s')",
#'         "UserPackageDeliveryGetUserPackageDeliveriesByPackageId",
#'         package_id
#'     )
#'     url <- sprintf(fmt = get(x = "api", envir = cache_env), suf)
#'     resc <- get_all_pages(url)
#'     purrr::map_dfr(
#'         purrr::pluck(resc, "value"),
#'         ~purrr::compact(.x) |>
#'             unlist() |>
#'             t() |>
#'             tibble::as_tibble()
#'     )
#' }


#' #' List all user package deliveries (data files) for one package. @@@@~~~~
#' #'
#' #' @param subscription_id id of the subscription
#' #' @param from_datetime fromDate
#' #' @param to_datetime toDate
#' #'
#' #' @return Returns the list of deliveries by date range
#' #'
#' #' @export
#' list_data_by_date_range <- function(
#'         subscription_id,
#'         from_datetime = Sys.time() - 72*24*60*60,
#'         to_datetime = Sys.time()) {
#'     suf <- sprintf(
#'         fmt = "StandardExtractions/%s(SubscriptionId='%s',FromDate=%s,ToDate=%s)",
#'         "UserPackageDeliveryGetUserPackageDeliveriesByDateRange",
#'         subscription_id,
#'         req_posixct_format(from_datetime),
#'         req_posixct_format(to_datetime)
#'     )
#'     url <- sprintf(fmt = get(x = "api", envir = cache_env), suf)
#'     resc <- get_all_pages(url)
#'     purrr::map_dfr(
#'         purrr::pluck(resc, "value"),
#'         ~purrr::compact(.x) |>
#'             unlist() |>
#'             t() |>
#'             tibble::as_tibble()
#'     )
#' }


#' #' Get the user package delivery (data file) and save it to disk @@@@~~~~
#' #'
#' #' @param packageDeliveryId user package delivery id
#' #' @param path the path and file name to content to
#' #' @param overwrite will overwrite or not
#' #'
#' #' @export
#' get_package_delivery_data <- function(
#'         package_delivery_id = "0x08603982a9e894fd",
#'         path,
#'         overwrite = FALSE,
#'         aws = FALSE) {
#'     suf <- sprintf(
#'         fmt = "StandardExtractions/%s('%s')/$value",
#'         "UserPackageDeliveries",
#'         package_delivery_id
#'     )
#'     url <- sprintf(fmt = get(x = "api", envir = cache_env), suf)
#'     res <- httr::GET(
#'         url,
#'         httr::add_headers(
#'             prefer = "respond-async",
#'             Authorization = get("token", envir = cache_env)
#'         ),
#'         if (aws) httr::add_headers("X-Direct-Download" = "true"),
#'         httr::config(
#'             http_content_decoding = 0,
#'             followlocation = 0
#'         ),
#'         httr::write_disk(
#'             path = path,
#'             overwrite = overwrite
#'         ),
#'         httr::progress()
#'     )
#'     # if the resource requested has been temporarily
#'     # moved to the URL given by the "location" header.
#'     if (httr::status_code(res) == 302) {
#'         url2 <- res$headers$location
#'         res2 <- httr::GET(
#'             url2,
#'             httr::add_headers(prefer = "respond-async"),
#'             httr::config(
#'                 http_content_decoding = 0,
#'                 followlocation = 0
#'             ),
#'             httr::write_disk(
#'                 path = path,
#'                 overwrite = overwrite
#'             ),
#'             httr::progress()
#'         )
#'         httr::stop_for_status(res2)
#'         return(res2)
#'     } else {
#'         httr::stop_for_status(res)
#'         return(res)
#'     }
#' }
