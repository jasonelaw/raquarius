library(R6)
library(httr2)

ProvisioningClient <- R6Class(
  "ProvisioningClient",

  public = list(
    base_url = NULL,

    # Initialize the client with basic configuration
    initialize = function() {
      self$base_url <- aq_get_url()
      private$api_key <- get_auth_token()
    },

    # CREATE (POST)
    create = function(endpoint, body) {
      req <- private$base_request(endpoint) %>%
        req_body_json(body) %>%
        req_method("POST")

      req_perform(req) %>% resp_body_json()
    },

    # READ (GET)
    read = function(endpoint, query = NULL) {
      req <- private$base_request(endpoint)
      if (!is.null(query)) {
        req <- req_url_query(req, !!!query)
      }

      req_perform(req) %>% resp_body_json()
    },

    # UPDATE (PUT/PATCH)
    update = function(endpoint, body) {
      req <- private$base_request(endpoint) %>%
        req_body_json(body) %>%
        req_method("PUT")

      req_perform(req) %>% resp_body_json()
    },

    # DELETE (DELETE)
    delete = function(endpoint) {
      req <- private$base_request(endpoint) %>%
        req_method("DELETE")

      req_perform(req) # Returns response status
    }
  ),

  private = list(
    api_key = NULL, # Kept hidden from user console printing

    # Internal helper to construct headers and base request
    base_request = function(endpoint) {
      request(self$base_url) %>%
        req_url_path_append(endpoint) %>%
        req_headers("Authorization" = paste("Bearer", private$api_key)) %>%
        req_retry(max_tries = 3) # Built-in error handling
    }
  )
)


#' Get a monitoring location
prGetLocation <- function(LocationUniqueId) {
  aqts("provisioning") |>
    httr2::req_template(
      "GET /locations/{LocationUniqueId}"
    ) |>
    req_auth_token()
}

prUpdateLocation <- function(x) {
  req <- aqts("provisioning") |>
    httr2::req_template(
      "PUT /locations/{LocationUniqueId}",
      LocationUniqueId = x$UniqueId
    )
  x <- x[-1]
  req |>
    req_auth_token() |>
    httr2::req_body_json(x)
}

prDeleteLocation <- function(x) {
  req <- aqts("provisioning") |>
    httr2::req_template("DELETE /locations/{unique_id}", x$LocationUniqueId)
}
#' Delete a location
#'
#' Deletes a monitoring location. Only an empty location can be deleted.
#' @export
prDeleteLocation <- function(unique_id, .perform = TRUE) {
  ret <- aquarius(api = "provisioning", class = "locations") |>
    req_template("DELETE /locations/{unique_id}")
  if (.perform) {
    ret <- req_perform_aqts(ret)
  }
  ret
}

# Request factory -------------------------------------------------------------
pr_request_factory <- function(template, formals) {
  formals <- c(formals, rlang::pairlist2(.perform = TRUE))
  f <- function() {
    ret <- aqts(route = "", api = "provisioning") |>
      httr2::req_template(template)
    handle_request(ret, .perform = .perform, .format = TRUE)
  }
  formals(f) <- formals
  f
}

getLocation <- pr_request_factory(
  template = "locations/{LocationUniqueId}",
  formals = alist(LocationUniqueId = )
)

#' Get items nested under locations
#'
#' @param LocationUniqueId the guid for a location
#'
getLocationTimeseries <- pr_request_factory(
  template = "locations/{LocationUniqueId}/timeseries",
  formals = alist(LocationUniqueId = )
)

#' Get items nested under locations
#'
#' @param LocationUniqueId the guid for a location
getLocationDatum <- pr_request_factory(
  template = "locations/{LocationUniqueId}/standardreferencedatums",
  formals = alist(LocationUniqueId = , query = "/Results")
)
