aq_provisioning_req <- function(path, ...) {
  pars <- process_pars(...)
  aquarius() |>
    req_url_path_append("provisioning", "v1", path) |>
    req_url_query(!!!pars)
}

GetLocation <- function(LocationUniqueId){
  aq_provisioning_req(path = NULL) |>
    req_template("GET /locations/{LocationUniqueId}")
}
