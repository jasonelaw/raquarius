prGetLocation <- function(unique_id){
  aquarius(api = "provisioning", class = "locations") |>
    req_template("GET /locations/{unique_id}")
}
