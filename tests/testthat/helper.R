skip_if_webportal_offline <- function() {
  skip_if_offline(httr2::url_parse(wp_get_url())$hostname)
}

skip_if_aqts_offline <- function() {
  skip_if_offline(httr2::url_parse(aq_get_url())$hostname)
}
