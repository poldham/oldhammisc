cr_abstract2 <- function (doi, ...)
{
  url <- paste0("http://api.crossref.org/works/", doi, ".xml")
  cli <- crul::HttpClient$new(url = url, opts = list(followlocation = 1),
    headers = list(`User-Agent` = rcrossref_ua(), `X-USER-AGENT` = rcrossref_ua()))
  res <- cli$get(...)
  res$raise_for_status()
  txt <- res$parse("UTF-8")
  xml <- tryCatch(read_xml(txt), error = function(e) e)
  if (inherits(xml, "error")) {
    message(doi, " not found ", call. = FALSE)
  }
  # tt <- tryCatch(xml2::xml_find_first(xml, "//jats:abstract"), warning = function(w) w)
  # if (inherits(tt, "warning")) {
  #   message("no abstract found for ", doi, call. = FALSE)
  # }
  xml2::xml_text(xml)
}
