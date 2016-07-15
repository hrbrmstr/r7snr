b64_decode <- purrr::safely(base64)

check_encoding <- function(x) {
  if ((tolower(x) %in% tolower(iconvlist())))
    return(x)

  message("Invalid encoding ", x, ": defaulting to UTF-8.")
  "UTF-8"
}

guess_encoding <- function(encoding = NULL, type = NULL) {
  if (!is.null(encoding))
    return(check_encoding(encoding))

  charset <- if (!is.null(type)) parse_media(type)$params$charset

  if (is.null(charset)) {
    message("No encoding supplied: defaulting to UTF-8.")
    return("UTF-8")
  }

  check_encoding(charset)
}

parse_text <- function(content, type = NULL, encoding = NULL) {
  encoding <- guess_encoding(encoding, type)
  iconv(readBin(content, character()), from = encoding, to = "UTF-8")
}

parse_auto <- function(content, type = NULL, encoding = NULL, ...) {
  if (length(content) == 0) {
    return(NULL)
  }

  if (is.null(type)) {
    stop("Unknown mime type: can't parse automatically. Use the `type` ",
      "argument to specify manually.", call. = FALSE)
  }

  mt <- parse_media(type)
  parser <- parsers[[mt$complete]]
  if (is.null(parser)) {
    stop("No automatic parser available for ", mt$complete, ".",
      call. = FALSE)
  }

  parser(x = content, type = type, encoding = encoding, ...)
}

parseability <- function(type) {
  if (is.null(type) || type == "") return("raw")
  mt <- parse_media(type)

  if (exists(mt$complete, parsers)) {
    "parsed"
  } else if (mt$type == "text") {
    "text"
  } else {
    "raw"
  }
}


parsers <- new.env(parent = emptyenv())

# Binary formats ---------------------------------------------------------------

# http://www.ietf.org/rfc/rfc4627.txt - section 3. (encoding)
parsers$`application/json` <- function(x, type = NULL, encoding = NULL,
                                       simplifyVector = FALSE, ...) {
  jsonlite::fromJSON(parse_text(x, encoding = "UTF-8"),
    simplifyVector = simplifyVector, ...)
}
parsers$`application/x-www-form-urlencoded` <- function(x, encoding = NULL,
                                                        type = NULL, ...) {
  parse_query(parse_text(x, encoding = "UTF-8"))
}

# Text formats -----------------------------------------------------------------
parsers$`image/jpeg` <- function(x, type = NULL, encoding = NULL, ...) {
  need_package("jpeg")
  jpeg::readJPEG(x)
}

parsers$`image/png` <- function(x, type = NULL, encoding = NULL, ...) {
  need_package("png")
  png::readPNG(x)
}

parsers$`text/plain` <- function(x, type = NULL, encoding = NULL, ...) {
  encoding <- guess_encoding(encoding, type)
  parse_text(x, type = type, encoding = encoding)
}

parsers$`text/html` <- function(x, type = NULL, encoding = NULL, ...) {
  need_package("xml2")

  encoding <- guess_encoding(encoding, type)
  xml2::read_html(x, encoding = encoding, ...)
}

parsers$`application/xml` <- function(x, type = NULL, encoding = NULL, ...) {
  need_package("xml2")

  encoding <- guess_encoding(encoding, type)
  xml2::read_xml(x, encoding = encoding, ...)
}

parsers$`text/xml` <- function(x, type = NULL, encoding = NULL, ...) {
  need_package("xml2")

  encoding <- guess_encoding(encoding, type)
  xml2::read_xml(x, encoding = encoding, ...)
}

parsers$`text/csv` <- function(x, type = NULL, encoding = NULL, ...) {
  need_package("readr")

  encoding <- guess_encoding(encoding, type)
  readr::read_csv(x, locale = readr::locale(encoding = encoding), ...)
}

parsers$`text/tab-separated-values` <- function(x, type = NULL, encoding = NULL, ...) {
  need_package("readr")

  encoding <- guess_encoding(encoding, type)
  readr::read_tsv(x, locale = readr::locale(encoding = encoding), ...)
}

# Parses a header lines as recieved from libcurl. Multiple responses
# will be intermingled, each separated by an http status line.
parse_headers <- function(x) {
  lines <- strsplit(x, "\r?\n")[[1]]

  new_response <- grepl("^HTTP", lines)
  grps <- cumsum(new_response)

  lapply(unname(split(lines, grps)), parse_single_header)
}

parse_single_header <- function(lines) {
  status <- parse_http_status(lines[[1]])

  # Parse headers into name-value pairs
  header_lines <- lines[lines != ""][-1]
  pos <- regexec("^([^:]*):\\s*(.*)$", header_lines)
  pieces <- regmatches(header_lines, pos)

  n <- vapply(pieces, length, integer(1))
  if (any(n != 3)) {
    bad <- header_lines[n != 3]
    pieces <- pieces[n == 3]

    warning("Failed to parse headers:\n", paste0(bad, "\n"), call. = FALSE)
  }

  names <- vapply(pieces, "[[", 2, FUN.VALUE = character(1))
  values <- lapply(pieces, "[[", 3)
  headers <- insensitive(stats::setNames(values, names))

  list(status = status$status, version = status$version, headers = headers)
}

parse_http_status <- function(x) {
  status <- as.list(strsplit(x, "\\s+")[[1]])
  names(status) <- c("version", "status", "message")[seq_along(status)]
  status$status <- as.integer(status$status)


  status
}

parsers <- new.env(parent = emptyenv())

# Binary formats ---------------------------------------------------------------

# http://www.ietf.org/rfc/rfc4627.txt - section 3. (encoding)
parsers$`application/json` <- function(x, type = NULL, encoding = NULL,
                                       simplifyVector = FALSE, ...) {
  jsonlite::fromJSON(parse_text(x, encoding = "UTF-8"),
    simplifyVector = simplifyVector, ...)
}
parsers$`application/x-www-form-urlencoded` <- function(x, encoding = NULL,
                                                        type = NULL, ...) {
  parse_query(parse_text(x, encoding = "UTF-8"))
}

# Text formats -----------------------------------------------------------------
parsers$`image/jpeg` <- function(x, type = NULL, encoding = NULL, ...) {
  need_package("jpeg")
  jpeg::readJPEG(x)
}

parsers$`image/png` <- function(x, type = NULL, encoding = NULL, ...) {
  need_package("png")
  png::readPNG(x)
}

parsers$`text/plain` <- function(x, type = NULL, encoding = NULL, ...) {
  encoding <- guess_encoding(encoding, type)
  parse_text(x, type = type, encoding = encoding)
}

parsers$`text/html` <- function(x, type = NULL, encoding = NULL, ...) {
  need_package("xml2")

  encoding <- guess_encoding(encoding, type)
  xml2::read_html(x, encoding = encoding, ...)
}

parsers$`application/xml` <- function(x, type = NULL, encoding = NULL, ...) {
  need_package("xml2")

  encoding <- guess_encoding(encoding, type)
  xml2::read_xml(x, encoding = encoding, ...)
}

parsers$`text/xml` <- function(x, type = NULL, encoding = NULL, ...) {
  need_package("xml2")

  encoding <- guess_encoding(encoding, type)
  xml2::read_xml(x, encoding = encoding, ...)
}

parsers$`text/csv` <- function(x, type = NULL, encoding = NULL, ...) {
  need_package("readr")

  encoding <- guess_encoding(encoding, type)
  readr::read_csv(x, locale = readr::locale(encoding = encoding), ...)
}

parsers$`text/tab-separated-values` <- function(x, type = NULL, encoding = NULL, ...) {
  need_package("readr")

  encoding <- guess_encoding(encoding, type)
  readr::read_tsv(x, locale = readr::locale(encoding = encoding), ...)
}


#' Create a vector with case insensitive name matching.
#'
#' @param x vector to modify
#' @export
#' @keywords internal
#' @examples
#' x <- c("abc" = 1, "def" = 2)
#' x["ABC"]
#' y <- insensitive(x)
#' y["ABC"]
#' y[["ABC"]]
insensitive <- function(x) {
  names(x) <- tolower(names(x))
  structure(x, class = c("insensitive", class(x)))
}

#' @export
`[.insensitive` <- function(x, i, ...) {
  if (is.character(i)) {
    i <- tolower(i)
  }

  NextMethod()
}

#' @export
`[[.insensitive` <- `[.insensitive`


#' @export
"$.insensitive" <- function(x, name) {
  name <- tolower(name)
  x[[name]]
}

#' Decode & parse Sonar HTTP scan result
#'
#' @export
snr_parse_response <- function(x) {

  map(x, function(y) {
    x <- b64_decode(x, encode=FALSE)
    if (is.null(x$result)) {
      list(status=NA, version=NA, headers=NA, body=NA)
    } else {
      resp <- stri_split_fixed(x$result, "\r\n\r\n", 2)[[1]]
      headers <- parse_headers(resp[1])[[1]]
      body <- parse_text(charToRaw(resp[2]), encoding="UTF-8")
      headers[["body"]] <- body
      headers
    }
  })

}

