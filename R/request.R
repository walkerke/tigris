# Requests to the Census Bureau's servers, built on httr2: retries for
# temporary failures, and clear errors for missing files and failed
# connections. There is deliberately no overall time limit, as large files can
# take a long time to download on slow connections.

# HTTP statuses worth retrying: rate limiting and temporary server problems
tigris_retry_statuses <- c(429, 500, 502, 503, 504)
tigris_max_tries <- 3

tigris_user_agent <- function() {
  sprintf(
    "tigris/%s (https://github.com/walkerke/tigris)",
    utils::packageVersion("tigris")
  )
}

# Build a request. `validate`, if given, checks a downloaded file; a successful
# response whose file fails it (e.g. an HTML error page instead of a zip) is
# retried within the same attempt budget as server errors.
tigris_request <- function(url, progress = FALSE, validate = NULL) {
  req <- httr2::request(url) |>
    httr2::req_user_agent(tigris_user_agent()) |>
    httr2::req_retry(
      max_tries = tigris_max_tries,
      is_transient = function(resp) {
        if (httr2::resp_status(resp) %in% tigris_retry_statuses) {
          return(TRUE)
        }
        !is.null(validate) && is_success(resp) &&
          inherits(resp$body, "httr2_path") && !validate(resp$body)
      },
      retry_on_failure = TRUE
    ) |>
    # Callers check the status themselves to give tigris error messages
    httr2::req_error(is_error = function(resp) FALSE)

  if (progress) {
    req <- httr2::req_progress(req)
  }

  req
}

# Successful response: HTTP 200, or any completed FTP transfer (FTP reports its
# own codes, e.g. 226, and failures arrive as curl errors instead)
is_success <- function(resp) {
  !is_http(httr2::resp_url(resp)) || httr2::resp_status(resp) == 200
}

is_http <- function(url) {
  grepl("^https?://", url)
}

# Perform a request. A missing FTP file and a connection that fails on every
# attempt get clear errors; the original error is kept on the condition as
# `original_error` rather than printed.
tigris_perform <- function(req, path = NULL) {
  tryCatch(
    httr2::req_perform(req, path = path),
    httr2_failure = function(e) {
      if (inherits(e$parent, "curl_error_remote_file_not_found")) {
        stop_file_not_found(req$url, original_error = e)
      }

      host <- httr2::url_parse(req$url)$hostname
      cause <- root_cause_message(e)
      cli_abort(
        c(
          "Unable to connect to {host} after {tigris_max_tries} attempts.",
          "i" = "{cause}"
        ),
        class = "tigris_connection_error",
        original_error = e,
        call = NULL
      )
    }
  )
}

stop_file_not_found <- function(url, original_error = NULL) {
  cli_abort(
    "File not found on the Census Bureau's server: {.url {url}}",
    class = "tigris_file_not_found",
    original_error = original_error,
    call = NULL
  )
}

# Download `url` to `path`. The file goes to a temporary file first and is
# moved to `path` only once it's complete and passes `validate`, so a failed
# download never leaves a partial file behind.
tigris_download <- function(url, path, validate = NULL, progress = FALSE) {
  tmp <- tempfile("download-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)

  # Remember the result for the last response, so the final file isn't
  # checked twice (checks can be expensive, e.g. reading a whole shapefile)
  last_check <- NA
  check <- if (!is.null(validate)) {
    function(file) {
      last_check <<- isTRUE(validate(file))
      last_check
    }
  }

  resp <- tigris_perform(tigris_request(url, progress, check), path = tmp)

  if (is_http(url)) {
    status <- httr2::resp_status(resp)

    if (status == 404) {
      stop_file_not_found(url)
    }

    if (status != 200) {
      cli_abort(
        "Unable to download {.url {url}} ({status} {httr2::resp_status_desc(resp)}).",
        class = "tigris_download_error",
        call = NULL
      )
    }
  }

  if (!is.null(check) && !(if (is.na(last_check)) check(tmp) else last_check)) {
    cli_abort(
      "The file downloaded from {.url {url}} is incomplete or invalid.",
      class = "tigris_download_error",
      call = NULL
    )
  }

  if (!file.rename(tmp, path)) {
    cli_abort("Unable to save the download to {.path {path}}.", call = NULL)
  }

  invisible(path)
}

# The innermost error message in a chain of errors (e.g. curl's, under httr2's)
root_cause_message <- function(e) {
  while (!is.null(e$parent)) {
    e <- e$parent
  }
  trimws(gsub("\\s+", " ", conditionMessage(e)))
}
