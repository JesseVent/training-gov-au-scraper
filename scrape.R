#!/usr/bin/env Rscript
# Scrape packaging rules (rules text + units by group) for current qualifications on training.gov.au.
# Run from this folder: `Rscript scrape.R` (or `Rscript scrape.R --selftest`).
# In RStudio: setwd() to this folder, edit the config below, then source().

# ---- Config ----
MAX_QUALIFICATIONS  <- 20   # how many to scrape, in code order; Inf = all (~1,153)
REQUESTS_PER_SECOND <- 2    # rate limit across all requests
MAX_RETRIES         <- 4    # retries on 429/5xx/network errors (honours Retry-After)

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(xml2)
})

API <- "https://training.gov.au/api"
RAW_DIR <- "raw"
OUT_DIR <- "out"
FIXTURE <- "fixtures/CHC33015_bundle.json"
IN_TEXT <- "(rules text)"  # group for refs mentioned in the rules prose rather than listed in a table
BLOCKS <- c("p", "li", "ul", "ol", "div", "h1", "h2", "h3", "h4", "h5", "h6", "table", "tr", "td", "th", "br")
n_requests <- 0

api_get <- function(path, ...) {
  n_requests <<- n_requests + 1
  resp <- request(API) |>
    req_url_path_append(path) |>
    req_url_query(`api-version` = "1.0", ...) |>
    req_headers(Accept = "application/json") |>
    req_user_agent("training-gov-au-scraper/0.1") |>
    # one-token bucket = steady rate; `rate =` alone allows a 60s burst before throttling kicks in
    req_throttle(capacity = 1, fill_time_s = 1 / REQUESTS_PER_SECOND) |>
    req_retry(
      max_tries = MAX_RETRIES + 1,
      retry_on_failure = TRUE,
      is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503, 504)
    ) |>
    req_perform()
  message(sprintf("  %d %s", resp_status(resp), path))
  resp_body_json(resp, simplifyVector = FALSE)
}

list_qualifications <- function(limit) {
  quals <- list()
  offset <- 0L
  repeat {
    page <- api_get(
      "/search/training",
      searchText = "", offset = offset, pageSize = as.integer(min(200, limit)),
      includeTotalCount = "true", orderBy = "Code asc",
      filter = "(Status/Id eq 0) and (Type/Id eq 2)"
    )
    quals <- c(quals, lapply(page$data, \(d) list(code = d$code, title = d$title)))
    offset <- offset + length(page$data)
    if (!length(page$data) || offset >= page$totalCount || length(quals) >= limit) break
  }
  message(page$totalCount, " current qualifications on training.gov.au")
  quals[seq_len(min(length(quals), limit))]
}

fetch_one <- function(code, title) {
  releases <- api_get(paste0("/training/", code))$releases
  current <- Filter(\(r) identical(r$currency, "current"), releases)
  rel <- if (length(current)) current[[1]] else releases[[1]]
  bundles <- api_get(sprintf("/training/%s/releases/%s", code, rel$releaseNumber), include = "All")$contentBundles
  bundle_id <- if (length(bundles)) bundles[[1]]$id else NULL
  bundle <- if (is.null(bundle_id)) NULL else api_get(paste0("/content/bundle/", bundle_id))
  list(code = code, title = title, release = rel$releaseNumber, bundle_id = bundle_id, bundle = bundle)
}

save_record <- function(rec) {
  path <- file.path(RAW_DIR, paste0(rec$code, ".json"))
  tmp <- paste0(path, ".tmp")
  writeLines(toJSON(rec, auto_unbox = TRUE, null = "null", digits = NA), tmp, useBytes = TRUE)
  file.rename(tmp, path)
}

squish <- function(x) trimws(gsub("[\\s ]+", " ", x, perl = TRUE))

# Lines of text outside tables, plus every <ntr-tcref> tagged with its group: the last
# heading-shaped line outside a table (no refs, short, no closing punctuation). Headings are
# <strong> in some packages, a CSS class or plain <p> in others, so markup can't be trusted.
parse_rules <- function(bundle) {
  items <- Filter(\(i) identical(i$contentType, "PackagingRules"), bundle$items)
  if (!length(items)) return(list(rules_text = NULL, units = list()))
  st <- new.env()
  st$lines <- character()
  st$units <- list()
  st$group <- NA_character_
  st$table <- 0
  st$buf <- character()
  st$has_ref <- FALSE

  flush <- function() {
    text <- squish(paste(st$buf, collapse = ""))
    if (nzchar(text) && st$table == 0) {
      st$lines <- c(st$lines, text)
      # ponytail: shape heuristic; notes and rule bullets end in punctuation. Check the group list on new packages.
      if (!st$has_ref && nchar(text) <= 120 && !grepl("[.:;]$", text)) st$group <- text
    }
    st$buf <- character()
    st$has_ref <- FALSE
  }

  walk <- function(node) {
    for (child in xml_contents(node)) {
      type <- xml_type(child)
      if (type == "text") {
        st$buf <- c(st$buf, xml_text(child))
        next
      }
      if (type != "element") next
      tag <- xml_name(child)
      if (tag %in% BLOCKS) flush()
      if (tag == "table") st$table <- st$table + 1
      if (tag == "ntr-tcref") {
        st$has_ref <- TRUE
        st$units[[length(st$units) + 1]] <- list(
          group = if (st$table > 0) st$group else IN_TEXT,
          code = xml_attr(child, "data-nrt-code"),
          title = squish(xml_attr(child, "data-nrt-title")),
          nrt_type = xml_attr(child, "data-nrt-type")
        )
      }
      walk(child)
      if (tag %in% BLOCKS) flush()
      if (tag == "table") st$table <- st$table - 1
    }
  }

  walk(read_html(items[[1]]$content, options = c("RECOVER", "NOERROR")))
  flush()
  list(rules_text = paste(st$lines, collapse = "\n"), units = st$units)
}

pluck_chr <- function(units, field) vapply(units, \(u) u[[field]], character(1))

write_outputs <- function() {
  dir.create(OUT_DIR, showWarnings = FALSE)
  jsonl <- character()
  rows <- list()
  flagged <- character()
  for (f in sort(list.files(RAW_DIR, pattern = "\\.json$", full.names = TRUE))) {
    rec <- fromJSON(f, simplifyVector = FALSE)
    parsed <- parse_rules(rec$bundle)
    groups <- pluck_chr(parsed$units, "group")
    if (!any(is.na(groups) | groups != IN_TEXT)) flagged <- c(flagged, rec$code)
    jsonl <- c(jsonl, toJSON(
      list(code = rec$code, title = rec$title, release = rec$release, bundle_id = rec$bundle_id,
           rules_text = parsed$rules_text, units = parsed$units),
      auto_unbox = TRUE, null = "null", na = "null"
    ))
    if (length(parsed$units)) {
      rows[[length(rows) + 1]] <- data.frame(
        qual_code = rec$code, qual_title = rec$title, release = rec$release, group = groups,
        unit_code = pluck_chr(parsed$units, "code"), unit_title = pluck_chr(parsed$units, "title"),
        nrt_type = pluck_chr(parsed$units, "nrt_type")
      )
    }
  }
  writeLines(jsonl, file.path(OUT_DIR, "qualifications.jsonl"), useBytes = TRUE)
  write.csv(do.call(rbind, rows), file.path(OUT_DIR, "units.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
  message(sprintf("wrote %d qualifications to %s/", length(jsonl), OUT_DIR),
          if (length(flagged)) paste0("; no units in tables (check by hand): ", paste(flagged, collapse = ", ")))
}

selftest <- function() {
  p <- parse_rules(fromJSON(FIXTURE, simplifyVector = FALSE))
  codes_in <- \(g) pluck_chr(Filter(\(u) identical(u$group, g), p$units), "code")
  stopifnot(
    startsWith(p$rules_text, "Total number of units = 13"),
    identical(codes_in("Core units"),
              c("CHCCCS015", "CHCCCS023", "CHCCOM005", "CHCDIV001", "CHCLEG001", "HLTAAP001", "HLTWHS002")),
    setequal(pluck_chr(Filter(\(u) u$code == "HLTAID011", p$units), "group"),
             c("Group D AGED CARE", "Other electives")),
    identical(codes_in("Group C electives – HOME AND COMMUNITY specialisation (4 units)"),
              c("CHCAGE001", "CHCDIS007", "CHCCCS011", "CHCCCS025", "CHCHCS001")),
    all(pluck_chr(p$units, "nrt_type") == "unit")
  )
  message(sprintf("selftest ok: %d unit refs, %d groups", length(p$units), length(unique(pluck_chr(p$units, "group")))))
}

main <- function() {
  if ("--selftest" %in% commandArgs(trailingOnly = TRUE)) return(invisible(selftest()))
  dir.create(RAW_DIR, showWarnings = FALSE)
  t0 <- Sys.time()
  quals <- list_qualifications(MAX_QUALIFICATIONS)
  todo <- Filter(\(q) !file.exists(file.path(RAW_DIR, paste0(q$code, ".json"))), quals)
  message(sprintf("%d selected, %d cached, %d to fetch", length(quals), length(quals) - length(todo), length(todo)))
  failed <- character()
  for (i in seq_along(todo)) {
    q <- todo[[i]]
    message(sprintf("[%d/%d] %s %s", i, length(todo), q$code, q$title))
    # one bad qualification shouldn't stop the run; re-running retries it
    tryCatch(save_record(fetch_one(q$code, q$title)), error = \(e) {
      failed <<- c(failed, q$code)
      message("  FAILED ", q$code, ": ", conditionMessage(e))
    })
  }
  write_outputs()
  message(sprintf("requests %d | elapsed %.0fs | failed %d%s", n_requests,
                  as.numeric(Sys.time() - t0, units = "secs"), length(failed),
                  if (length(failed)) paste0(": ", paste(failed, collapse = ", ")) else ""))
}

main()
