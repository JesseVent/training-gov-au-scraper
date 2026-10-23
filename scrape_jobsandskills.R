#!/usr/bin/env Rscript
# Download every file published under jobsandskills.gov.au/data in its original format, with a manifest.
# Run from this folder: `Rscript scrape_jobsandskills.R` (or `Rscript scrape_jobsandskills.R --selftest`).
# In RStudio: setwd() to this folder, edit the config below, then source().

# ---- Config ----
MAX_PAGES           <- Inf  # pages to crawl under /data, breadth-first from the landing page; Inf = all
REQUESTS_PER_SECOND <- 1    # rate limit across all requests
MAX_RETRIES         <- 4    # retries on 429/5xx/network errors (exponential backoff, honours Retry-After)

suppressPackageStartupMessages({
  library(httr2)
  library(xml2)
  library(readxl)
})

SITE <- "https://www.jobsandskills.gov.au"
HOST <- "www.jobsandskills.gov.au"
START <- "/data"
UA <- "jobsandskills-scraper/0.1 (+https://github.com/JesseVent/training-gov-au-scraper)"
OUT_DIR <- "data/jobsandskills"
FILES_DIR <- file.path(OUT_DIR, "files")
MANIFEST <- file.path(OUT_DIR, "manifest.csv")
MANIFEST_COLS <- c("dataset_title", "file_label", "page_url", "file_url", "uploaded", "last_modified", "etag",
                   "stated_bytes", "bytes", "md5", "local_path", "downloaded_at", "check")
FIXTURE <- "fixtures/jsa_occupation_shortage.html"
ROBOTS_FIXTURE <- "fixtures/jsa_robots.txt"
FILE_EXT <- "\\.(csv|xlsx?|xlsm|zip|pdf|docx?|pptx?|json|txt)$"
n_requests <- 0
problems <- data.frame(kind = character(), url = character(), reason = character())

note <- function(kind, url, reason) {
  reason <- trimws(gsub("\\s+", " ", reason))  # curl/httr2 errors span several lines
  problems <<- rbind(problems, data.frame(kind = kind, url = url, reason = reason))
  message("  ", toupper(kind), " ", url, ": ", reason)
}

fetch <- function(url, headers = list(), path = NULL, ok = \(r) resp_status(r) < 400) {
  n_requests <<- n_requests + 1
  resp <- request(url) |>
    req_user_agent(UA) |>
    req_headers(!!!headers) |>
    # one-token bucket = steady rate; `rate =` alone allows a 60s burst before throttling kicks in
    req_throttle(capacity = 1, fill_time_s = 1 / REQUESTS_PER_SECOND) |>
    req_retry(
      max_tries = MAX_RETRIES + 1,
      retry_on_failure = TRUE,
      is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503, 504)
    ) |>
    req_error(is_error = \(r) !ok(r)) |>
    req_perform(path = path)
  message(sprintf("  %d %s", resp_status(resp), url))
  resp
}

squish <- function(x) trimws(gsub("[\\s ]+", " ", x, perl = TRUE))

# ---- robots.txt (RFC 9309) ----
# Rules from the group naming our product token, else `*`. Longest matching pattern wins, Allow on a tie.
parse_robots <- function(txt) {
  rules <- data.frame(agent = character(), allow = logical(), pattern = character())
  agents <- character()
  seen <- character()
  in_rules <- FALSE
  for (line in trimws(sub("#.*", "", strsplit(txt, "\r?\n")[[1]]))) {
    m <- regmatches(line, regexec("^([A-Za-z-]+)\\s*:\\s*(.*)$", line))[[1]]
    if (!length(m)) next
    key <- tolower(m[2])
    if (key == "user-agent") {
      if (in_rules) agents <- character()
      in_rules <- FALSE
      agents <- c(agents, tolower(m[3]))
      seen <- c(seen, tolower(m[3]))
    } else if (key %in% c("allow", "disallow")) {
      in_rules <- TRUE
      # an empty Disallow allows everything, so it adds no rule
      if (nzchar(m[3]) && length(agents)) {
        rules <- rbind(rules, data.frame(agent = agents, allow = key == "allow", pattern = m[3]))
      }
    }
  }
  token <- tolower(sub("/.*", "", UA))
  rules[rules$agent == (if (token %in% seen) token else "*"), c("allow", "pattern")]
}

# `*` matches anything, a trailing `$` anchors the end; otherwise a pattern is a path prefix
glob_rx <- function(p) {
  end <- endsWith(p, "$")
  if (end) p <- substr(p, 1, nchar(p) - 1)
  parts <- gsub("([.\\\\|()\\[\\]{}^$+?])", "\\\\\\1", strsplit(p, "*", fixed = TRUE)[[1]], perl = TRUE)
  paste0("^", paste(parts, collapse = ".*"), if (end) "$")
}

robots_allows <- function(rules, path) {
  hit <- vapply(rules$pattern, \(p) grepl(glob_rx(p), path, perl = TRUE), logical(1))
  if (!any(hit)) return(TRUE)
  r <- rules[hit, ]
  r$allow[order(-nchar(r$pattern), !r$allow)][1]
}

# A 4xx means there is no robots.txt, so everything is allowed. 5xx and network errors throw here, and
# the caller treats that as complete disallow, as RFC 9309 requires.
load_robots <- function() {
  resp <- fetch(paste0(SITE, "/robots.txt"), ok = \(r) resp_status(r) < 500)
  parse_robots(if (resp_status(resp) < 400) resp_body_string(resp) else "")
}

# ---- Pages ----
# Dataset title (h1), /data links to crawl, and file links, read from <main> only (the menus repeat the
# whole site). File links in a download block take its `fileName` label and stated `fileSize` in bytes.
parse_page <- function(html, page_url) {
  doc <- read_html(html)
  main <- xml_find_first(doc, "//main")
  if (inherits(main, "xml_missing")) main <- doc
  links <- character()
  outside <- character()
  files <- list()
  for (a in xml_find_all(main, ".//a[@href]")) {
    url <- sub("#.*", "", url_absolute(xml_attr(a, "href"), page_url))
    u <- tryCatch(httr2::url_parse(url), error = \(e) NULL)
    if (is.null(u) || !u$scheme %in% c("http", "https")) next
    if (grepl(FILE_EXT, u$path, ignore.case = TRUE)) {
      item <- xml_find_first(a, "ancestor::div[p[contains(@class, 'fileName')]][1]")
      boxed <- !inherits(item, "xml_missing")
      size <- if (boxed) squish(xml_text(xml_find_first(item, ".//span[contains(@class, 'fileSize')]"))) else NA
      files[[length(files) + 1]] <- data.frame(
        file_label = squish(xml_text(if (boxed) xml_find_first(item, ".//p[contains(@class, 'fileName')]") else a)),
        file_url = url,
        stated_bytes = if (isTRUE(grepl("^\\d+$", size))) size else NA_character_
      )
    } else if (identical(u$hostname, HOST) && (u$path == START || startsWith(u$path, paste0(START, "/")))) {
      # pager pages keep ?page=; other queries (sorts, filters) would only revisit the same listings
      links <- c(links, paste0(u$path, if (!is.null(u$query$page)) paste0("?page=", u$query$page)))
    } else if (identical(u$hostname, HOST) && u$path != "/") {
      outside <- c(outside, u$path)
    }
  }
  files <- do.call(rbind, files)
  list(
    title = squish(xml_text(xml_find_first(main, ".//h1"))),
    links = unique(links),
    outside = unique(outside),
    files = if (is.null(files)) NULL else files[!duplicated(files$file_url), ]
  )
}

# Breadth-first over /data. Returns one row per distinct file link, tagged with the first page linking it.
crawl <- function(rules) {
  queue <- START
  seen <- character()
  found <- list()
  while (length(queue) && length(seen) < MAX_PAGES) {
    path <- queue[1]
    queue <- queue[-1]
    seen <- c(seen, path)
    url <- paste0(SITE, path)
    if (!robots_allows(rules, path)) {
      note("skipped", url, "disallowed by robots.txt")
      next
    }
    message(sprintf("[page %d, %d queued] %s", length(seen), length(queue), path))
    page <- tryCatch({
      resp <- fetch(url)
      final <- httr2::url_parse(resp_url(resp))$path
      if (!identical(final, START) && !startsWith(final, paste0(START, "/"))) {
        note("skipped", url, paste("redirects outside /data, to", resp_url(resp)))
        NULL
      } else if (!identical(resp_content_type(resp), "text/html")) {
        note("skipped", url, paste("not an HTML page:", resp_content_type(resp)))
        NULL
      } else {
        parse_page(resp_body_string(resp), url)
      }
    }, error = \(e) {
      note("failed", url, conditionMessage(e))
      NULL
    })
    if (is.null(page)) next
    queue <- c(queue, setdiff(page$links, c(seen, queue)))
    # the landing page lists some datasets that live outside /data; they're out of scope but worth naming
    if (path == START) for (p in page$outside) note("skipped", paste0(SITE, p), "outside /data (linked from the /data landing page)")
    if (!is.null(page$files)) found[[length(found) + 1]] <- cbind(dataset_title = page$title, page_url = url, page$files)
  }
  if (length(queue)) message(sprintf("MAX_PAGES reached; %d pages left unvisited", length(queue)))
  found <- do.call(rbind, found)
  list(pages = length(seen), files = if (is.null(found)) NULL else found[!duplicated(found$file_url), ])
}

# ---- Files ----
# Mirror the URL path under FILES_DIR minus Drupal's /sites/default/files/ prefix. That keeps the YYYY-MM
# upload folder, so same-named files from different months don't collide. Dot segments are dropped, since
# the path comes from the site.
local_path <- function(file_url) {
  path <- sub("^/sites/default/files/", "", httr2::url_parse(file_url)$path)
  segs <- strsplit(URLdecode(path), "/", fixed = TRUE)[[1]]
  segs <- segs[nzchar(segs) & !segs %in% c(".", "..")]
  do.call(file.path, as.list(c(FILES_DIR, segs)))
}

# "ok", or why the file doesn't open as its type. Types without a check here only need to be non-empty.
check_file <- function(path) {
  if (!file.exists(path) || file.size(path) == 0) return("empty file")
  tryCatch({
    switch(tolower(tools::file_ext(path)),
      csv = read.csv(path, nrows = 100, check.names = FALSE),
      xls = , xlsx = , xlsm = for (s in excel_sheets(path)) read_excel(path, s, n_max = 5),
      zip = , docx = , pptx = if (system2("unzip", c("-tqq", shQuote(path)), stdout = FALSE, stderr = FALSE) != 0) {
        stop("zip integrity test failed")
      },
      pdf = {
        b <- readBin(path, "raw", file.size(path))
        if (!length(grepRaw("%PDF-", head(b, 1024), fixed = TRUE))) stop("no %PDF- header")
        if (!length(grepRaw("%%EOF", tail(b, 2048), fixed = TRUE))) stop("no %%EOF trailer (truncated?)")
      }
    )
    "ok"
  }, error = \(e) conditionMessage(e))
}

# Fetch one file unless the copy on disk is the one the manifest describes and the server says it hasn't
# changed (304). A server that ignores If-None-Match/If-Modified-Since costs a download, but an identical
# body still counts as unchanged and the file on disk isn't touched.
download <- function(f, prev) {
  dest <- local_path(f$file_url)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  have <- !is.null(prev) && file.exists(dest) && identical(unname(tools::md5sum(dest)), prev$md5)
  headers <- if (have) Filter(nzchar, list(`If-None-Match` = prev$etag, `If-Modified-Since` = prev$last_modified)) else list()
  tmp <- paste0(dest, ".tmp")
  on.exit(unlink(tmp))
  resp <- fetch(f$file_url, headers, path = tmp)
  row <- list(
    dataset_title = f$dataset_title, file_label = f$file_label, page_url = f$page_url, file_url = f$file_url,
    uploaded = regmatches(f$file_url, regexpr("(?<=/files/)\\d{4}-\\d{2}(?=/)", f$file_url, perl = TRUE))[1],
    stated_bytes = f$stated_bytes, local_path = dest
  )
  if (resp_status(resp) == 304) {
    return(c(row, prev[c("last_modified", "etag", "bytes", "md5", "downloaded_at", "check")], status = "unchanged"))
  }
  same <- file.exists(dest) && identical(unname(tools::md5sum(tmp)), unname(tools::md5sum(dest)))
  status <- if (same) "unchanged" else if (file.exists(dest)) "updated" else "new"
  if (!same) file.rename(tmp, dest)
  c(row, list(
    last_modified = resp_header(resp, "last-modified") %||% "", etag = resp_header(resp, "etag") %||% "",
    bytes = sprintf("%.0f", file.size(dest)), md5 = unname(tools::md5sum(dest)),
    downloaded_at = if (same && have) prev$downloaded_at else format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    check = check_file(dest), status = status
  ))
}

write_manifest <- function(rows) {
  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
  rows <- rows[order(rows$page_url, rows$file_url), MANIFEST_COLS]
  tmp <- paste0(MANIFEST, ".tmp")
  write.csv(rows, tmp, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  file.rename(tmp, MANIFEST)
}

selftest <- function() {
  robots <- parse_robots(paste(readLines(ROBOTS_FIXTURE, warn = FALSE), collapse = "\n"))
  stopifnot(
    robots_allows(robots, "/data/occupation-shortage"),
    robots_allows(robots, "/core/misc/a.css"), !robots_allows(robots, "/core/misc/a.php"),
    !robots_allows(robots, "/search/node?keys=x"),
    !robots_allows(robots, "/jobs-and-skills-atlas-pro-dashboard/x"),
    robots_allows(parse_robots("User-agent: *\nDisallow: /data\nAllow: /data/ok$"), "/data/ok"),
    !robots_allows(parse_robots("User-agent: *\nDisallow: /data\nAllow: /data/ok$"), "/data/ok/more"),
    !robots_allows(parse_robots("User-agent: *\nAllow: /\n\nUser-agent: jobsandskills-scraper\nDisallow: /data"), "/data/x"),
    robots_allows(parse_robots("User-agent: other\nDisallow: /"), "/data")
  )

  url <- paste0(SITE, "/data/occupation-shortage")
  p <- parse_page(paste(readLines(FIXTURE, warn = FALSE), collapse = "\n"), url)
  key <- p$files[p$files$file_url == paste0(SITE, "/sites/default/files/2026-10/2026_osl_key_findings_report.pdf"), ]
  stopifnot(
    p$title == "Occupation Shortage", nrow(p$files) == 10,
    key$file_label == "2026 OSL Key Findings Report", key$stated_bytes == "523603",
    setequal(p$links, c("/data", "/data/occupation-shortage/stakeholder-engagement", "/data/skills-priority-list"))
  )
  # pager links keep only ?page=; files on other sites are still listed (main() skips them); mailto ignored
  q <- parse_page(paste0('<main><h1>T</h1><a href="?page=2&amp;sort=x">2</a><a href="/news">n</a>',
                         '<a href="https://example.org/a.PDF">Paper</a><a href="mailto:a@b.c">m</a></main>'), url)
  stopifnot(identical(q$links, "/data/occupation-shortage?page=2"), identical(q$outside, "/news"),
            identical(q$files$file_url, "https://example.org/a.PDF"), identical(q$files$file_label, "Paper"))

  dir <- tempfile("jsa-selftest")
  dir.create(dir)
  old_files_dir <- FILES_DIR
  FILES_DIR <<- file.path(dir, "files")
  on.exit(FILES_DIR <<- old_files_dir)
  for (u in c("/sites/default/files/../../../etc/passwd.csv", "/sites/default/files/%2e%2e/%2e%2e/x.csv")) {
    lp <- local_path(paste0(SITE, u))
    stopifnot(startsWith(lp, FILES_DIR), !grepl("..", lp, fixed = TRUE))
  }
  stopifnot(local_path(paste0(SITE, "/sites/default/files/2026-10/2026%20OSL.xlsx")) ==
              file.path(FILES_DIR, "2026-10", "2026 OSL.xlsx"))

  f <- \(name) file.path(dir, name)
  writeLines("a,b\n1,2", f("ok.csv"))
  file.copy(readxl_example("datasets.xlsx"), f("ok.xlsx"))
  writeLines(c("%PDF-1.4", "1 0 obj <<>> endobj", "%%EOF"), f("ok.pdf"))
  writeLines(c("%PDF-1.4", "1 0 obj <<>>"), f("cut.pdf"))
  utils::zip(f("ok.zip"), f("ok.csv"), flags = "-qj")
  z <- readBin(f("ok.zip"), "raw", file.size(f("ok.zip")))
  writeBin(z[seq_len(length(z) %/% 2)], f("cut.zip"))
  file.create(f("empty.xlsx"))
  writeLines("not a workbook", f("bad.xlsx"))
  checks <- vapply(c("ok.csv", "ok.xlsx", "ok.pdf", "ok.zip", "cut.pdf", "cut.zip", "empty.xlsx", "bad.xlsx"),
                   \(n) check_file(f(n)), "")
  stopifnot(all(checks[1:4] == "ok"), all(checks[5:8] != "ok"))

  # Re-runs: 200 -> new; conditional GET answered 304 -> unchanged; server ignores the conditional and
  # sends the same body -> unchanged; new body -> updated. The mock writes to the .tmp path itself because
  # mocked responses skip req_perform(path =).
  file_url <- paste0(SITE, "/sites/default/files/2026-10/t.csv")
  fl <- list(dataset_title = "T", file_label = "t", page_url = url, file_url = file_url, stated_bytes = NA_character_)
  serve <- function(body, etag, honour = TRUE) function(req) {
    if (honour && identical(req$headers[["If-None-Match"]], etag)) return(response(304, url = req$url))
    writeLines(body, paste0(local_path(req$url), ".tmp"))
    response(200, url = req$url, headers = list(ETag = etag))
  }
  r1 <- with_mocked_responses(serve("a,b\n1,2", "v1"), download(fl, NULL))
  r2 <- with_mocked_responses(serve("a,b\n1,2", "v1"), download(fl, r1))
  r3 <- with_mocked_responses(serve("a,b\n1,2", "v1", honour = FALSE), download(fl, r2))
  r4 <- with_mocked_responses(serve("a,b\n3,4", "v2"), download(fl, r3))
  stopifnot(
    r1$status == "new", r1$check == "ok", r1$etag == "v1", r1$uploaded == "2026-10",
    r2$status == "unchanged", r2$downloaded_at == r1$downloaded_at,
    r3$status == "unchanged", r3$md5 == r1$md5,
    r4$status == "updated", r4$etag == "v2", r4$md5 != r1$md5,
    identical(list.files(dirname(local_path(file_url))), "t.csv")
  )
  message("selftest ok: robots rules, page parse (", nrow(p$files), " files), paths, ",
          length(checks), " file checks, re-run statuses")
}

main <- function() {
  if ("--selftest" %in% commandArgs(trailingOnly = TRUE)) return(invisible(selftest()))
  t0 <- Sys.time()
  rules <- tryCatch(load_robots(), error = \(e) {
    note("failed", paste0(SITE, "/robots.txt"), paste(conditionMessage(e),
         "| RFC 9309: an unreachable robots.txt means complete disallow, so nothing was crawled"))
    NULL
  })
  pages <- 0
  files <- NULL
  counts <- c(new = 0, updated = 0, unchanged = 0)
  rows <- if (file.exists(MANIFEST)) read.csv(MANIFEST, colClasses = "character") else NULL
  if (!is.null(rules)) {
    crawled <- crawl(rules)
    pages <- crawled$pages
    files <- crawled$files
  }
  if (!is.null(files)) {
    host <- vapply(files$file_url, \(u) httr2::url_parse(u)$hostname %||% "", "")
    for (u in files$file_url[host != HOST]) note("skipped", u, "file hosted on another site")
    files <- files[host == HOST, ]
    for (i in seq_len(nrow(files))) {
      f <- files[i, ]
      if (!robots_allows(rules, httr2::url_parse(f$file_url)$path)) {
        note("skipped", f$file_url, "disallowed by robots.txt")
        next
      }
      message(sprintf("[file %d/%d] %s", i, nrow(files), f$file_url))
      prev <- if (is.null(rows)) NULL else rows[rows$file_url == f$file_url, ]
      prev <- if (NROW(prev)) as.list(prev[1, ]) else NULL
      row <- tryCatch(download(f, prev), error = \(e) {
        note("failed", f$file_url, conditionMessage(e))
        NULL
      })
      if (is.null(row)) next
      counts[row$status] <- counts[row$status] + 1
      if (row$check != "ok") note("invalid", f$file_url, row$check)
      if (!is.na(row$stated_bytes) && row$stated_bytes != row$bytes) {
        note("size", f$file_url, sprintf("page states %s bytes, got %s", row$stated_bytes, row$bytes))
      }
      row <- as.data.frame(lapply(row[MANIFEST_COLS], \(x) if (is.null(x) || is.na(x)) "" else as.character(x)))
      rows <- rbind(rows[rows$file_url != f$file_url, ], row)
      write_manifest(rows)  # after every file, so an interrupted run still skips what it already has
    }
  }
  # one row per file on disk: drop rows whose file was deleted by hand
  if (!is.null(rows)) {
    rows <- rows[file.exists(rows$local_path), ]
    write_manifest(rows)
  }

  datasets <- if (is.null(files)) character() else unique(files$dataset_title)
  message(sprintf("\npages %d | datasets with files %d | files linked %d | new %d, updated %d, unchanged %d",
                  pages, length(datasets), NROW(files), counts[["new"]], counts[["updated"]], counts[["unchanged"]]))
  for (d in datasets) message(sprintf("  %3d  %s", sum(files$dataset_title == d), d))
  message(sprintf("manifest %s | requests %d | elapsed %.0fs",
                  if (is.null(rows)) "not written" else sprintf("%s (%d rows)", MANIFEST, nrow(rows)),
                  n_requests, as.numeric(Sys.time() - t0, units = "secs")))
  for (k in unique(problems$kind)) {
    p <- problems[problems$kind == k, ]
    message(sprintf("\n%s (%d):", toupper(k), nrow(p)))
    for (i in seq_len(nrow(p))) message(sprintf("  %s: %s", p$url[i], p$reason[i]))
  }
  if (!interactive() && any(problems$kind %in% c("failed", "invalid"))) quit(status = 1)
}

main()
