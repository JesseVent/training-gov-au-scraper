# training-gov-au-scraper

Pulls the packaging rules (rules text + units by group) for every current qualification on
training.gov.au. R with httr2, jsonlite and xml2.

Settings are at the top of `scrape.R`:

```r
MAX_QUALIFICATIONS  <- Inf  # how many to scrape, in code order; Inf = all (~1,153)
REQUESTS_PER_SECOND <- 2    # rate limit across all requests
MAX_RETRIES         <- 4    # retries on 429/5xx/network errors (honours Retry-After)
```

```bash
Rscript scrape.R --selftest   # parser check against fixtures/CHC33015_bundle.json
Rscript scrape.R              # scrape using the config above
```

In RStudio, `setwd()` to this folder first, then `source("scrape.R")`.

Each qualification takes 3 requests:
1. `/api/training/{code}` gives the current release.
2. `/api/training/{code}/releases/{n}?include=All` gives the content bundle ID.
3. `/api/content/bundle/{id}` returns the bundle; the script keeps its `PackagingRules` item.

No auth or cookies are needed. A full run at 2 requests/s takes about 29 minutes.

- `raw/{CODE}.json`: cached API responses. Cached codes are skipped, so re-running resumes, and raising
  `MAX_QUALIFICATIONS` only fetches the new ones. Delete a file to re-fetch it.
- `out/qualifications.jsonl`: `code, title, release, bundle_id, rules_text, units[{group, code, title, nrt_type}]`
- `out/packaging_rules.csv`: the packaging rules text, one row per qualification.
- `out/units.csv`: the same units, one row each. All three files are rebuilt from `raw/` on every run.

A unit's group is the heading above it: a `<strong>`/plain paragraph or a heading row inside the table.
Units are listed in table rows, or one per paragraph/bullet as `CODE Title`, depending on the package.
Special groups:
- Groups starting with `Prerequisite` (`Prerequisite requirements`, `Prerequisite Units`, ...): prerequisite
  tables, not group membership.
- `(rules text)`: units or qualifications mentioned in the rules prose, e.g. "must include CUASOU331".
- empty: units listed before any heading (a few AVI and POL qualifications).

Filter out the first two when you want membership only. `nrt_type` is `unit` or `unitOfCompetency`
depending on the package. The run summary lists any qualification with no listed units; check those by hand.

`out/` holds a full run of all 1,153 current qualifications (2026-10-16): 60,830 unit rows. 14 restricted
Police qualifications (POL50225, POL60118, ...) have no packaging rules; their content is withheld from the
register and available only through ANZPAA. On a 31-qualification sample, core units match
training.gov.au's unit grid 31/31 and full unit sets 28/31; the 3 differences are inconsistencies on the
source side (the rules page and the grid disagree).

## Skill-group classification

`classify.R` asks OpenAI Decisions (`gpt-6-luna`) to pick a skill group for every course in
`data/Course_List.csv`, from the file's own 29 `skill_area_group` values. Each choice's description lists
that group's sub-areas. It runs twice:
- `blind`: course name, training package and occupations;
- `subarea`: the same plus `skill_area_description`.

```bash
OPENAI_API_KEY=... Rscript classify.R            # both runs, all courses
Rscript classify.R --limit 5                     # smoke run on the first 5 courses
Rscript classify.R --selftest                    # parser check against fixtures/decisions_response.json
```

Paid: about 1,050 input tokens per call at $0.10/M, so about $0.50 for both runs (4,842 calls).
Responses are cached in `raw/decisions/{run}/{CODE}.json`, so a rerun only calls what's missing.
`out/course_skill_groups.csv` is the input columns plus, per run, `_group`, `_probability`,
`_confidence`, `_reason` (why there's no answer) and `_agrees` (matches `skill_area_group`).

It also carries each course's `description` and `packaging_rules` from the scrape (run `scrape.R` first).
Superseded courses have no content of their own on training.gov.au, so they take the text of the current
qualification that superseded them, found by following `mappingInformation` on `/api/training/{code}`
(free, 2 requests/s, cached in `raw/lineage/`). `overview_code` says whose text it is, `overview_hops`
how many supersessions away (0 = the course's own), and `overview_reason` why there's none.

Each course is also matched to the occupation it mainly trains for on Jobs and Skills Australia's 2026
Occupation Shortage List (`data/2026_occupation_shortage_list_-_6_digit_anzsco_and_osca.xlsx`, downloaded by
hand from [jobsandskills.gov.au](https://www.jobsandskills.gov.au/data/occupation-shortage); the site
rejects scripted requests), once in ANZSCO 2022 and once in OSCA 2024. Decisions takes at most 255
choices, so it picks the 2-digit occupation group first, then the occupation among the two likeliest
groups' occupations. The model sees course name, package, occupations, skill area and the first 1,000
characters of the description. About 6,000 input tokens per course per code set, so about $2.90 for both.
Columns per set (`anzsco_`, `osca_`): `code`, `title`, `probability` (of the occupation among the
candidates), `rating` (2026 national rating: Shortage, No shortage, Regional shortage, Metro shortage),
`rating_confidence` (Clear or On-balance), `reason`.

## Jobs and Skills Australia data files

`scrape_jobsandskills.R` downloads every file linked under
[jobsandskills.gov.au/data](https://www.jobsandskills.gov.au/data) (PDF, XLSX, DOCX, ZIP, ...) in its original
format. Settings (`MAX_PAGES`, `REQUESTS_PER_SECOND <- 1`, `MAX_RETRIES <- 4`) are at the top.

```bash
Rscript scrape_jobsandskills.R --selftest   # robots, page parse, file checks and re-run logic, offline
Rscript scrape_jobsandskills.R              # crawl and download
```

- It reads robots.txt first. An unreachable robots.txt counts as complete disallow (RFC 9309), so the run stops.
- It crawls `/data` and every `/data/...` page linked from a page's `<main>`, breadth-first, keeping `?page=`
  pager links. Datasets the landing page links outside `/data` (`/studies/...`, `/research/studies/...`,
  the Atlas) and files hosted on other sites are listed as skipped, not fetched.
- Files go to `data/jobsandskills/files/{YYYY-MM}/{filename}`, mirroring the site's upload folder (git-ignored).
  Each file is checked after download: CSVs load, XLSX sheets open, ZIP/DOCX pass `unzip -t`, PDFs have a
  header and an `%%EOF`.
- `data/jobsandskills/manifest.csv` has one row per downloaded file: `dataset_title` (the page's h1),
  `file_label`, `page_url`, `file_url`, `uploaded` (the YYYY-MM folder), `last_modified` and `etag` (HTTP
  headers), `stated_bytes` (the page's figure) and `bytes`, `md5`, `local_path`, `downloaded_at`, `check`. The
  pages show no per-file publish date, only future release calendars.
- Re-runs send `If-None-Match`/`If-Modified-Since` for files already on disk and skip anything unchanged. Files
  keep the same path, so nothing is duplicated.
- Failed pages and files are logged and the run carries on. The summary lists every failure, skip, invalid
  file and size mismatch, and the script exits 1 if anything failed.

**It doesn't currently get through.** jobsandskills.gov.au's firewall resets every request that doesn't look
exactly like Chrome. That includes this script's identifying User-Agent, and robots.txt itself (HTTP/2
`INTERNAL_ERROR`, 2026-10-23). So a run stops at robots.txt and collects nothing. It needs JSA to allowlist
the User-Agent, or an explicit decision to present as a browser.

Data © Commonwealth of Australia, sourced from [training.gov.au](https://training.gov.au).
