# training-gov-au-scraper

Pulls the packaging rules (rules text + units by group) for every current qualification on
training.gov.au. R with httr2, jsonlite and xml2.

Settings are at the top of `scrape.R`:

```r
MAX_QUALIFICATIONS  <- 20   # how many to scrape, in code order; Inf = all (~1,153)
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
- `out/units.csv`: the same units, one row each. Both files are rebuilt from `raw/` on every run.

Groups are the heading line above each unit table. Two special groups:
- `Prerequisite requirements`: prerequisite tables, not group membership.
- `(rules text)`: units or qualifications mentioned in the rules prose.

Filter on both when you want membership only. The run summary lists any qualification with no units in a
table; check those by hand.

`out/` in this repo is sample output: the first 20 qualifications by code, from a test run on 2026-10-16.

Data © Commonwealth of Australia, sourced from [training.gov.au](https://training.gov.au).
