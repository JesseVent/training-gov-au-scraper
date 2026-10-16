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

Data © Commonwealth of Australia, sourced from [training.gov.au](https://training.gov.au).
