
<!-- README.md is generated from README.Rmd. Please edit that file -->

# abstr

<!-- badges: start -->

[![R-CMD-check](https://github.com/Robinlovelace/abstr/workflows/R-CMD-check/badge.svg)](https://github.com/Robinlovelace/abstr/actions)
<!-- badges: end -->

The goal of abstr is to provide an R interface to the A/B Street
transport planning/simulation game. In the first instance, it provides a
way to generate scenarios of change and saving them as `.json` files
that can be directly imported into the A/B Street game. See
<https://a-b-street.github.io/docs/dev/formats/scenarios.html#example>
for details of the schema that the package outputs.

## Installation

You can install the released version of abstr from
<!-- [CRAN](https://CRAN.R-project.org) with: --> GitHub as follows:

``` r
remotes::install_github("cyipt/abstr")
```

## Example

``` r
library(abstr)
dslines = leeds_desire_lines
ablines = ab_scenario(
 leeds_houses,
 leeds_buildings,
 dslines,
 leeds_zones,
 output_format = "sf"
)
plot(dslines$geometry, lwd = dslines[[3]] / 30)
plot(leeds_site_area$geometry, add = TRUE)
plot(leeds_buildings$geometry, add = TRUE)
plot(ablines$geometry, col = "blue", add = TRUE)
```

<img src="man/figures/README-output-sf-1.png" width="100%" />
