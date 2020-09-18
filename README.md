
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggip

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggip)](https://CRAN.R-project.org/package=ggip)
[![R build
status](https://github.com/davidchall/ggip/workflows/R-CMD-check/badge.svg)](https://github.com/davidchall/ggip/actions)
[![Coverage
status](https://codecov.io/gh/davidchall/ggip/branch/master/graph/badge.svg)](https://codecov.io/gh/davidchall/ggip?branch=master)
<!-- badges: end -->

ggip is a [{ggplot2}](https://ggplot2.tidyverse.org) extension for
visualizing IP addresses and networks stored in
[{ipaddress}](https://davidchall.github.io/ipaddress/) vectors.

Here are some of the key features:

  - IP data mapped to 2D plane by a **unified coordinate system**
  - Compatible with **existing ggplot2 layers**
  - Custom **IP-specific layers** for common use cases
  - Full support for both **IPv4 and IPv6** address spaces

## Installation

You can install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("davidchall/ggip")
```

## Usage

Plotting with {ggip} follows most of the conventions set by
[{ggplot2}](https://ggplot2.tidyverse.org). A major difference is that
`coord_ip()` is required to map IP data to the 2D grid (addresses to
points and networks to rectangles). Learn more in `vignette("ggip")`.

Here’s a quick showcase of what’s possible:

``` r
library(ggplot2)
library(ggip)
library(ggfittext)

ggplot(iana_ipv4) +
  stat_summary_address(aes(ip = address), data = ip_data) +
  geom_rect(
    aes(xmin = network$xmin, xmax = network$xmax, ymin = network$ymin, ymax = network$ymax),
    alpha = 0.2, fill = "white"
  ) +
  geom_fit_text(aes(
    xmin = network$xmin, xmax = network$xmax, ymin = network$ymin, ymax = network$ymax,
    label = label, color = allocation
  ), reflow = TRUE) +
  scale_fill_viridis_c(trans = "log2", na.value = "black", guide = "none") +
  scale_color_brewer(name = NULL, palette = "Accent") +
  coord_ip(pixel_prefix = 20) +
  theme_ip_dark()
#> Warning: Transformation introduced infinite values in discrete y-axis
```

<img src="man/figures/README-ipv4-heatmap-1.png" width="100%" />
