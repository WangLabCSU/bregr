# Connects dots

**\[stable\]**

Check
[`polar_init()`](https://wanglabcsu.github.io/bregr/reference/polar_init.md)
for examples.

## Usage

``` r
polar_connect(data, mapping, ...)
```

## Arguments

- data:

  A `data.frame` contains connections of all events.

- mapping:

  Set of aesthetic mappings to
  [`ggplot2::geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html).
  Set mappings for `x` and `xend` are required.

- ...:

  Other arguments passing to
  [`ggplot2::geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html).

## Value

A `ggplot` object.

## See also

Other risk_network:
[`br_show_risk_network()`](https://wanglabcsu.github.io/bregr/reference/br_show_risk_network.md),
[`polar_init()`](https://wanglabcsu.github.io/bregr/reference/polar_init.md)
