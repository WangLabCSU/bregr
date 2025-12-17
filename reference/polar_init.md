# Init a dot plot in polar system

**\[stable\]**

## Usage

``` r
polar_init(data, mapping, ...)
```

## Arguments

- data:

  A `data.frame` contains all events, e.g., genes.

- mapping:

  Set of aesthetic mappings to
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).
  You should not set mapping for `y`.

- ...:

  Other arguments passing to
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

## Value

A `ggplot` object.

## See also

Other risk_network:
[`br_show_risk_network()`](https://wanglabcsu.github.io/bregr/reference/br_show_risk_network.md),
[`polar_connect()`](https://wanglabcsu.github.io/bregr/reference/polar_connect.md)

## Examples

``` r
library(ggplot2)
# -------------------
#  Init a polar plot
# -------------------

data <- data.frame(x = LETTERS[1:7])

p1 <- polar_init(data, aes(x = x))
p1


# Set aes value
p2 <- polar_init(data, aes(x = x), size = 3, color = "red", alpha = 0.5)
p2


# Set aes mapping
set.seed(123L)
data1 <- data.frame(
  x = LETTERS[1:7],
  shape = c("r", "r", "r", "b", "b", "b", "b"),
  color = c("r", "r", "r", "b", "b", "b", "b"),
  size = abs(rnorm(7))
)
# Check https://ggplot2.tidyverse.org/reference/geom_point.html
# for how to use both stroke and color
p3 <- polar_init(data1, aes(x = x, size = size, color = color, shape = shape), alpha = 0.5)
p3


# --------------------
#  Connect polar dots
# --------------------
data2 <- data.frame(
  x1 = LETTERS[1:7],
  x2 = c("B", "C", "D", "E", "C", "A", "C"),
  color = c("r", "r", "r", "b", "b", "b", "b")
)
p4 <- p3 + polar_connect(data2, aes(x = x1, xend = x2))
p4


p5 <- p3 + polar_connect(data2, aes(x = x1, xend = x2, color = color), alpha = 0.8, linetype = 2)
p5


# Use two different color scales
if (requireNamespace("ggnewscale")) {
  library(ggnewscale)
  p6 <- p3 +
    new_scale("color") +
    polar_connect(data2, aes(x = x1, xend = x2, color = color), alpha = 0.8, linetype = 2)
  p6 + scale_color_brewer()
  p6 + scale_color_manual(values = c("darkgreen", "magenta"))
}
```
