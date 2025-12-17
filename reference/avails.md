# Package availability

**\[stable\]**

Package resource, definitions ready for use.

## Usage

``` r
br_avail_methods()

br_avail_methods_use_exp()

br_avail_method_config(method)
```

## Arguments

- method:

  Method for model construction. See `br_avail_methods()` for available
  options.

## Value

A character vector representing the available methods or options.

## Functions

- `br_avail_methods()`: Returns available modeling methods. This
  correlates to
  [`br_set_model()`](https://wanglabcsu.github.io/bregr/reference/pipeline.md).

- `br_avail_methods_use_exp()`: Returns available modeling methods which
  set `exponentiate=TRUE` at default by **bregr**.

- `br_avail_method_config()`: Returns model configs for specified method
  to generate modeling templates.

## See also

[pipeline](https://wanglabcsu.github.io/bregr/reference/pipeline.md) for
building `breg` objects.
