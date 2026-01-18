# GitHub Copilot Instructions for bregr R Package

This file contains comprehensive instructions for GitHub Copilot to
effectively work with the `bregr` R package repository.

## Repository Overview

The `bregr` package is an R package for easy and efficient batch
processing of regression models. It provides tools for running
univariate and multivariate regression models in batch, returning
results in tidy format with visualization capabilities.

### Package Structure

- **Type**: Standard R package
- **Language**: R (\>= 4.1.0)
- **Testing**: testthat framework
- **Documentation**: roxygen2 + pkgdown
- **CI/CD**: GitHub Actions with R-CMD-check
- **Dependencies**: tidyverse ecosystem, statistical modeling packages

### Key Components

- `R/`: Core R functions for batch regression modeling
- `tests/testthat/`: Unit tests and example tests
- `man/`: Auto-generated documentation files
- `vignettes/`: Package vignettes and tutorials
- `data-raw/`: Raw data processing scripts
- `DESCRIPTION`: Package metadata and dependencies

## Development Environment Setup

### R Installation (Using rig + pak - RECOMMENDED)

Use the modern R installation manager `rig` and package installer `pak`
for optimal development experience:

``` bash
# Install rig (R Installation Manager)
curl -Ls https://github.com/r-lib/rig/releases/download/latest/rig-linux-latest.tar.gz | sudo tar xz -C /usr/local

# Install latest R release
sudo rig add release

# Verify installation
rig list
R --version
```

**Timing**: rig installation ~1 minute, R installation ~5-8 minutes

### Package Dependencies Installation

``` bash
# Navigate to package directory
cd /path/to/bregr

# Install all package dependencies using pak (much faster than install.packages)
R --slave -e "pak::local_install_deps()"

# Install additional development tools
R --slave -e "pak::pak(c('knitr', 'rmarkdown', 'testthat', 'pkgdown'))"
```

**Timing**: Dependencies installation ~4-6 minutes, dev tools ~2-3
minutes

### System Dependencies

Essential system packages (automatically handled by pak): - `pandoc`
(for vignettes and documentation) - `libxml2-dev`,
`libcurl4-openssl-dev`, `libssl-dev` (for various R packages) - Graphics
libraries: `libfreetype6-dev`, `libjpeg-dev`, `libpng-dev`,
`libtiff-dev`

## Build and Testing Commands

### Package Building

``` bash
# Quick build (without vignettes) - recommended for development
R CMD build . --no-build-vignettes

# Full build (with vignettes) - for release
R CMD build .
```

**Timing**: Quick build ~10-20 seconds, Full build ~5-10 minutes

### Package Checking

``` bash
# Quick check (skip suggested packages)
_R_CHECK_FORCE_SUGGESTS_=false R CMD check package_*.tar.gz --no-manual

# Full check (requires all suggested packages)
R CMD check package_*.tar.gz --no-manual
```

**Timing**: Quick check ~2-3 minutes, Full check ~10-15 minutes

### Running Tests

``` bash
# Install package locally first
R --slave -e "pak::local_install('.')"

# Run tests
R --slave -e "testthat::test_dir('tests/testthat')"

# Test basic functionality
R --slave -e "library(bregr); mtcars_result <- br_pipeline(mtcars[1:10,], y='mpg', x=c('cyl','disp'), method='gaussian'); print('Success!')"
```

**Timing**: Local install ~10-15 seconds, Tests ~30-60 seconds

### Documentation Building

``` bash
# Build pkgdown site
R --slave -e "pkgdown::build_site()"

# Update documentation
R --slave -e "devtools::document()"
```

**Timing**: pkgdown build ~2-5 minutes, documentation update ~30 seconds

## Package Functionality

### Core Features

- [`br_pipeline()`](https://wanglabcsu.github.io/bregr/reference/pipeline.md):
  Main function for batch regression modeling
- `br_show_*()`: Visualization functions (forest plots, tables,
  networks)
- Support for multiple regression methods: gaussian, binomial, cox, etc.
- Tidy output format compatible with broom ecosystem

### Supported Models

- Linear regression (`gaussian`)
- Logistic regression (`binomial`)
- Cox proportional hazards (`coxph`)
- Poisson regression (`poisson`)
- And more (see vignettes for full list)

### Example Usage

``` r
library(bregr)

# Simple linear regression example
result <- br_pipeline(
  data = mtcars,
  y = "mpg",
  x = c("cyl", "disp", "hp"),
  method = "gaussian"
)

# Visualize results
br_show_forest(result)
br_show_table(result)
```

## Development Workflow

### Making Changes

1.  Edit R functions in `R/` directory
2.  Update documentation with roxygen2 comments
3.  Run `devtools::document()` to update man files
4.  Test changes with `testthat::test_dir('tests/testthat')`
5.  Build and check package with `R CMD build` and `R CMD check`

### Adding New Functions

1.  Create function in appropriate R file in `R/` directory
2.  Add roxygen2 documentation with `@export` if public function
3.  Add unit tests in `tests/testthat/`
4.  Update `NAMESPACE` with `devtools::document()`
5.  Consider adding examples to vignettes if it’s a major feature

### Performance Considerations

- Use `pak` instead of
  [`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
  for faster package management
- Skip vignette building during development iterations
- Use `_R_CHECK_FORCE_SUGGESTS_=false` for faster checking
- Use `_R_CHECK_DEPENDS_ONLY_=true` for depends checking
- Consider parallel testing for large test suites

## Common Issues and Solutions

### Build Issues

- **Vignette build failures**: Often due to missing suggested packages,
  build without vignettes for development
- **Dependency conflicts**: Use
  [`pak::pak_sitrep()`](https://pak.r-lib.org/reference/pak_sitrep.html)
  to diagnose issues
- **System library missing**: pak will usually install automatically,
  but check error messages

### Testing Issues

- **Package not found errors**: Make sure to install package locally
  with `pak::local_install('.')`
- **Suggested package errors**: Install missing packages or use
  `_R_CHECK_FORCE_SUGGESTS_=false`

### Documentation Issues

- **Missing exports**: Add `@export` to roxygen2 comments and run
  `devtools::document()`
- **Cross-reference errors**: Check that referenced packages are
  available

## Performance Metrics (Typical Timings)

- **rig setup**: ~1 minute
- **R installation**: ~5-8 minutes  
- **Package dependencies**: ~4-6 minutes
- **Quick build**: ~10-20 seconds
- **Quick check**: ~2-3 minutes
- **Local install**: ~10-15 seconds
- **Test suite**: ~30-60 seconds
- **pkgdown build**: ~2-5 minutes
- **Full check**: ~10-15 minutes
- **Full build with vignettes**: ~5-10 minutes

## Best Practices

1.  **Use rig and pak**: Modern, faster alternatives to traditional R
    installation and package management
2.  **Incremental development**: Use quick builds and checks during
    development
3.  **Test early and often**: Install locally and test after each
    significant change
4.  **Documentation-driven development**: Keep roxygen2 comments up to
    date
5.  **Leverage CI/CD**: Let GitHub Actions handle full checks for pull
    requests
6.  **Version control**: Use meaningful commit messages and atomic
    commits

## Troubleshooting

### Package Manager Issues

``` bash
# Update pak
R --slave -e "pak::pak_update()"

# Clear pak cache
R --slave -e "pak::cache_clean()"

# Check pak status
R --slave -e "pak::pak_sitrep()"
```

### R Environment Issues

``` bash
# List installed R versions
rig list

# Switch R version if needed
rig default 4.5.1

# Check R configuration
R --slave -e "sessionInfo()"
```

This setup provides a modern, efficient development environment for the
bregr R package using best practices and contemporary tools.
