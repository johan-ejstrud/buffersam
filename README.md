# buffersam
This package implements the buffered random sampling algorithm described in the paper: Kingsley, M., Kanneworff, P., & Carlsson, D. (2004). Buffered random sampling: a sequential inhibited spatial point process applied to sampling in a trawl survey for northern shrimp Pandalus borealis in West Greenland waters. Ices Journal of Marine Science, 61, 12-24.

# Installation
Install `buffersam` by running:
```r
devtools::install_github("johan-ejstrud/buffersam", build_vignettes = TRUE)
```

# How to use
After installation, see the details on how to use the package by running:
```r
vignette("buffersam")
```

# GitHub page
https://github.com/johan-ejstrud/buffersam

# Versioning
When a new version is ready
1. Run `usethis::use_version("patch/minor/major")`.
1. Update `NEWS.sh` and commit.
1. Run `./tag_release.sh`. This tags and pushes the commit.
1. Open https://github.com/johan-ejstrud/buffersam/tags and make release.
