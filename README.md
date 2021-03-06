[![Appveyor build Status](https://ci.appveyor.com/api/projects/status/github/KWB-R/kwb.vs2dh?branch=master&svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-vs2dh/branch/master)
[![Travis build Status](https://travis-ci.org/KWB-R/kwb.vs2dh.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.vs2dh)
[![codecov](https://codecov.io/github/KWB-R/kwb.vs2dh/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.vs2dh)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.vs2dh)]()

# kwb.vs2dh

Interface to the open-source model USGS model VS2DH for simulation of water and energy transport in variable-saturated porous media (for more information see: http://wwwbrr.cr.usgs.gov/projects/GW_Unsat/vs2di1.3/).

## Installation

For more details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Option: specify GitHub Personal Access Token (GITHUB_PAT)
### see: https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat
### why this might be important for you!

#Sys.setenv(GITHUB_PAT = "mysecret_access_token")

if (!require("remotes")) {
install.packages("remotes", repos = "https://cloud.r-project.org")
}

### Temporal workaround to due bug in latest CRAN of R package remotes v2.0.2
### on Windows(for details: see https://github.com/r-lib/remotes/issues/248)

remotes::install_github("r-lib/remotes@18c7302637053faf21c5b025e1e9243962db1bdc")
remotes::install_github("KWB-R/kwb.vs2dh")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.vs2dh](https://kwb-r.github.io/kwb.vs2dh)

Development: [https://kwb-r.github.io/kwb.vs2dh/dev](https://kwb-r.github.io/kwb.vs2dh/dev)
