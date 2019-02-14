# PsNR
R package for pharmacometric utilities used by PsN

This package is needed for using the -rplots functionality in PsN. Currently this package does not contain all R functions available in PsN. This package removes the need for the old install_R_packages.R script as all dependencies will be taken care of by installing this package.

# Installation instructions

This package can be installed using the devtools package with: ``devtools::install_github("UUPharmacometrics/PsNR")``

Note that this will use the default R library and upgrade dependencies in that library if needed.
For installation in a separate library use ``.libPaths("path_to_my_library")`` before installing.
