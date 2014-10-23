WRSEL
=====

Function for computing estimates under a Weighted Rank Squared Error Loss function, with the option of parallelising intensive computations. Please see the accompanying notebook http://nbviewer.ipython.org/github/andrewzm/WRSEL/blob/master/ipython/WRSEL_Example_1.ipynb and (after having read the notebook!) the associated RShine application at https://andrewzm.shinyapps.io/WRSEL_example/ for an explanation of WRSEL and an interactive toy study.

This package is still under development and is not available on CRAN. To install you will need to have `devtools` installed and loaded. Then type in  `install_github('andrewzm/WRSEL')`. Please do not hesitate to contact me if you have any queries. 

Package details
------------------

Package: WRSEL

Type: Package

Title: Weighted Rank Squared Error Loss

Version: 1.0

Date: 2014-09-18

Author: Andrew Zammit Mangion

Maintainer: Andrew Zammit Mangion <andrewzm@gmail.com>

Description: Find the optimal estimate under a weighted rank squared error loss
    (WRSEL) function. For details see Wright, Deanne L., Hal S. Stern, and Noel
    Cressie. "Loss functions for estimation of extrema with an application to
    disease mapping." Canadian Journal of Statistics 31.3 (2003): 251-266.

Suggests:
    parallel

Depends:
    R (>= 2.14)

Imports:
    Matrix

License: GPL (>= 2)

NeedsCompilation: no
