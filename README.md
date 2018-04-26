This is a fork of the [CRAN package BLPEstimatoR](https://cran.r-project.org/web/packages/BLPestimatoR) by Daniel Brunner (aut), Constantin Weiser (ctr), Andre Romahn (ctr)

I just made a series of small changes to add some convinience features to the package:


1. Add function `BLP.factor.to.dummies` and call it in `estimateBLP`. It allows variables in Xlin, Xrandom or instruments to be factors (or characters) and automatically converts them to dummies, as for example is done in R's `lm` function.

2. Add argument `prodid` to `estimateBLP` to allow automatic naming of elasticities matrices. Adapt `get.Elasticities` correspondingly

3. Add the function `elasticitiesBLP` for convenient extraction of elasticities.