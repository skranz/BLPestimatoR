1. Add function BLP.factor.to.dummies and call it in estimateBLP. It allows variables in Xlin to be factors (or characters) and automatically converts them to dummies, as for example is done in R's `lm` function.

2. Add argument 'prodid' to estimateBLP to allow automatic naming of elasticities matrices. Change get.Elasticities correspondingly

3. Add the convinience function elasticitiesBLP for simpler extraction of elasticities.