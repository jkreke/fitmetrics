# fitmetrics
fit.R measures how good, with respect to noise, a particular fit metric is, such as R-Squared or RMSE.  The point is to compare one’s measure to the likelihood of noise giving that same measure.  R-Squared is often used, even for small values of degrees of freedom.  This package shows that R-Squared is very sensitive to noise at low degrees of freedom and should probably be avoided, especially when there are only 3 or 4 degrees of freedom.
