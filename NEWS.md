# treenetproc 0.2.2
* Fixed bug when data came with already formatted `ts` field in the input dendro data. Now, treenetproc skips the as.POSIXct() transformation of the input timestamp (ts) when is already formatted as datetime (POSIXct)

# treenetproc 0.2.1

* Fixed bug when the data is too homogeneous, making the outlier detection mechanism to fail and throw too many outliers no matter the `tol` values. This happens when the statistic used. Mean Absolute Deviation (MAD) results to 0. Now, in such case, we fallback to an alternative statistic to measure deviation from the mean (Mean Absolute Error, MAE), in order to detect outliers.

# treenetproc 0.2.0

* Added plot_name param to `corr_dendro_L2()`
* Fixed bug in `forcejump()`
* Added option force.now to `corr_dendro_L2()` that enfoces a jump in the exact moment given by the user. This is because the `force` option decides where to do the jump and might not the most suitable place.
* Fixed some bugs when reversing corrections.
* Define new function `recalc_growth_variables()`, that allows to recalculate the growth variables when the input data has changed (for instance, if the start of the data has changed or the data has been filtered)
