# saros 0.1.1
* Added: col_to_binaries() utility function for mutating a single variable into multiple binary (dummy) variables, but does not exclude one binary. Useful as input for report_chart_likert() if wanting a non-stacked chart (frequency distribution)
* Bugfixes

* Added: by-argument for report_chart_likert(): You can now make figures split by a background variable.
* Added: figure caption to report_chart_likert() with label_separator.
* Added: crosstable_to_apa() which is a post-processing function to format a crosstable-object to APA-standard and also add caption and footer with N=.  
* Better error messages for report_chart_likert() when there are multiple cols and by-columns.
* Added a `NEWS.md` file to track changes to the package.
