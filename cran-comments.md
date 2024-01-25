## Resubmission to new release

## R CMD check results

0 errors | 0 warnings | 2 notes

## Responses to manual review:
  * Note that there are not yet any references to describe the methods used in the package, nor are there any new statistical methods applied.
  * Description field in DESCRIPTION now has only single spaces.
  * list_available_element_types and render_full_reports now have return values.
  * Unexported functions now have no examples.
  * All \dontrun-tags are removed, except one which is supposed to result in error and \donttest for embed_cat_freq_plot which takes > 5 sec.
  * Removed default save locations and getwd() in arguments.
  * No function writes by default to working directory/package folder. 
  * The NOTE: Found the following files/directories: ''NULL'' is due to a bug in running the examples. (I can prove this is not particular to my package).
  * Additonally, customizable has been rewritten to customized, in the description field.
  * Following {goodpractice}-package I have also replaced sapply with vapply to ensure robustness.
  * All cran checks have been rerun again.
