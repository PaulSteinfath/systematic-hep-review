# Systematic review of methods used in HEP research 

## Folder structure

* `app` - UI and server functions for the Shiny app.
* `assets` - required static files that are not created based on the data.
* `data` - all data is expected to be stored in this folder.
  * `derivatives` - all produced data can be stored here.
  * `HEP - Pubmed Results.csv` - the CSV file of our table (Pubmed tab).
  * `HEP - Manual.csv` - the CSV file of our table (Manual tab).
* `functions` - helper functions for different parts of the analysis.
* `results` - all plots can be stored here (this folder is ignored by Git).
* `tests` - if needed, tests for our functions.

## Development

Validating the table against the codebook: run the `validate.py` script.

Main analysis: run the `main.R` script. 

Web application: run `runApp("app")` from the root directory.

Running tests: ``testthat::test_dir("tests")``
