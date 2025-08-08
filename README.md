# Systematic review of methods used in HEP research 

## Folder structure

* `assets` - required static files that are not created based on the data.
* `data` - all project data are stored in this folder.
  * `exports` - original CSVs that were exported from Pubmed
  * `raw` - data that was extracted from all reviewed studies
    * `pubmed.csv` - data from studies that were retrieved via Pubmed
    * `manual.csv` - data from studies that were retrieved manually
    * `codebook.csv` - codebook that was used for validating the data
    * `PRISMA_template.csv` - template file for the PRISMA diagram
  * `derivatives` - output files of the analysis scripts
    * `screening.csv` - screening information for all reviewed studies
    * `included.csv` - extracted information for all studies included in the review
    * `PRISMA.csv` - data used to generate the PRISMA diagram
* `functions` - helper functions for different parts of the analysis.
* `results` - all output plots.
* `rsconnect` - configuration files for the Shiny app.
* `tests` - if needed, tests for our functions.
* `init_workspace.R` - project dependencies.
* `main.R` - main entry point of the analysis.
* `server.R` - backend of the Shiny app.
* `ui.R` - frontend of the Shiny app.
* `validate.py` - script that validates the raw tables (`pubmed.csv` / `manual.csv`) against the codebook (`codebook.csv`)

## Development

Main analysis: run the `main.R` script.

Validating the table against the codebook (requires `numpy` and `pandas` packages, tested with Python 3.11.5):

  * for studies retrieved from Pubmed:

  ```
  python ./validate.py
  ```

  * for studies added manually:

  ```
  python ./validate.py --manual
  ```

Web application: run `runApp("app")` from the root directory.

Running tests: ``testthat::test_dir("tests")``
