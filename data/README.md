This repository contains raw and preprocessed data that support the systematic review entitled "Heartbeat-evoked responses in M/EEG: A systematic review of methods with suggestions for analysis and reporting".

The data is structured in the following way:

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

For more details, please see the corresponding paper (TODO: link) and the repository with analysis scripts (https://github.com/PaulSteinfath/systematic-hep-review). If you used the data in your project, please consider citing the review:

> TODO: citation