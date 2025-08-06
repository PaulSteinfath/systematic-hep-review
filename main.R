# Initialize the workspace: load dependencies and functions
source('init_workspace.R')

# Paths
data_path <- file.path(getwd(), 'data')
raw_path <- file.path(data_path, 'raw')
derivatives_path <- file.path(data_path, 'derivatives')
manual_path <- file.path(raw_path, 'manual.csv')
pubmed_path <- file.path(raw_path, 'pubmed.csv')
prisma_template_path <- file.path(raw_path, 'PRISMA_template.csv')
prisma_path <- file.path(derivatives_path, 'PRISMA.csv')
results_path <- file.path(getwd(), 'results')
dir.create(derivatives_path, showWarnings = F)
dir.create(results_path, showWarnings = F)

# Load and preprocess the data
# TODO: download the data from OSF / wherever we put it?
df_full <- load_data(pubmed_path, manual_path)
c(df_screening, df_included) %<-% preprocess(df_full)
errors <- validate_preprocessed(df_included)

# Generate main figures
ext = 'png'
generate_prisma(df_screening, prisma_template_path,
                derivatives_path, results_path, ext = ext)
figure_overview_studies(df_included, results_path, ext = ext)
figure_overview_pipelines(df_included, results_path, ext = ext)
figure_meeg_acq_prep(df_included, results_path, ext = ext)
figure_ecg_summary(df_included, results_path, ext = ext)
figure_cfa_removal(df_included, results_path, ext = ext)
figure_hep_estimation_summary(df_included, results_path, ext = ext)
figure_stats(df_included, results_path, ext = ext)
figure_controls(df_included, results_path, ext = ext)

# Generate supplementary figures
figure_additional_hedges_g(df_included, results_path, ext = ext)
figure_epoch_simulation(df_included, results_path, ext = ext)
figure_control_variables(df_included, results_path, ext = ext)

# Save the results
write.csv(df_included, file.path(derivatives_path, 'included.csv'), row.names = F)
write.csv(df_screening, file.path(derivatives_path, 'screening.csv'), row.names = F)

# R Markdown report with data from global environment
rmarkdown::render(file.path(func_path, 'manuscript_statistics.Rmd'), 
                  knit_root_dir = getwd(),
                  output_format = 'html_document',
                  output_dir = results_path)
