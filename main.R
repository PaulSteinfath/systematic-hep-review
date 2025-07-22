library(cowplot)
library(dplyr)
library(eegUtils)
library(grid)
library(gridExtra)
library(gridGraphics)
library(ggimage)
library(ggplot2)
library(ggtext)
library(magick)
library(PRISMA2020)
library(purrr)
library(scales)
library(stringr)
library(testit)
library(tidyr)
library(viridis)
library(zeallot)
library(pwr)
library(ggridges)

# Paths
data_path <- file.path(getwd(), 'data')
func_path <- file.path(getwd(), 'functions')
manual_path <- file.path(data_path, 'HEP - Manual.csv')
pubmed_path <- file.path(data_path, 'HEP - Pubmed Results.csv')
prisma_template_path <- file.path(data_path, 'PRISMA_template.csv')
derivatives_path <- file.path(data_path, 'derivatives')
prisma_path <- file.path(derivatives_path, 'PRISMA.csv')
results_path <- file.path(getwd(), 'results')
dir.create(derivatives_path, showWarnings = F)
dir.create(results_path, showWarnings = F)

# Imports
source(file.path(func_path, 'figures.R'))
source(file.path(func_path, 'layouts.R'))
source(file.path(func_path, 'preprocess.R'))
source(file.path(func_path, 'prisma.R'))
source(file.path(func_path, 'validate.R'))
source("config.R")

# Main analysis
# TODO: download the data from OSF / wherever we put it?
df_full <- load_data(pubmed_path, manual_path)
c(df_screening, df_included) %<-% preprocess(df_full)
errors <- validate_preprocessed(df_included)
generate_prisma(df_screening, prisma_template_path,
                derivatives_path, results_path, ext = 'png')

figure_ecg_summary(df_included, results_path, ext = 'png')
make_figures(df_included, results_path, ext = 'png')

# Save the results
write.csv(df_included, file.path(derivatives_path, 'included.csv'), row.names = F)
write.csv(df_screening, file.path(derivatives_path, 'screening.csv'), row.names = F)

# R Markdown report with data from global environment
rmarkdown::render(file.path(func_path, 'manuscript_statistics.Rmd'), 
                  output_format = 'html_document',
                  output_dir = results_path)
