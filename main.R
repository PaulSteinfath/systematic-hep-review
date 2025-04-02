library(cowplot)
library(dplyr)
library(ggimage)
library(ggplot2)
library(magick)
library(stringr)
library(tidyr)
library(zeallot)

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
source(file.path(func_path, 'preprocess.R'))
source(file.path(func_path, 'prisma.R'))
source(file.path(func_path, 'validate.R'))
source(file.path(func_path, 'utils.R'))

# Main analysis
# TODO: download the data from OSF / wherever we put it?
df_full <- load_data(pubmed_path, manual_path)
validate_data(df_full)
c(df_screening, df_included) %<-% preprocess(df_full)
p_prisma <- generate_prisma(df_screening, prisma_template_path,
                            derivatives_path, results_path)
make_figures(df_included, results_path)

# Save MD5 sums for tracking changes in the generated figures
df_sums <- get_check_sums(results_path)
write.csv(df_sums, file = file.path("md5sums.csv"), row.names = F)
