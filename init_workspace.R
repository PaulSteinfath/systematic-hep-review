# Dependencies
library(cowplot)
library(dplyr)
library(eegUtils)
library(grid)
library(gridExtra)
library(gridGraphics)
library(ggimage)
library(ggridges)
library(ggplot2)
library(ggtext)
library(magick)
library(PRISMA2020)
library(purrr)
library(pwr)
library(scales)
library(stringr)
library(testit)
library(tidyr)
library(viridis)
library(zeallot)

source_all <- function(folder) {
  for (f in list.files(path = folder, pattern="*.R")) {
    source(file.path(folder, f))
  }
}

# Import all functions and parameters
func_path <- file.path(getwd(), 'functions')

# Load parameters first
config_path <- file.path(func_path, "config")
source_all(config_path)

# Preprocessing and analysis
source(file.path(func_path, 'utils.R'))
source(file.path(func_path, 'preprocess.R'))
source(file.path(func_path, 'validate.R'))
analysis_path <- file.path(func_path, "analysis")
source_all(analysis_path)

# Figures
figures_path <- file.path(func_path, "figures")
plots_path <- file.path(func_path, "plots")
source_all(plots_path)
source_all(figures_path)

