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

# Load parameters first
config_path <- file.path(getwd(), "config")
for (f in list.files(path = config_path, pattern="*.R")) {
  source(file.path(config_path, f))
}

# Import all functions
func_path <- file.path(getwd(), 'functions')
source(file.path(func_path, 'figures.R'))
source(file.path(func_path, 'preprocess.R'))
source(file.path(func_path, 'prisma.R'))
source(file.path(func_path, 'validate.R'))
