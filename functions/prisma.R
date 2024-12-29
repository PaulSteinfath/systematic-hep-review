library(PRISMA2020)     # PRISMA_data, PRISMA_flowdiagram, PRISMA_save


plot_prisma_diagram <- function(df_prisma) {
  prisma_data <- PRISMA_data(df_prisma)
  PRISMA_flowdiagram(prisma_data, previous = F, other = T, 
                     detail_databases = T, fontsize = 14)
}


generate_prisma <- function(df_screening, prisma_path, save_path) {
  # TODO: generate automatically based on the template and the screening data
  df_prisma <- read.csv(prisma_path)
  p_prisma <- plot_prisma_diagram(df_prisma)
  PRISMA_save(
    p_prisma, 
    file.path(save_path, "PRISMA.png"),
    overwrite = T
  )
}