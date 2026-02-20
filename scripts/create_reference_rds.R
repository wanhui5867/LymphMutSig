# =============================================================================
# create_reference_rds.R
# Generate signature_metadata.rds, signature_profiles.rds, exposure_summary.rds
# for LymphMutSig Shiny app. Run once to prepare data from TSV files.
# =============================================================================

library(tidyverse)

# Set paths (run from project root LymphMutSig/)
data_dir <- "data"
out_dir <- "data"  # RDS files saved alongside TSV

# -----------------------------------------------------------------------------
# 1. Signature profiles: list with SBS, DBS, ID, Kataegis (feature matrices)
# -----------------------------------------------------------------------------
df_sbs <- read_tsv(file.path(data_dir, "MutSig.SBS.our.tsv"), show_col_types = FALSE)
df_dbs <- read_tsv(file.path(data_dir, "MutSig.DBS.our.tsv"), show_col_types = FALSE)
df_id <- read_tsv(file.path(data_dir, "MutSig.ID.our.tsv"), show_col_types = FALSE)

# Build list: each element is a matrix (rows = features, cols = signatures)
# First column is feature id (e.g. MutationType)
signature_profiles <- list(
  SBS = as.data.frame(df_sbs),
  DBS = as.data.frame(df_dbs),
  ID  = as.data.frame(df_id),
  Kataegis = as.data.frame(df_sbs[, 1:3])  # placeholder: reuse SBS structure with 2 sigs
)
names(signature_profiles$Kataegis) <- c("MutationType", "K1", "K2")

# -----------------------------------------------------------------------------
# 2. Signature metadata: signature_id, mutation_class, name, putative_etiology, description
#    (from data/signature_metadata.tsv)
# -----------------------------------------------------------------------------
meta_raw <- read_tsv(file.path(data_dir, "signature_metadata.tsv"),
                     show_col_types = FALSE, col_names = FALSE, trim_ws = TRUE)

# Drop header row and assign clean column names
meta <- meta_raw[-1, ]
names(meta) <- c("mutation_class", "signature_id", "putative_etiology",
                 "overall_prevalence", "description")

signature_metadata <- meta %>%
  mutate(
    mutation_class = as.character(mutation_class),
    signature_id = as.character(signature_id),
    name = signature_id
  ) %>%
  select(signature_id, mutation_class, name, putative_etiology, description)

# -----------------------------------------------------------------------------
# 3. Exposure summary: signature_id, entity, mean_exposure, proportion_positive, attribution_score
#    (from data/exposure_summary.tsv)
# -----------------------------------------------------------------------------
exp_raw <- read_tsv(file.path(data_dir, "exposure_summary.tsv"), show_col_types = FALSE)

exposure_summary <- exp_raw %>%
  mutate(
    signature_short = Signature,
    signature_id = dplyr::case_when(
      grepl("^S", signature_short) ~ paste0("Sig S", sub("^S", "", signature_short)),
      grepl("^D", signature_short) ~ paste0("Sig D", sub("^D", "", signature_short)),
      grepl("^ID", signature_short) ~ paste0("Sig ID", sub("^ID", "", signature_short)),
      grepl("^K", signature_short) ~ paste0("Sig K", sub("^K", "", signature_short)),
      TRUE ~ as.character(Signature)
    ),
    entity = Subtype,
    proportion_positive = proportion / 100,
    mean_exposure = exposure,
    attribution_score = exposure
  ) %>%
  select(signature_id, entity, mean_exposure, proportion_positive, attribution_score)

# -----------------------------------------------------------------------------
# Save RDS
# -----------------------------------------------------------------------------
saveRDS(signature_metadata, file.path(out_dir, "signature_metadata.rds"))
saveRDS(signature_profiles, file.path(out_dir, "signature_profiles.rds"))
saveRDS(exposure_summary, file.path(out_dir, "exposure_summary.rds"))

message("Created signature_metadata.rds, signature_profiles.rds, exposure_summary.rds in ", out_dir)
