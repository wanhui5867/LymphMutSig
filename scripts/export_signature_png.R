# =============================================================================
# export_signature_png.R
# Convert signature PDF plots in data/SignaturesPlots/ into per-signature
# PNG files saved under www/signatures/<safe_signature_id>.png.
#
# NOTE:
# - Requires the 'magick' package: install.packages('magick')
# - Assumes each PDF contains one page per signature for a given mutation class
# - Assumes PDF filenames contain one of: 'SBS', 'DBS', 'ID', 'Kataegis'
# - Assumes the page order matches the order of signatures in signature_metadata
# =============================================================================

library(magick)
library(dplyr)

root_dir <- "."  # run from project root LymphMutSig/
plots_dir <- file.path(root_dir, "data", "SignaturesPlots")
img_dir   <- file.path(root_dir, "www", "signatures")

if (!dir.exists(plots_dir)) {
  stop("Directory not found: ", plots_dir, "  请先确保 SignaturePlots PDF 已生成到该目录下.")
}
if (!dir.exists(img_dir)) dir.create(img_dir, recursive = TRUE, showWarnings = FALSE)

sig_meta <- readRDS(file.path(root_dir, "data", "signature_metadata.rds"))

# Map PDF filenames to mutation classes (exact filenames)
pdf_map <- list(
  "SBS" = "SBS_96_plots_Sig.pdf",
  "DBS" = "DBS_78_plots_Sig.pdf",
  "ID"  = "ID_83_plots_Sig.pdf",
  "Kataegis" = "katageis_SBS96_plots_Sig.pdf"  # Note: filename uses "katageis"
)

for (mutation_class in names(pdf_map)) {
  pdf_file <- file.path(plots_dir, pdf_map[[mutation_class]])
  if (!file.exists(pdf_file)) {
    warning("PDF file not found: ", pdf_file, ". Skipping ", mutation_class)
    next
  }

  sig_ids <- sig_meta %>%
    filter(.data$mutation_class == .env$mutation_class) %>%
    pull(.data$signature_id)
  if (length(sig_ids) == 0) {
    message("No signatures found in metadata for class ", mutation_class, ". Skip ", pdf_file)
    next
  }

  message("Processing ", pdf_map[[mutation_class]], " as ", mutation_class, " (", length(sig_ids), " signatures)...")
  imgs <- image_read_pdf(pdf_file)
  if (length(imgs) != length(sig_ids)) {
    warning("Number of pages (", length(imgs), ") in ", pdf_map[[mutation_class]],
            " does not match number of signatures (", length(sig_ids), ") for ", mutation_class,
            ". Will map min(pages, signatures).")
  }
  n <- min(length(imgs), length(sig_ids))
  for (i in seq_len(n)) {
    sig_id  <- sig_ids[i]
    safe_id <- gsub("[^A-Za-z0-9]", "_", sig_id)
    out_path <- file.path(img_dir, paste0(safe_id, ".png"))
    image_write(imgs[i], path = out_path, format = "png")
    message("  -> ", sig_id, "  => ", out_path)
  }
}

message("All done. PNG files are in ", img_dir)
