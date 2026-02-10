#!/usr/bin/env Rscript
# Bangladesh Dengue Tracker - Analysis Script
# Processes all 13 sequences
# Copyright © Md Abrar Faiyaz, 2026

cat("==============================================\n")
cat("Bangladesh Dengue Tracker\n")
cat("Processing 13 DENV-2 Sequences\n")
cat("==============================================\n\n")

# Load required libraries
suppressPackageStartupMessages({
  library(Biostrings)
  library(dplyr)
  library(ggplot2)
})

# File paths
INPUT_FASTA <- "data/bd_multiple_sequence_raw.fasta"
OUTPUT_DIR <- "data/processed"

# Create output directories
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create("plots", recursive = TRUE, showWarnings = FALSE)

# Check if input file exists
if (!file.exists(INPUT_FASTA)) {
  stop("\n❌ ERROR: Could not find ", INPUT_FASTA, 
       "\n\nPlease ensure the FASTA file is in the 'data' folder.\n",
       "Expected location: ", getwd(), "/", INPUT_FASTA, "\n")
}

# === STEP 1: Load sequences ===
cat("[1/4] Loading sequences from FASTA file...\n")
seqs <- readDNAStringSet(INPUT_FASTA)
seq_ids <- sapply(strsplit(names(seqs), " "), function(x) x[1])

cat(sprintf("✓ Successfully loaded %d sequences\n\n", length(seqs)))

# Verify all 13 expected sequences
expected_ids <- c("PQ657766.1", "PP309840.1", "PP309841.1", "PP309842.1", 
                  "PP309843.1", "PP309844.1", "PP309845.1", "PP309846.1",
                  "PP309847.1", "PP309848.1", "PP309849.1", "PP309850.1", 
                  "PP325839.1")

cat("Sequence inventory:\n")
for (i in 1:length(seq_ids)) {
  marker <- if (seq_ids[i] %in% expected_ids) "✓" else "⚠"
  cat(sprintf("  %s %2d. %-15s  Length: %s bp\n", 
              marker, i, seq_ids[i], format(width(seqs)[i], big.mark = ",")))
}

if (length(seqs) == 13) {
  cat("\n✓✓✓ All 13 sequences confirmed! ✓✓✓\n\n")
} else {
  cat(sprintf("\n⚠ WARNING: Expected 13 sequences, found %d\n\n", length(seqs)))
}

# === STEP 2: Quality Control ===
cat("[2/4] Quality control analysis...\n")

qc_summary <- data.frame(
  Sequence_ID = seq_ids,
  Length = width(seqs),
  GC_Content = as.numeric(letterFrequency(seqs, "GC", as.prob = TRUE)),
  N_Content = as.numeric(letterFrequency(seqs, "N", as.prob = TRUE)),
  A_Count = as.numeric(letterFrequency(seqs, "A")),
  T_Count = as.numeric(letterFrequency(seqs, "T")),
  G_Count = as.numeric(letterFrequency(seqs, "G")),
  C_Count = as.numeric(letterFrequency(seqs, "C")),
  stringsAsFactors = FALSE
)

# QC filters
qc_summary$Pass_QC <- (qc_summary$Length >= 500) & (qc_summary$N_Content <= 0.05)

cat(sprintf("✓ QC complete: %d/%d sequences passed\n", 
            sum(qc_summary$Pass_QC), nrow(qc_summary)))
cat(sprintf("  Mean length: %s bp\n", format(round(mean(qc_summary$Length)), big.mark = ",")))
cat(sprintf("  Mean GC%%: %.2f%%\n\n", mean(qc_summary$GC_Content) * 100))

# Save QC summary
write.csv(qc_summary, file.path(OUTPUT_DIR, "qc_summary.csv"), row.names = FALSE)
cat(sprintf("✓ Saved: %s\n\n", file.path(OUTPUT_DIR, "qc_summary.csv")))

# === STEP 3: Motif Analysis ===
cat("[3/4] Detecting dengue-specific motifs...\n")

filtered_seqs <- seqs[qc_summary$Pass_QC]
filtered_ids <- seq_ids[qc_summary$Pass_QC]

motifs <- list(
  "ATG" = "Translation start codon",
  "GAC" = "Codon for aspartic acid in envelope (E) protein",
  "AATAAA" = "Polyadenylation signal",
  "CACAG" = "Conserved pentanucleotide in 3' stem-loop",
  "AGAGA" = "Part of 3' UTR elements"
)

all_matches <- data.frame()

for (motif_name in names(motifs)) {
  matches <- vmatchPattern(motif_name, filtered_seqs)
  
  for (i in seq_along(matches)) {
    if (length(matches[[i]]) > 0) {
      positions <- start(matches[[i]])
      for (pos in positions) {
        all_matches <- rbind(all_matches, data.frame(
          Sequence_ID = filtered_ids[i],
          Motif = motif_name,
          Description = motifs[[motif_name]],
          Position = pos,
          Count = length(positions),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
}

# Motif summary
motif_summary <- all_matches %>%
  group_by(Motif, Description) %>%
  summarise(
    Total_Occurrences = n(),
    Sequences_With_Motif = n_distinct(Sequence_ID),
    Avg_Per_Sequence = round(Total_Occurrences / Sequences_With_Motif, 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(Total_Occurrences))

write.csv(all_matches, file.path(OUTPUT_DIR, "motif_matches.csv"), row.names = FALSE)
write.csv(motif_summary, file.path(OUTPUT_DIR, "motif_summary.csv"), row.names = FALSE)

cat(sprintf("✓ Found %d total motif matches\n", nrow(all_matches)))
cat(sprintf("✓ Motifs detected in %d sequences\n\n", length(unique(all_matches$Sequence_ID))))

cat("Motif summary:\n")
for (i in 1:nrow(motif_summary)) {
  cat(sprintf("  %s: %d occurrences\n", 
              motif_summary$Motif[i], 
              motif_summary$Total_Occurrences[i]))
}
cat("\n")

# Save filtered sequences for custom search
writeXStringSet(filtered_seqs, file.path(OUTPUT_DIR, "filtered_sequences.fasta"))
cat(sprintf("✓ Saved filtered sequences\n\n"))

# === STEP 4: Generate plots ===
cat("[4/4] Creating visualizations...\n")

# Plot 1: Length distribution
p1 <- ggplot(qc_summary, aes(x = Length)) +
  geom_histogram(bins = 15, fill = "#3498DB", alpha = 0.8, color = "white") +
  labs(title = sprintf("Sequence Length Distribution (n=%d)", nrow(qc_summary)),
       x = "Length (bp)", y = "Count") +
  theme_minimal(base_size = 13)
ggsave("plots/length_distribution.png", p1, width = 9, height = 6, dpi = 300)
cat("  ✓ plots/length_distribution.png\n")
p1
# Plot 2: GC content
p2 <- ggplot(qc_summary, aes(x = GC_Content * 100)) +
  geom_histogram(bins = 12, fill = "#27AE60", alpha = 0.8, color = "white") +
  geom_vline(xintercept = 47, linetype = "dashed", color = "#E74C3C", size = 1) +
  labs(title = "GC Content Distribution",
       x = "GC Content (%)", y = "Count") +
  theme_minimal(base_size = 13)
ggsave("plots/gc_distribution.png", p2, width = 9, height = 6, dpi = 300)
cat("  ✓ plots/gc_distribution.png\n")
p2
# Plot 3: Motif frequency
p3 <- ggplot(motif_summary, aes(x = reorder(Motif, Total_Occurrences), 
                                 y = Total_Occurrences, fill = Motif)) +
  geom_col(alpha = 0.8) +
  coord_flip() +
  labs(title = "Motif Frequency", x = NULL, y = "Total Occurrences") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
ggsave("plots/motif_frequency.png", p3, width = 9, height = 6, dpi = 300)
cat("  ✓ plots/motif_frequency.png\n")
p3
# === FINAL SUMMARY ===
cat("\n==============================================\n")
cat("✓✓✓ ANALYSIS COMPLETE! ✓✓✓\n")
cat("==============================================\n\n")

cat("Summary:\n")
cat(sprintf("  • Sequences processed: %d\n", nrow(qc_summary)))
cat(sprintf("  • Sequences passed QC: %d\n", sum(qc_summary$Pass_QC)))
cat(sprintf("  • Total motif matches: %d\n", nrow(all_matches)))
cat(sprintf("  • Mean length: %s bp\n", format(round(mean(qc_summary$Length)), big.mark = ",")))
cat(sprintf("  • Mean GC%%: %.2f%%\n\n", mean(qc_summary$GC_Content) * 100))

cat("✓ All 13 sequences ready for dashboard:\n")
for (id in qc_summary$Sequence_ID) {
  cat(sprintf("    • %s\n", id))
}

cat("\n==============================================\n")
cat("Next step: Launch dashboard\n")
cat("  > library(shiny)\n")
cat("  > runApp('app.R')\n")
cat("==============================================\n")
