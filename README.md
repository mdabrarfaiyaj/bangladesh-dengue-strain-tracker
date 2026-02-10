# Bangladeshi Dengue Strain Tracker 

**Interactive R Shiny Dashboard** for analyzing dengue virus sequences from the 2023 Bangladesh outbreak

[![R](https://img.shields.io/badge/R-≥4.2-276DC3?style=flat&logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Dashboard-blue?style=flat&logo=shiny&logoColor=white)](https://shiny.posit.co/)
[![Bioconductor](https://img.shields.io/badge/Bioconductor-Used-1A5490?style=flat)](https://bioconductor.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

**Complete Production-Ready Dashboard**  
Copyright © Md Abrar Faiyaj,2026 – Open-source under the MIT License

**🌐 Live Dashboard**

Link : https://u3j9z9-md0abrar-faiyaj.shinyapps.io/bangladesh-denv-tracker/
- - -

## 🚀 Quick Start (3 Steps)

### 1. Install Dependencies (once)

```r
source("install_packages.R")
```

### Step 2: Process Data

```r
#analyze the data
source("analyze_sequences.R")
```

### Step 3: Launch Dashboard

```r
# Launch the app
library(shiny)
runApp("app.R")
```

That's it! Your dashboard should open in a browser.

---

## ✅ What's Included

- **All 13 sequences** (PQ657766.1, PP309840-PP309850, PP325839.1)
- **8 interactive tabs**: Summary, Genomics, Phylogenetics, Epidemiology, Geography, Temporal, Motifs, Data
- **Complete visualizations**: Maps, charts, tables
- **All recommendations implemented**

---

## 📁 Project Structure

```
bangladesh-dengue-strain-tracker/
├── app.R                     # Main Shiny dashboard
├── analyze_sequences.R       # QC, motif analysis & data processing
├── install_packages.R        # Installs CRAN + Bioconductor packages
├── README.md                 # This file
├── LICENSE                   # MIT License
├── data/
│   └── BD Multiple Raw Sequence/
│       └── bd_multiple_sequence_raw.fasta   # 13 BD sequences (gitignored)
└── plots/                    # Generated figures (gitignored)
    └── data/processed/       # CSV outputs (gitignored)
```

---

## 🐛 Troubleshooting

**Error: "data/bd_multiple_sequence_raw.fasta not found"**
- Make sure you're in the correct directory
- Check: `getwd()` should show the project folder
- Fix: `setwd("path/to/bangladesh_dengue_tracker")`

**Error: Package not found**
- Run `source("install_packages.R")` again

**Dashboard shows wrong number of sequences**
- Run `source("analyze_sequences.R")` again
- Check `data/processed/qc_summary.csv` has 13 rows

---

## 📊 Verify Everything Works

After Step 2, check terminal output:
```
✓✓✓ All 13 sequences confirmed! ✓✓✓
  1. PQ657766.1
  2. PP309840.1
  ...
  13. PP325839.1  
```

After Step 3, check dashboard Data Table tab - should show 13 rows.

---

## 🌐 Deploy to shinyapps.io

```r
library(rsconnect)

# One-time setup
rsconnect::setAccountInfo(
  name = "YOUR_USERNAME",
  token = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)

# Deploy
rsconnect::deployApp(
  appName = "bangladesh-dengue-tracker",
  appFiles = c(
    "app.R",
    "data/processed/qc_summary.csv",
    "data/processed/motif_matches.csv",
    "data/processed/motif_summary.csv"
  ),
  forceUpdate = TRUE
)
```

---

## 📚 Data Sources
All sequences are from peer-reviewed publications deposited in NCBI GenBank:
1. **Nasif et al. (2024)** - MRA 00162-24: 12 Dhaka sequences, Link: https://journals.asm.org/doi/10.1128/mra.00162-24
2. **Hossain et al. (2025)** - MRA 00023-25: 1 Chittagong sequence  ,Link: https://journals.asm.org/doi/10.1128/mra.00023-25
3. **Hossain et al. (2025)** - Rest of the Information comes from this paper. Link : https://pubmed.ncbi.nlm.nih.gov/40432700/

---

## ✅ Features Implemented

- [x] All 13 sequences analyzed (PQ657766.1, PP309840-PP309850, PP325839.1)
- [x] Executive Summary
- [x] Professional visualizations
- [x] Genomic Analysis of DENV-2 serotype
- [x] Interactive maps
- [x] Phylogenetic tree
- [x] Epidemiological data
- [x] Geographic mapping
- [x] Temporal trends
- [x] Motif analysis
- [x] Complete data table
- [x] Data Source
- [x] Copyright footer

- - -
## 📚 How to Cite This Work
If you use or reference this dashboard:

Md. Abrar Faiyaj (2026). Bangladeshi Dengue Strain Tracker 2023. GitHub repository: https://github.com/mdabrarfaiyaj/bangladesh-dengue-strain-tracker
- - -
## Open-source under the MIT License
Last updated: 10th February, 2026

