# Bangladeshi Dengue Strain Tracker

**Complete Production-Ready Dashboard**  
Copyright © Md Abrar Faiyaj, 2026 | MIT License

---

## 🚀 QUICK START (3 STEPS)

### Step 1: Install Packages

```r
# Run this ONCE
source("install_packages.R")
```

### Step 2: Process Data

```r
# Run this to generate CSV files
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
bangladesh_dengue_tracker/
├── app.R                      ← Main dashboard
├── analyze_sequences.R        ← Data processor
├── install_packages.R         ← Package installer
├── README.md                  ← This file
├── data/
│   ├── bd_multiple_sequence_raw.fasta  ← Your 13 sequences
│   └── processed/
│       ├── qc_summary.csv              ← Generated
│       ├── motif_matches.csv           ← Generated
│       └── motif_summary.csv           ← Generated
└── plots/                     ← Generated figures
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
  13. PP325839.1  ← Should be here!
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

1. **Nasif et al. (2024)** - MRA 00162-24: 12 Dhaka sequences
2. **Hossain et al. (2025)** - MRA 00023-25: 1 Chittagong sequence  
3. **Hossain et al. (2025)** - HSR e70852: Epidemiological data

---

## ✅ Features Implemented

- [x] All 13 sequences analyzed
- [x] PP325839.1 included
- [x] Professional visualizations
- [x] No overlapping text
- [x] Interactive maps
- [x] Phylogenetic tree
- [x] Epidemiological data
- [x] Geographic mapping
- [x] Temporal trends
- [x] Motif analysis
- [x] Complete data table
- [x] Copyright footer

---

**Ready to publish on GitHub!**  
**Ready to deploy on shinyapps.io!**  
**Ready for your portfolio!**
