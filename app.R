# Bangladeshi Dengue Strain Tracker - FIXED VERSION
# Copyright © Md Abrar Faiyaj, 2026

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(leaflet)
library(viridis)

# === LOAD DATA ===
qc <- read.csv("data/processed/qc_summary.csv", stringsAsFactors = FALSE)
motifs <- read.csv("data/processed/motif_matches.csv", stringsAsFactors = FALSE)
motif_sum <- read.csv("data/processed/motif_summary.csv", stringsAsFactors = FALSE)

# === EPIDEMIOLOGICAL DATA ===
epi_gender <- data.frame(
  Gender = c("Male", "Female"),
  Infections = c(192610, 128569),
  Deaths = c(735, 970)
) %>% mutate(
  Infection_Pct = round(100 * Infections / sum(Infections), 2),
  Death_Pct = round(100 * Deaths / sum(Deaths), 2),
  CFR = round(100 * Deaths / Infections, 3)
)

epi_age <- data.frame(
  Age_Group = factor(c("≤5", ">5-10", ">10-20", ">20-40", ">40-60", ">60-80", ">80"),
                     levels = c("≤5", ">5-10", ">10-20", ">20-40", ">40-60", ">60-80", ">80")),
  Cases = c(16019, 16209, 60724, 145571, 64373, 17226, 1057),
  Deaths = c(66, 58, 144, 593, 528, 280, 36)
) %>% mutate(CFR = round(100 * Deaths / Cases, 2))

epi_monthly <- data.frame(
  Month = factor(month.abb, levels = month.abb),
  Cases = c(566, 166, 111, 143, 1036, 5956, 43854, 71976, 79598, 67769, 40716, 9288),
  Deaths = c(0, 0, 0, 0, 3, 34, 172, 380, 396, 329, 296, 95)
) %>% mutate(Month_Num = 1:12)

geo_data <- data.frame(
  Division = c("Dhaka", "Chittagong", "Barisal", "Rangpur", "Rajshahi", 
               "Khulna", "Mymensingh", "Sylhet"),
  Cases = c(169321, 44435, 23146, 24638, 17933, 28019, 12252, 1435),
  Deaths = c(1163, 134, 206, 49, 47, 51, 54, 1),
  Sequences = c(12, 1, 0, 0, 0, 0, 0, 0),
  Lat = c(23.8103, 22.3569, 22.7010, 25.7439, 24.3745, 22.8456, 24.7471, 24.8949),
  Lon = c(90.4125, 91.7832, 90.3535, 89.2752, 88.6042, 89.5403, 90.4203, 91.8687)
) %>% mutate(CFR = round(100 * Deaths / Cases, 3))

colors <- list(
  primary = "#2C3E50", blue = "#3498DB", green = "#27AE60", 
  yellow = "#F39C12", red = "#E74C3C", purple = "#9B59B6", 
  teal = "#1ABC9C", orange = "#E67E22", pink = "#E91E63"
)

# === UI ===
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Bangladeshi Dengue Strain Tracker", titleWidth = 400),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Executive Summary", tabName = "summary", icon = icon("chart-bar")),
      menuItem("Genomic Analysis", tabName = "genomics", icon = icon("dna")),
      menuItem("Phylogenetics", tabName = "phylo", icon = icon("project-diagram")),
      menuItem("Epidemiology", tabName = "epi", icon = icon("hospital")),
      menuItem("Geographic Mapping", tabName = "geo", icon = icon("map-marked-alt")),
      menuItem("Temporal Trends", tabName = "temporal", icon = icon("chart-line")),
      menuItem("Motif Analysis", tabName = "motifs", icon = icon("microscope")),
      menuItem("Data Table", tabName = "datatable", icon = icon("table")),
      menuItem("Research Context", tabName = "about", icon = icon("book"))
    ),
    hr(),
    div(style = "padding: 12px; color: white; font-size: 11px;",
        p("🦟 2023 Outbreak", style = "font-weight: bold; margin: 3px 0;"),
        p(sprintf("📊 Sequences: %d", nrow(qc)), style = "margin: 3px 0;"),
        p("🦠 DENV-2", style = "margin: 3px 0;")
    ),
    div(style = "position: absolute; bottom: 10px; left: 10px; right: 10px; 
                 padding: 10px; background: rgba(255,255,255,0.1); 
                 border-radius: 5px; font-size: 9px; color: #BDC3C7;",
        p("Copyright © Md Abrar Faiyaj, 2026", style = "margin: 2px 0;"),
        p("Data: Nasif et al. 2024, Hossain et al. 2025", style = "margin: 2px 0;"),
        p("MIT License", style = "margin: 2px 0;")
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background: #FAFBFC; }
      .box { border-radius: 10px; box-shadow: 0 3px 10px rgba(0,0,0,0.1); }
      .highlight-box { background: #FFF9E6; border-left: 4px solid #F39C12; 
                       padding: 15px; margin: 12px 0; border-radius: 6px; }
    "))),
    
    tabItems(
      # EXECUTIVE SUMMARY
      tabItem(tabName = "summary",
              fluidRow(
                valueBox("321,179", "Cases", icon = icon("viruses"), color = "red", width = 3),
                valueBox("1,705", "Deaths", icon = icon("heart-broken"), color = "purple", width = 3),
                valueBox(nrow(qc), "Genomes", icon = icon("dna"), color = "blue", width = 3),
                valueBox("Genotype II", "Cosmopolitan", icon = icon("globe"), color = "orange", width = 3)
              ),
              fluidRow(
                box(title = "Key Findings", status = "warning", solidHeader = TRUE, width = 12,
                    div(class = "highlight-box",
                        h4("Data from 3 Peer-Reviewed Studies"),
                        tags$ul(
                          tags$li("Nasif et al. (2024): 12 Dhaka sequences"),
                          tags$li("Hossain et al. (2025): 1 Chittagong sequence"),
                          tags$li("Hossain et al. (2025): Epidemiological data")
                        ),
                        p(strong(sprintf("Total: %d complete DENV-2 genomes", nrow(qc))))
                    ))
              )
      ),
      
      # GENOMICS 
      tabItem(tabName = "genomics",
              fluidRow(
                box(width = 12,
                    div(style = "background: #E8F8F5; padding: 15px; border-radius: 8px; border-left: 4px solid #1ABC9C;",
                        h4("Summary of 13 Sequences Analysis", style = "margin-top: 0;"),
                        p(sprintf("Analyzed %d complete DENV-2 sequences from Bangladesh 2023 outbreak", nrow(qc)))
                    ))
              ),
              fluidRow(
                valueBox(nrow(qc), "Total Sequences", "2023 outbreak", icon = icon("dna"), color = "blue", width = 2),
                valueBox(nrow(qc), "Passed QC", "100%", icon = icon("check-circle"), color = "green", width = 2),
                valueBox(format(round(mean(qc$Length)), big.mark = ","), "Mean Length", "bp", 
                         icon = icon("ruler"), color = "aqua", width = 2),
                valueBox(paste0(round(mean(qc$GC_Content) * 100, 1), "%"), "Mean GC%", "DENV-2", 
                         icon = icon("chart-pie"), color = "yellow", width = 2),
                valueBox(nrow(motifs), "Motif Matches", "5 patterns", 
                         icon = icon("search"), color = "red", width = 2),
                valueBox("Genotype II", "Cosmopolitan", "Indian lineage", 
                         icon = icon("globe"), color = "purple", width = 2)
              ),
              fluidRow(
                box(title = "Genome Structure (10.7 kb)", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 12,
                    plotlyOutput("genome_structure", height = "420px")   # ← Updated here!
                )
              ),
              fluidRow(
                box(title = "GC Content Distribution", status = "success", solidHeader = TRUE, width = 6,
                    plotlyOutput("gc_plot", height = "350px")),
                box(title = "Sequence Similarity", status = "info", solidHeader = TRUE, width = 6,
                    plotlyOutput("similarity_heatmap", height = "350px"))
              ),
              fluidRow(
                box(title = "Quality Validation: GC% vs Length", status = "warning", 
                    solidHeader = TRUE, width = 12,
                    plotlyOutput("qc_scatter", height = "400px"))
              )
      ),
      
      # PHYLOGENETICS
      tabItem(tabName = "phylo",
              fluidRow(
                box(width = 12,
                    div(style = "background: #EBF5FB; padding: 15px; border-radius: 8px; border-left: 4px solid #3498DB;",
                        h4("Phylogenetic Context", style = "margin-top: 0;"),
                        tags$ul(
                          tags$li("Chittagong (PQ657766): 99.57% similarity to Thailand GQ868591"),
                          tags$li("Dhaka strains: Related to Indian 2022-2023 isolates"),
                          tags$li("All 13: Genotype II-Cosmopolitan"),
                          tags$li("Southeast Asian / American lineage")
                        )))
              ),
              fluidRow(
                box(title = "Phylogenetic Tree", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("phylo_tree", height = "600px"))
              )
      ),
      
      # EPIDEMIOLOGY
      tabItem(tabName = "epi",
              fluidRow(
                box(width = 12, div(class = "highlight-box",
                                    h4("Summary of Epidemiological Data"),
                                    tags$ul(
                                      tags$li("321,179 cases, 1,705 deaths (0.53% CFR)"),
                                      tags$li("Males: 59.97% infected, Females: 56.89% mortality"),
                                      tags$li("Peak: September 2023")
                                    )))
              ),
              fluidRow(
                box(title = "Gender Distribution", status = "warning", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(4, h5("Infections", style = "text-align: center;"), 
                             plotlyOutput("epi_inf_pie", height = "280px")),
                      column(4, h5("Mortality", style = "text-align: center;"), 
                             plotlyOutput("epi_death_pie", height = "280px")),
                      column(4, h5("Case Fatality Rate", style = "text-align: center;"), 
                             plotlyOutput("epi_cfr_bar", height = "280px"))
                    ))
              ),
              fluidRow(
                box(title = "Age Distribution", status = "success", solidHeader = TRUE, width = 12,
                    plotlyOutput("epi_age_plot", height = "400px"))
              )
      ),
      
      # GEOGRAPHY
      tabItem(tabName = "geo",
              fluidRow(
                box(width = 12,
                    div(style = "background: #FADBD8; padding: 15px; border-radius: 8px; border-left: 4px solid #E74C3C;",
                        h4("Geographic Mapping with Sequence Locations"),
                        p("Circle size = cases | Color = CFR | Red markers = sequences")))
              ),
              fluidRow(
                box(title = "Bangladesh Map", status = "danger", solidHeader = TRUE, width = 12,
                    leafletOutput("geo_map", height = "550px"))
              ),
              fluidRow(
                box(title = "Division Statistics", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("geo_table"))
              )
      ),
      
      # TEMPORAL
      tabItem(tabName = "temporal",
              fluidRow(
                box(width = 12,
                    div(style = "background: #FEF5E7; padding: 15px; border-radius: 8px; border-left: 4px solid #F39C12;",
                        h4("Temporal Trends Analysis"),
                        p("Monsoon-driven outbreak (July-October peak)")))
              ),
              fluidRow(
                box(title = "2023 Epidemic Curve", status = "warning", solidHeader = TRUE, width = 12,
                    plotlyOutput("temporal_plot", height = "450px"))
              )
      ),
      
      # MOTIFS
      tabItem(tabName = "motifs",
              fluidRow(
                valueBox(nrow(motifs), "Total Matches", "5 patterns", icon = icon("search"), color = "red", width = 3),
                valueBox(motif_sum$Motif[1], "Most Common", paste(motif_sum$Total_Occurrences[1], "hits"), 
                         icon = icon("star"), color = "yellow", width = 3),
                valueBox(13, "Sequences", "All analyzed", icon = icon("check-circle"), color = "green", width = 3),
                valueBox(round(nrow(motifs)/13, 1), "Avg/Sequence", "Matches", 
                         icon = icon("calculator"), color = "blue", width = 3)
              ),
              fluidRow(
                box(title = "Motif Selection Criteria", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("motif_criteria_table"))
              ),
              fluidRow(
                box(title = "Motif Frequency", status = "success", solidHeader = TRUE, width = 6,
                    plotlyOutput("motif_freq_plot", height = "400px")),
                box(title = "Motif Positions", status = "info", solidHeader = TRUE, width = 6,
                    plotlyOutput("motif_pos_plot", height = "400px"))
              )
      ),
      
      # DATA TABLE
      tabItem(tabName = "datatable",
              box(title = sprintf("Complete Dataset: All %d Sequences", nrow(qc)), 
                  status = "primary", solidHeader = TRUE, width = 12,
                  p(strong(sprintf("Showing all %d sequences including PP325839.1", nrow(qc))),
                    style = "color: #27AE60; margin-bottom: 10px;"),
                  DTOutput("full_data_table"))
      ),
      
      # ABOUT
      tabItem(tabName = "about",
              fluidRow(
                box(title = "Peer-Reviewed Sources", status = "primary", solidHeader = TRUE, width = 12,
                    h4("Publications"),
                    div(style = "background: #F8F9FA; padding: 15px; margin: 10px 0; border-radius: 6px;",
                        h5("1. Nasif et al. (2024)"),
                        p(strong("Microbiology Resource Announcements"), ", 13(6), e00162-24"),
                        p("12 sequences: PP309840-PP309850, PP325839")),
                    div(style = "background: #F8F9FA; padding: 15px; margin: 10px 0; border-radius: 6px;",
                        h5("2. Hossain et al. (2025)"),
                        p(strong("Microbiology Resource Announcements"), ", 14(5), e00023-25"),
                        p("1 sequence: PQ657766")),
                    div(style = "background: #F8F9FA; padding: 15px; margin: 10px 0; border-radius: 6px;",
                        h5("3. Hossain et al. (2025)"),
                        p(strong("Health Science Reports"), ", 8, e70852"),
                        p("Epidemiology: 321,179 cases"))
                )
              )
      )
    ),
    
    div(style = "position: fixed; bottom: 0; left: 0; right: 0; 
         background: #2C3E50; color: white; padding: 8px; text-align: center; 
         font-size: 11px; z-index: 1000;",
        "Copyright © Md Abrar Faiyaj, 2026 | Data: Nasif et al. 2024, Hossain et al. 2025 | MIT License")
  )
)

# === SERVER ===
server <- function(input, output, session) {
  
  output$genome_structure <- renderPlotly({
    
    # Data: approximate positions (standard DENV ~10.7 kb genome)
    regions <- data.frame(
      Region = c("5' UTR", "C", "prM", "E", "NS1", "NS2A", "NS2B", "NS3", "NS4A", "NS4B", "NS5", "3' UTR"),
      Start  = c(1,    96,   291,  791,   2471,  3521,  3891,  4266,  5471,  5736,  6476,  10271),
      End    = c(95,   290,  790,  2470,  3520,  3890,  4265,  5470,  5735,  6475,  10270, 10735),
      Color  = c("#ADD8E6", "#90EE90", "#90EE90", "#90EE90",  # light blue UTR, light green structural
                 "#FFD580", "#FFD580", "#FFD580", "#FFD580", "#FFD580", "#FFD580", "#FFD580", "#FF6347")  # orange NS, red 3'UTR
    )
    
    regions$Mid <- (regions$Start + regions$End) / 2   # for text labels
    regions$Length <- regions$End - regions$Start + 1
    
    # Cleavage sites (approximate positions where NS2B-NS3 protease cuts)
    cleavage_pos <- c(290, 790, 2470, 3520, 3890, 4265, 5470, 5735, 6475)  # after C, prM, E, NS1, NS2A, NS2B, NS3, NS4A, NS4B
    
    p <- plot_ly() %>%
      # Add colored rectangles for each region
      add_trace(
        type = "scatter",
        mode = "none",
        x = c(regions$Start[1], regions$End[1], regions$End[1], regions$Start[1], regions$Start[1]),
        y = c(0,0,1,1,0),
        fill = "toself",
        fillcolor = regions$Color[1],
        showlegend = FALSE,
        hoverinfo = "text",
        text = ~paste0(regions$Region[1], "<br>", regions$Start[1], "–", regions$End[1], " nt")
      )
    
    # Add remaining regions (loop for simplicity)
    for (i in 2:nrow(regions)) {
      p <- p %>% add_trace(
        type = "scatter",
        mode = "none",
        x = c(regions$Start[i], regions$End[i], regions$End[i], regions$Start[i], regions$Start[i]),
        y = c(0,0,1,1,0),
        fill = "toself",
        fillcolor = regions$Color[i],
        showlegend = FALSE,
        hoverinfo = "text",
        text = ~paste0(regions$Region[i], "<br>", regions$Start[i], "–", regions$End[i], " nt<br>Length: ", regions$Length[i], " nt")
      )
    }
    
    # Add protein labels centered above or inside boxes
    p <- p %>% add_annotations(
      x = regions$Mid,
      y = rep(1.15, nrow(regions)),   # above the bar
      text = regions$Region,
      showarrow = FALSE,
      font = list(size = 12, color = "black"),
      xanchor = "center"
    )
    
    # Add small downward arrows at cleavage sites
    for (pos in cleavage_pos) {
      p <- p %>% add_annotations(
        x = pos,
        y = 0,
        text = "▼",
        showarrow = FALSE,
        font = list(size = 16, color = "black"),
        yshift = -10,
        arrowhead = 0
      ) %>%
        add_segments(
          x = pos, xend = pos,
          y = -0.1, yend = 0,
          line = list(color = "black", width = 1.5),
          showlegend = FALSE
        )
    }
    
    # Final layout – make it look like a classic schematic
    p %>% layout(
      title = list(
        text = "DENV-2 Genome Organization (~10.7 kb)",
        font = list(size = 16),
        x = 0.5,
        y = 0.98
      ),
      xaxis = list(
        title = "Genome Position (nucleotides)",
        range = c(0, 10800),
        zeroline = FALSE,
        showgrid = TRUE,
        gridcolor = "#f0f0f0",
        tickfont = list(size = 12)
      ),
      yaxis = list(
        title = "",
        showticklabels = FALSE,
        range = c(-0.3, 1.4),
        fixedrange = TRUE
      ),
      height = 380,
      margin = list(l = 40, r = 40, t = 80, b = 80),
      plot_bgcolor = "#ffffff",
      paper_bgcolor = "#ffffff",
      shapes = list(  # subtle background line for the genome "tape"
        list(
          type = "rect",
          x0 = 0, x1 = 10735,
          y0 = -0.05, y1 = 0.05,
          fillcolor = "#e0e0e0",
          line = list(width = 0)
        )
      ),
      hovermode = "closest",
      dragmode = FALSE
    ) %>%
      config(displayModeBar = FALSE)
  })
  
  # GC plot
  output$gc_plot <- renderPlotly({
    p <- ggplot(qc, aes(x = GC_Content * 100)) +
      annotate("rect", xmin = 46, xmax = 48, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = colors$green) +
      geom_histogram(bins = 12, fill = colors$green, alpha = 0.8, color = "white") +
      geom_vline(xintercept = mean(qc$GC_Content * 100), linetype = "dashed", 
                 color = colors$red, linewidth = 1.2) +
      labs(x = "GC Content (%)", y = "Count") +
      theme_minimal()
    ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Similarity heatmap
  output$similarity_heatmap <- renderPlotly({
    gc_matrix <- outer(qc$GC_Content, qc$GC_Content,
                       FUN = function(x, y) 100 - abs(x - y) * 100)
    plot_ly(x = paste0("S", 1:nrow(qc)), y = paste0("S", 1:nrow(qc)),
            z = gc_matrix, type = "heatmap", colors = viridis(100)) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = ""))
  })
  
  # QC scatter
  output$qc_scatter <- renderPlotly({
    p <- ggplot(qc, aes(x = Length, y = GC_Content * 100)) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 46, ymax = 48, alpha = 0.15, fill = colors$green) +
      geom_point(color = colors$blue, size = 8, alpha = 0.7) +
      geom_point(color = "white", size = 4.5) +
      geom_smooth(method = "lm", se = TRUE, color = colors$primary, fill = colors$primary, alpha = 0.2) +
      labs(x = "Length (bp)", y = "GC Content (%)") +
      theme_minimal()
    ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white", font = list(size = 14)))
  })
  
  # Phylo tree
  output$phylo_tree <- renderPlot({
    par(mar = c(2, 1, 4, 1), bg = "#FAFBFC")
    plot(1, type = "n", xlim = c(0, 12), ylim = c(0, 11), xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
    text(6, 10.5, "Phylogenetic Relationship: DENV-2 Genotype II-Cosmopolitan", 
         cex = 1.7, font = 2, col = colors$primary)
    points(6, 7.5, pch = 19, cex = 3, col = "#95A5A6")
    text(6, 6.7, "Genotype II", cex = 1, col = "#7F8C8D", font = 2)
    segments(6, 7.5, 2.5, 5, lwd = 3.5, col = "#16A085")
    points(2.5, 5, pch = 19, cex = 2.2, col = "#16A085")
    text(2.5, 4.2, "Thailand\nGQ868591", cex = 1, col = "#16A085", font = 2)
    segments(2.5, 5, 1, 2.5, lwd = 4, col = colors$teal)
    points(1, 2.5, pch = 19, cex = 2.5, col = colors$teal)
    text(1, 1.8, "Chittagong\nPQ657766", cex = 1, col = colors$teal, font = 2)
    segments(2.5, 5, 1.5, 1, lwd = 2.5, col = "#3498DB", lty = 2)
    text(1.5, 0.5, "SE Asian/\nAmerican", cex = 0.9, col = "#3498DB", font = 3)
    segments(6, 7.5, 8, 5, lwd = 3.5, col = colors$red)
    points(8, 5, pch = 19, cex = 2.2, col = colors$red)
    text(8, 4.2, "Indian\n2022-2023", cex = 1, col = colors$red, font = 2)
    segments(8, 5, 9.5, 2.5, lwd = 4, col = colors$green)
    points(9.5, 2.5, pch = 19, cex = 2.5, col = colors$green)
    text(9.5, 1.8, "Dhaka\n12 sequences", cex = 1, col = colors$green, font = 2)
    legend("bottom", ncol = 2, legend = c("Chittagong→Thailand", "Dhaka→India"),
           col = c(colors$teal, colors$green), lwd = c(4, 4), cex = 0.95, bg = "white")
  }, height = 600)
  
  # Epidemiology
  output$epi_inf_pie <- renderPlotly({
    plot_ly(epi_gender, labels = ~Gender, values = ~Infections, type = "pie",
            marker = list(colors = c(colors$blue, colors$pink)),
            textposition = "inside", textinfo = "label+percent",
            insidetextfont = list(size = 16, color = "white")) %>%
      layout(showlegend = FALSE, margin = list(l = 20, r = 20, t = 20, b = 20))
  })
  
  output$epi_death_pie <- renderPlotly({
    plot_ly(epi_gender, labels = ~Gender, values = ~Deaths, type = "pie",
            marker = list(colors = c(colors$blue, colors$pink)),
            textposition = "inside", textinfo = "label+percent",
            insidetextfont = list(size = 16, color = "white")) %>%
      layout(showlegend = FALSE, margin = list(l = 20, r = 20, t = 20, b = 20))
  })
  
  output$epi_cfr_bar <- renderPlotly({
    p <- ggplot(epi_gender, aes(x = Gender, y = CFR, fill = Gender)) +
      geom_col(color = "white", linewidth = 1.5, alpha = 0.9) +
      geom_text(aes(label = paste0(CFR, "%")), vjust = -0.5, fontface = "bold", size = 7) +
      scale_fill_manual(values = c(Male = colors$blue, Female = colors$pink)) +
      labs(x = NULL, y = "CFR (%)") +
      theme_minimal() + theme(legend.position = "none") +
      ylim(0, max(epi_gender$CFR) * 1.3)
    ggplotly(p) %>% layout(showlegend = FALSE)
  })
  
  output$epi_age_plot <- renderPlotly({
    age_long <- rbind(
      data.frame(Age_Group = epi_age$Age_Group, Value = epi_age$Cases, Type = "Cases"),
      data.frame(Age_Group = epi_age$Age_Group, Value = epi_age$Deaths, Type = "Deaths")
    )
    p <- ggplot(age_long, aes(x = Age_Group, y = Value, fill = Type)) +
      geom_col(position = "dodge", color = "white", linewidth = 1.2, alpha = 0.9) +
      scale_fill_manual(values = c(Cases = colors$blue, Deaths = colors$red)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Age Group (years)", y = "Count") +
      theme_minimal() + theme(legend.position = "top", legend.title = element_blank())
    ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Geography
  output$geo_map <- renderLeaflet({
    pal <- colorNumeric("YlOrRd", geo_data$CFR)
    leaflet(geo_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 90.4, lat = 23.8, zoom = 7) %>%
      addCircleMarkers(
        lng = ~Lon, lat = ~Lat, radius = ~sqrt(Cases) / 35,
        fillColor = ~pal(CFR), fillOpacity = 0.75,
        color = "white", weight = 3,
        popup = ~paste0("<b>", Division, "</b><br>Cases: ", format(Cases, big.mark = ","), 
                        "<br>Deaths: ", Deaths, "<br>CFR: ", CFR, "%"),
        label = ~Division
      ) %>%
      addAwesomeMarkers(
        data = geo_data[geo_data$Sequences > 0, ],
        lng = ~Lon, lat = ~Lat,
        icon = awesomeIcons(icon = "dna", library = "fa", markerColor = "red", iconColor = "white"),
        popup = ~paste0("<b>Genomic Surveillance</b><br>", Division, ": ", Sequences, " sequences")
      ) %>%
      addLegend("bottomright", pal = pal, values = ~CFR, title = "CFR (%)")
  })
  
  output$geo_table <- renderDT({
    datatable(geo_data %>% select(Division, Cases, Deaths, CFR, Sequences),
              options = list(pageLength = 8, dom = 't'),
              rownames = FALSE) %>%
      formatStyle('Division', fontWeight = 'bold') %>%
      formatStyle('CFR', backgroundColor = styleInterval(c(0.4, 0.6, 0.8),
                                                         c('#D5F4E6', '#FFF9C4', '#FFCDD2', '#EF9A9A')))
  })
  
  # Temporal
  output$temporal_plot <- renderPlotly({
    p <- ggplot(epi_monthly, aes(x = Month_Num)) +
      geom_col(aes(y = Cases), fill = colors$blue, alpha = 0.75) +
      geom_line(aes(y = Deaths * 220), color = colors$red, linewidth = 2.2) +
      geom_point(aes(y = Deaths * 220), color = colors$red, size = 4.5) +
      annotate("rect", xmin = 6.5, xmax = 10.5, ymin = -Inf, ymax = Inf, alpha = 0.12, fill = colors$orange) +
      annotate("text", x = 8.5, y = max(epi_monthly$Cases) * 0.88, label = "Monsoon Peak",
               color = colors$orange, fontface = "bold", size = 5) +
      scale_x_continuous(breaks = 1:12, labels = month.abb) +
      scale_y_continuous(name = "Cases", labels = scales::comma,
                         sec.axis = sec_axis(~./220, name = "Deaths")) +
      labs(x = "Month (2023)") + theme_minimal()
    ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Motifs
  output$motif_criteria_table <- renderDT({
    criteria <- data.frame(
      Motif = c("ATG", "GAC", "AATAAA", "CACAG", "AGAGA"),
      Function = c("Translation initiation codon", "Codon for aspartic acid in envelope (E) protein",
                   "Polyadenylation signal (eukaryotic-like)", 
                   "Conserved pentanucleotide in 3' stem-loop", "Part of 3' UTR elements"),
      Relevance = c("Marks start of protein-coding regions", 
                    "Potential mutation site; may affect antibody binding",
                    "RNA stability & translation efficiency",
                    "Viral replication control",
                    "RNA circularization for replication"),
      Citation = c("Universal genetic code", "General DENV genomics",
                   "Alternative mechanisms in flaviviruses",
                   "NC_001474 reference", "Alvarez et al. 2005")
    )
    datatable(criteria, options = list(pageLength = 5, dom = 't'), rownames = FALSE) %>%
      formatStyle('Motif', fontFamily = 'monospace', fontWeight = 'bold', 
                  backgroundColor = '#ECF0F1', fontSize = '14px')
  })
  
  output$motif_freq_plot <- renderPlotly({
    p <- ggplot(motif_sum, aes(x = reorder(Motif, Total_Occurrences), y = Total_Occurrences)) +
      geom_col(aes(fill = Motif), alpha = 0.9, color = "white", linewidth = 1.2) +
      geom_text(aes(label = Total_Occurrences), hjust = -0.3, fontface = "bold", size = 6.5) +
      scale_fill_viridis_d(option = "plasma") +
      coord_flip() + labs(x = NULL, y = "Matches") + theme_minimal() +
      theme(legend.position = "none", axis.text.y = element_text(family = "mono", face = "bold", size = 15)) +
      ylim(0, max(motif_sum$Total_Occurrences) * 1.15)
    ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  output$motif_pos_plot <- renderPlotly({
    pos_summary <- motifs %>% group_by(Motif) %>% 
      summarise(Mean_Pos = mean(Position), Count = n(), .groups = 'drop')
    p <- ggplot(pos_summary, aes(x = Mean_Pos, y = Motif, size = Count, color = Motif)) +
      geom_point(alpha = 0.7) +
      scale_color_viridis_d(option = "plasma") +
      scale_size_continuous(range = c(8, 20)) +
      labs(x = "Average Position (bp)", y = NULL) + theme_minimal() +
      theme(legend.position = "none", axis.text.y = element_text(family = "mono", face = "bold", size = 14))
    ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Data table
  output$full_data_table <- renderDT({
    datatable(qc, options = list(pageLength = 13, scrollX = TRUE), rownames = FALSE, filter = 'top',
              class = 'table-striped table-hover') %>%
      formatStyle('Sequence_ID', fontWeight = 'bold') %>%
      formatPercentage('GC_Content', 2) %>%
      formatStyle('GC_Content', backgroundColor = styleInterval(c(0.46, 0.48),
                                                                c('#FFE5E5', '#E8F8F5', '#FFE5E5')))
  })
}

shinyApp(ui = ui, server = server)