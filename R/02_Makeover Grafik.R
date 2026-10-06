# ============================================================
# MAKEOVER VISUALISASI KEMISKINAN
# Kabupaten/Kota Sulawesi Selatan - 2025
# ============================================================

library(dplyr)
library(ggplot2)


# ============================================================
# 1. MENYIAPKAN DATA
# ============================================================

data_grafik <- data_clean %>%
  filter(Tahun == 2025) %>%
  mutate(
    Kemiskinan_num = as.numeric(
      gsub(
        ",",
        ".",
        gsub(
          "[^0-9,.-]",
          "",
          as.character(Kemiskinan)
        )
      )
    )
  ) %>%
  filter(!is.na(Kemiskinan_num))


# ============================================================
# 2. MENGURUTKAN WILAYAH
# ============================================================

data_grafik <- data_grafik %>%
  arrange(Kemiskinan_num) %>%
  mutate(
    wilayah = factor(
      `Kabupaten/kota`,
      levels = `Kabupaten/kota`
    )
  )


# ============================================================
# 3. STATISTIK
# ============================================================

rata_rata <- mean(
  data_grafik$Kemiskinan_num,
  na.rm = TRUE
)

nilai_maks <- max(
  data_grafik$Kemiskinan_num,
  na.rm = TRUE
)

nilai_min <- min(
  data_grafik$Kemiskinan_num,
  na.rm = TRUE
)

wilayah_maks <- data_grafik %>%
  filter(
    Kemiskinan_num == nilai_maks
  ) %>%
  pull(`Kabupaten/kota`) %>%
  first()

wilayah_min <- data_grafik %>%
  filter(
    Kemiskinan_num == nilai_min
  ) %>%
  pull(`Kabupaten/kota`) %>%
  first()


# ============================================================
# 4. MENENTUKAN KATEGORI WARNA
# ============================================================

data_grafik <- data_grafik %>%
  mutate(
    kategori = case_when(
      Kemiskinan_num == nilai_maks ~ "Tertinggi",
      Kemiskinan_num == nilai_min ~ "Terendah",
      Kemiskinan_num >= quantile(
        Kemiskinan_num,
        0.75,
        na.rm = TRUE
      ) ~ "Tinggi",
      TRUE ~ "Lainnya"
    )
  )


# ============================================================
# 5. GRAFIK MAKEOVER
# ============================================================

grafik_makeover <- ggplot(
  data_grafik,
  aes(
    x = Kemiskinan_num,
    y = wilayah
  )
) +
  
  # ----------------------------------------------------------
# BATANG
# ----------------------------------------------------------

geom_col(
  aes(fill = kategori),
  width = 0.65
) +
  
  # ----------------------------------------------------------
# GARIS RATA-RATA PROVINSI
# ----------------------------------------------------------

geom_vline(
  xintercept = rata_rata,
  linetype = "dashed",
  linewidth = 0.9,
  color = "#E76F51"
) +
  
  # ----------------------------------------------------------
# LABEL RATA-RATA
# ----------------------------------------------------------

annotate(
  "label",
  x = rata_rata,
  y = Inf,
  label = paste0(
    "Rata-rata Sulsel  ",
    sprintf("%.2f", rata_rata),
    "%"
  ),
  vjust = 1.5,
  hjust = -0.05,
  size = 3.4,
  fontface = "bold",
  color = "#C8553D",
  fill = "#FFF4EF",
  label.size = 0
) +
  
  # ----------------------------------------------------------
# LABEL NILAI
# ----------------------------------------------------------

geom_text(
  aes(
    label = paste0(
      sprintf(
        "%.2f",
        Kemiskinan_num
      ),
      "%"
    )
  ),
  hjust = -0.15,
  size = 3.5,
  fontface = "bold",
  color = "#173F4F"
) +
  
  # ----------------------------------------------------------
# WARNA
# ----------------------------------------------------------

scale_fill_manual(
  values = c(
    "Tertinggi" = "#D95F59",
    "Tinggi" = "#E9A03F",
    "Lainnya" = "#72B7B2",
    "Terendah" = "#287D8E"
  )
) +
  
  # ----------------------------------------------------------
# SUMBU X
# ----------------------------------------------------------

scale_x_continuous(
  labels = function(x) {
    paste0(
      x,
      "%"
    )
  },
  expand = expansion(
    mult = c(
      0,
      0.12
    )
  )
) +
  
  # ----------------------------------------------------------
# JUDUL
# ----------------------------------------------------------

labs(
  title = "Di Mana Kemiskinan Paling Tinggi?",
  
  subtitle = paste0(
    "Persentase penduduk miskin menurut kabupaten/kota ",
    "di Sulawesi Selatan, 2025"
  ),
  
  x = "Persentase penduduk miskin",
  
  y = NULL,
  
  caption = paste0(
    "Tertinggi: ",
    wilayah_maks,
    " (",
    sprintf("%.2f", nilai_maks),
    "%)  •  ",
    "Terendah: ",
    wilayah_min,
    " (",
    sprintf("%.2f", nilai_min),
    "%)\n",
    "Sumber: BPS Provinsi Sulawesi Selatan | Diolah"
  )
) +
  
  # ----------------------------------------------------------
# TEMA
# ----------------------------------------------------------

theme_minimal(
  base_size = 12
) +
  
  theme(
    
    # Background
    plot.background = element_rect(
      fill = "#F7F9F8",
      color = NA
    ),
    
    panel.background = element_rect(
      fill = "#F7F9F8",
      color = NA
    ),
    
    # Judul
    plot.title = element_text(
      size = 23,
      face = "bold",
      color = "#123B4A",
      margin = margin(
        b = 5
      )
    ),
    
    plot.subtitle = element_text(
      size = 12,
      color = "#61727A",
      margin = margin(
        b = 18
      )
    ),
    
    # Axis
    axis.text.y = element_text(
      size = 10.5,
      color = "#304850"
    ),
    
    axis.text.x = element_text(
      size = 10,
      color = "#64757C"
    ),
    
    axis.title.x = element_text(
      size = 11,
      face = "bold",
      color = "#304850",
      margin = margin(
        t = 12
      )
    ),
    
    # Grid
    panel.grid.major.y = element_blank(),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_line(
      color = "#DCE5E6",
      linewidth = 0.4
    ),
    
    # Legend
    legend.position = "none",
    
    # Caption
    plot.caption = element_text(
      size = 9,
      color = "#718087",
      hjust = 0,
      margin = margin(
        t = 15
      )
    ),
    
    # Margin
    plot.margin = margin(
      20,
      35,
      20,
      20
    )
  )


# ============================================================
# 6. TAMPILKAN
# ============================================================

grafik_makeover


# ============================================================
# 7. SIMPAN
# ============================================================

ggsave(
  "makeover_kemiskinan_sulsel_2025.png",
  grafik_makeover,
  width = 12,
  height = 9,
  dpi = 300,
  bg = "#F7F9F8"
)