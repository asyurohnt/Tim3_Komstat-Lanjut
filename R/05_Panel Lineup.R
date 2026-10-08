# ============================================================
# CP3 - 20 PANEL LINEUP
# IPM vs KEMISKINAN KABUPATEN/KOTA SULAWESI SELATAN
# ============================================================

library(dplyr)
library(ggplot2)
library(sf)
library(patchwork)

# ============================================================
# 1. LOAD DATA
# ============================================================

data_clean <- readRDS("03_Data Clean.rds")

data_lineup <- data_clean %>%
  st_drop_geometry() %>%
  filter(
    Tahun == 2025,
    !is.na(`Kabupaten/kota`),
    !is.na(IPM),
    !is.na(Kemiskinan)
  ) %>%
  select(
    `Kabupaten/kota`,
    IPM,
    Kemiskinan
  )

# Cek jumlah wilayah
nrow(data_lineup)

# ============================================================
# 2. FUNGSI MEMBUAT GRAFIK LINEUP
# ============================================================

buat_panel <- function(data, nomor_panel) {
  
  ggplot(
    data,
    aes(
      x = IPM,
      y = Kemiskinan
    )
  ) +
    geom_point(
      color = "#123B4A",
      size = 2.5,
      alpha = 0.8
    ) +
    
    geom_smooth(
      method = "lm",
      se = TRUE,
      color = "#D95F59",
      fill = "#72B7B2",
      alpha = 0.25,
      linewidth = 0.7
    ) +
    
    labs(
      title = as.character(nomor_panel),
      x = "IPM",
      y = "Kemiskinan (%)"
    ) +
    
    theme_minimal(base_size = 8) +
    
    theme(
      plot.title = element_text(
        size = 10,
        face = "bold",
        hjust = 0.5,
        color = "#304850"
      ),
      axis.title = element_text(
        size = 7,
        color = "#304850"
      ),
      axis.text = element_text(
        size = 6,
        color = "#61727A"
      ),
      panel.grid.major = element_line(
        color = "#DCE5E6",
        linewidth = 0.3
      ),
      panel.grid.minor = element_blank(),
      plot.margin = margin(3, 3, 3, 3)
    )
}


# ============================================================
# 3. PANEL DATA ASLI
# ============================================================

panel_asli <- data_lineup


# ============================================================
# 4. MEMBUAT 19 PANEL SIMULASI
# ============================================================

set.seed(123)

panel_simulasi <- vector("list", 19)

for (i in 1:19) {
  
  panel_simulasi[[i]] <- data_lineup %>%
    mutate(
      Kemiskinan = sample(
        Kemiskinan,
        size = n(),
        replace = FALSE
      )
    )
}


# ============================================================
# 5. GABUNGKAN PANEL ASLI + SIMULASI
# ============================================================

semua_panel <- c(
  list(panel_asli),
  panel_simulasi
)

# ============================================================
# 6. ACAK POSISI PANEL
# ============================================================

set.seed(456)

urutan_panel <- sample(1:20)

semua_panel_acak <- semua_panel[urutan_panel]


# Menentukan posisi panel asli
posisi_panel_asli <- which(urutan_panel == 1)

cat(
  "Panel asli berada pada posisi:",
  posisi_panel_asli,
  "\n"
)


# ============================================================
# 7. MEMBUAT 20 GRAFIK
# ============================================================

grafik_panel <- lapply(
  seq_along(semua_panel_acak),
  function(i) {
    
    buat_panel(
      semua_panel_acak[[i]],
      i
    )
    
  }
)


# ============================================================
# 8. GABUNGKAN MENJADI 20-PANEL LINEUP
# ============================================================

lineup <- wrap_plots(
  grafik_panel,
  ncol = 5
) +
  
  plot_annotation(
    title = "Manakah Panel yang Paling Berbeda dari yang Lain?",
    subtitle = "Pilih satu panel yang menurut Anda paling menunjukkan pola hubungan IPM dan kemiskinan",
    caption = "Data Kabupaten/Kota Sulawesi Selatan, 2025 | Panel asli disembunyikan dari pengamat",
    theme = theme(
      plot.title = element_text(
        size = 16,
        face = "bold",
        color = "#123B4A",
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 10,
        color = "#61727A",
        hjust = 0.5
      ),
      plot.caption = element_text(
        size = 8,
        color = "#718087",
        hjust = 0
      )
    )
  )


# ============================================================
# 9. TAMPILKAN LINEUP
# ============================================================

print(lineup)


# ============================================================
# 10. SIMPAN LINEUP
# ============================================================

ggsave(
  "lineup_IPM_Kemiskinan.png",
  lineup,
  width = 14,
  height = 10,
  dpi = 300,
  bg = "#F7F9F8"
)


# ============================================================
# 11. SIMPAN INFORMASI PANEL ASLI
# ============================================================

cat(
  "Panel asli adalah panel nomor:",
  posisi_panel_asli,
  "\n"
)
