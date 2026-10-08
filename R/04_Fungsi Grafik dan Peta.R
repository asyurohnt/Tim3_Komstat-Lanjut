# ============================================================
# PROYEK KOMPUTASI STATISTIKA
# CHECKPOINT 3
# FUNGSI GRAFIK, PROFIL WILAYAH, DAN PETA KOROPLET
# ============================================================

# ============================================================
# 1. LOAD PACKAGE
# ============================================================

library(dplyr)
library(ggplot2)
library(sf)
library(purrr)
library(colorspace)


# ============================================================
# 2. LOAD DATA
# ============================================================

data_clean <- readRDS("03_Data Clean.rds")

# Menghilangkan geometry untuk grafik statistik
data_df <- st_drop_geometry(data_clean)


# ============================================================
# 3. PERSIAPAN VARIABEL NUMERIK
# ============================================================

data_df <- data_df %>%
  mutate(
    Kemiskinan = as.numeric(
      gsub(
        ",", ".",
        gsub(
          "[^0-9,.-]",
          "",
          as.character(Kemiskinan)
        )
      )
    ),
    IPM = as.numeric(
      gsub(
        ",", ".",
        gsub(
          "[^0-9,.-]",
          "",
          as.character(IPM)
        )
      )
    )
  )


# ============================================================
# BAGIAN 1
# FUNGSI GRAFIK DENGAN TIDY EVALUATION
# ============================================================


# ------------------------------------------------------------
# 4. FUNGSI GRAFIK TREN
# ------------------------------------------------------------

plot_tren <- function(data, indikator) {
  
  ggplot(
    data,
    aes(
      x = Tahun,
      y = {{ indikator }},
      group = `Kabupaten/kota`
    )
  ) +
    geom_line(
      color = "#123B4A",
      linewidth = 0.8
    ) +
    geom_point(
      color = "#D95F59",
      size = 2
    ) +
    facet_wrap(
      ~ `Kabupaten/kota`,
      ncol = 6
    ) +
    scale_x_continuous(
      breaks = sort(unique(data$Tahun))
    ) +
    labs(
      title = paste(
        "Tren",
        deparse(substitute(indikator)),
        "Kabupaten/Kota Sulawesi Selatan"
      ),
      subtitle = "Periode 2023–2025",
      x = "Tahun",
      y = deparse(substitute(indikator)),
      caption = "Sumber: BPS Provinsi Sulawesi Selatan | Diolah"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.background = element_rect(
        fill = "#F7F9F8",
        color = NA
      ),
      panel.background = element_rect(
        fill = "#F7F9F8",
        color = NA
      ),
      plot.title = element_text(
        size = 17,
        face = "bold",
        color = "#123B4A",
        margin = margin(b = 5)
      ),
      plot.subtitle = element_text(
        size = 11,
        color = "#61727A",
        margin = margin(b = 12)
      ),
      strip.background = element_rect(
        fill = "#EAF3F5",
        color = NA
      ),
      strip.text = element_text(
        size = 8.5,
        face = "bold",
        color = "#123B4A"
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        color = "#DCE5E6",
        linewidth = 0.35
      ),
      axis.text = element_text(
        color = "#304850"
      ),
      plot.caption = element_text(
        hjust = 0,
        size = 9,
        color = "#718087"
      ),
      plot.margin = margin(15, 15, 15, 15)
    )
}


# ------------------------------------------------------------
# 5. FUNGSI GRAFIK DISTRIBUSI
# ------------------------------------------------------------

plot_distribusi <- function(data, indikator) {
  
  ggplot(
    data,
    aes(
      x = factor(Tahun),
      y = {{ indikator }},
      fill = factor(Tahun)
    )
  ) +
    geom_boxplot(
      alpha = 0.65,
      width = 0.55,
      outlier.shape = NA
    ) +
    geom_jitter(
      width = 0.12,
      size = 2,
      alpha = 0.7,
      color = "#D95F59"
    ) +
    scale_fill_manual(
      values = c(
        "2023" = "#72B7B2",
        "2024" = "#E9A03F",
        "2025" = "#123B4A"
      )
    ) +
    labs(
      title = paste(
        "Distribusi",
        deparse(substitute(indikator))
      ),
      subtitle = "Perbandingan distribusi antar-tahun",
      x = "Tahun",
      y = deparse(substitute(indikator)),
      fill = "Tahun",
      caption = "Sumber: BPS Provinsi Sulawesi Selatan | Diolah"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.background = element_rect(
        fill = "#F7F9F8",
        color = NA
      ),
      panel.background = element_rect(
        fill = "#F7F9F8",
        color = NA
      ),
      plot.title = element_text(
        size = 17,
        face = "bold",
        color = "#123B4A",
        margin = margin(b = 5)
      ),
      plot.subtitle = element_text(
        size = 11,
        color = "#61727A",
        margin = margin(b = 12)
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        color = "#DCE5E6",
        linewidth = 0.35
      ),
      legend.position = "bottom",
      legend.title = element_text(
        face = "bold",
        color = "#304850"
      ),
      legend.text = element_text(
        color = "#304850"
      ),
      plot.caption = element_text(
        hjust = 0,
        size = 9,
        color = "#718087"
      ),
      plot.margin = margin(15, 15, 15, 15)
    )
}


# ============================================================
# 6. MENGHASILKAN GRAFIK DARI FUNGSI
# ============================================================

grafik_tren_kemiskinan <- plot_tren(
  data_df,
  Kemiskinan
)

grafik_distribusi_kemiskinan <- plot_distribusi(
  data_df,
  Kemiskinan
)

grafik_tren_ipm <- plot_tren(
  data_df,
  IPM
)

grafik_distribusi_ipm <- plot_distribusi(
  data_df,
  IPM
)


# ============================================================
# 7. MENYIMPAN GRAFIK
# ============================================================

ggsave(
  "CP3_tren_kemiskinan.png",
  grafik_tren_kemiskinan,
  width = 14,
  height = 10,
  dpi = 300,
  bg = "#F7F9F8"
)

ggsave(
  "CP3_distribusi_kemiskinan.png",
  grafik_distribusi_kemiskinan,
  width = 9,
  height = 7,
  dpi = 300,
  bg = "#F7F9F8"
)

ggsave(
  "CP3_tren_IPM.png",
  grafik_tren_ipm,
  width = 14,
  height = 10,
  dpi = 300,
  bg = "#F7F9F8"
)

ggsave(
  "CP3_distribusi_IPM.png",
  grafik_distribusi_ipm,
  width = 9,
  height = 7,
  dpi = 300,
  bg = "#F7F9F8"
)


# ============================================================
# BAGIAN 2
# OTOMATISASI 24 PROFIL KABUPATEN/KOTA
# ============================================================


# ------------------------------------------------------------
# 8. MEMBUAT FOLDER OUTPUT
# ------------------------------------------------------------

if (!dir.exists("CP3_profil_wilayah")) {
  dir.create("CP3_profil_wilayah")
}


# ------------------------------------------------------------
# 9. FUNGSI PROFIL SATU WILAYAH
# ------------------------------------------------------------

plot_profil_wilayah <- function(data, nama_wilayah) {
  
  data_wilayah <- data %>%
    filter(
      `Kabupaten/kota` == nama_wilayah
    )
  
  ggplot(
    data_wilayah,
    aes(
      x = Tahun,
      y = Kemiskinan
    )
  ) +
    geom_line(
      color = "#123B4A",
      linewidth = 1
    ) +
    geom_point(
      color = "#D95F59",
      fill = "#F7F9F8",
      size = 4,
      stroke = 1.2
    ) +
    geom_text(
      aes(
        label = sprintf(
          "%.2f%%",
          Kemiskinan
        )
      ),
      vjust = -1,
      size = 3.5,
      fontface = "bold",
      color = "#304850"
    ) +
    scale_x_continuous(
      breaks = sort(unique(data_wilayah$Tahun))
    ) +
    scale_y_continuous(
      labels = function(x) {
        paste0(x, "%")
      }
    ) +
    labs(
      title = paste(
        "Profil Kemiskinan",
        nama_wilayah
      ),
      subtitle = "Perkembangan persentase penduduk miskin, 2023–2025",
      x = "Tahun",
      y = "Penduduk miskin (%)",
      caption = "Sumber: BPS Provinsi Sulawesi Selatan | Diolah"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(
        fill = "#F7F9F8",
        color = NA
      ),
      panel.background = element_rect(
        fill = "#F7F9F8",
        color = NA
      ),
      plot.title = element_text(
        size = 20,
        face = "bold",
        color = "#123B4A",
        margin = margin(b = 5)
      ),
      plot.subtitle = element_text(
        size = 11,
        color = "#61727A",
        margin = margin(b = 15)
      ),
      axis.text = element_text(
        color = "#304850"
      ),
      axis.title = element_text(
        color = "#304850",
        face = "bold"
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        color = "#DCE5E6",
        linewidth = 0.4
      ),
      plot.caption = element_text(
        hjust = 0,
        size = 9,
        color = "#718087"
      ),
      plot.margin = margin(20, 20, 20, 20)
    )
}


# ------------------------------------------------------------
# 10. DAFTAR WILAYAH
# ------------------------------------------------------------

daftar_wilayah <- sort(
  unique(data_df$`Kabupaten/kota`)
)


# ------------------------------------------------------------
# 11. MEMBUAT 24 PROFIL SECARA OTOMATIS
# ------------------------------------------------------------

walk(
  daftar_wilayah,
  function(wilayah) {
    
    grafik <- plot_profil_wilayah(
      data_df,
      wilayah
    )
    
    nama_file <- paste0(
      "CP3_profil_",
      gsub(
        "[^A-Za-z0-9]+",
        "_",
        wilayah
      ),
      ".png"
    )
    
    ggsave(
      filename = file.path(
        "CP3_profil_wilayah",
        nama_file
      ),
      plot = grafik,
      width = 10,
      height = 7,
      dpi = 300,
      bg = "#F7F9F8"
    )
  }
)


# ============================================================
# BAGIAN 3
# PETA KOROPLET KEMISKINAN 2025
# ============================================================


# ------------------------------------------------------------
# 12. MENYIAPKAN DATA PETA
# ------------------------------------------------------------

peta_kemiskinan <- data_spasial %>%
  filter(Tahun == 2025)


# ------------------------------------------------------------
# 13. PETA KOROPLET
# ------------------------------------------------------------

peta_kemiskinan_plot <- ggplot(
  peta_kemiskinan
) +
  geom_sf(
    aes(fill = Kemiskinan),
    color = "white",
    linewidth = 0.4
  ) +
  scale_fill_continuous_sequential(
    palette = "Blues",
    name = "Kemiskinan (%)"
  ) +
  labs(
    title = "Kemiskinan Kabupaten/Kota Sulawesi Selatan",
    subtitle = "Persentase penduduk miskin, 2025",
    caption = "Sumber: BPS Provinsi Sulawesi Selatan | Diolah"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.background = element_rect(
      fill = "#F7F9F8",
      color = NA
    ),
    plot.title = element_text(
      size = 20,
      face = "bold",
      color = "#123B4A",
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#61727A",
      margin = margin(b = 15)
    ),
    plot.caption = element_text(
      size = 9,
      color = "#718087",
      hjust = 0
    ),
    legend.position = "right",
    legend.title = element_text(
      face = "bold",
      color = "#304850"
    ),
    legend.text = element_text(
      color = "#304850"
    ),
    plot.margin = margin(20, 20, 20, 20)
  )


# ------------------------------------------------------------
# 14. SIMPAN PETA KOROPLET
# ------------------------------------------------------------

ggsave(
  "CP3_peta_koroplet_kemiskinan_2025.png",
  peta_kemiskinan_plot,
  width = 11,
  height = 9,
  dpi = 300,
  bg = "#F7F9F8"
)


# ============================================================
# BAGIAN 4
# SIMULASI DEUTERANOPIA DAN PROTANOPIA
# ============================================================


# ------------------------------------------------------------
# 15. PALET WARNA PETA
# ------------------------------------------------------------

warna_peta <- c(
  "#EFF6FB",
  "#C6DBEF",
  "#9ECAE1",
  "#6BAED6",
  "#4292C6",
  "#2171B5",
  "#08519C",
  "#08306B"
)


# ------------------------------------------------------------
# 16. SIMULASI PERSEPSI WARNA
# ------------------------------------------------------------

warna_deuteranopia <- colorspace::deutan(
  warna_peta,
  severity = 1
)

warna_protanopia <- colorspace::protan(
  warna_peta,
  severity = 1
)


# ------------------------------------------------------------
# 17. PETA SIMULASI DEUTERANOPIA
# ------------------------------------------------------------

peta_deuteranopia <- peta_kemiskinan_plot +
  scale_fill_gradientn(
    colors = warna_deuteranopia,
    name = "Kemiskinan (%)"
  ) +
  labs(
    title = "Simulasi Deuteranopia",
    subtitle = "Persentase penduduk miskin, 2025"
  )


# ------------------------------------------------------------
# 18. PETA SIMULASI PROTANOPIA
# ------------------------------------------------------------

peta_protanopia <- peta_kemiskinan_plot +
  scale_fill_gradientn(
    colors = warna_protanopia,
    name = "Kemiskinan (%)"
  ) +
  labs(
    title = "Simulasi Protanopia",
    subtitle = "Persentase penduduk miskin, 2025"
  )


# ------------------------------------------------------------
# 19. SIMPAN HASIL SIMULASI
# ------------------------------------------------------------

ggsave(
  "CP3_peta_deuteranopia.png",
  peta_deuteranopia,
  width = 11,
  height = 9,
  dpi = 300,
  bg = "#F7F9F8"
)

ggsave(
  "CP3_peta_protanopia.png",
  peta_protanopia,
  width = 11,
  height = 9,
  dpi = 300,
  bg = "#F7F9F8"
)


# ============================================================
# SELESAI
# ============================================================