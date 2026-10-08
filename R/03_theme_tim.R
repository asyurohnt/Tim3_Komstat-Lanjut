# ============================================================
# PROYEK KOMPUTASI STATISTIKA - CHECKPOINT 2
# HARI 4-6: THEME KELOMPOK, PALET WARNA, DAN GALERI GRAFIK STATISTIK
# ============================================================

library(ggplot2)
library(dplyr)
library(sf)
library(scales)
library(patchwork)

# 1. Load Data
data_clean <- readRDS("03_Data_clean.rds") 

# 2. FIX NUMERIK & PERSIAPAN DATAFRAME (data_df)
data_clean <- data_clean %>%
  mutate(
    Kemiskinan = as.numeric(gsub(",", ".", as.character(Kemiskinan))),
    IPM        = as.numeric(gsub(",", ".", as.character(IPM)))
  )

# Hapus geometri spatial untuk membuat data_df (Data Frame Biasa)
data_df <- st_drop_geometry(data_clean)


# 3. Palet Warna Kelompok
palet_tim <- list(
  utama       = "#123B4A",
  sekunder    = "#72B7B2",
  sorotan_max = "#D95F59",
  sorotan_min = "#287D8E",
  aksen       = "#E9A03F",
  netral_bg   = "#F7F9F8",
  netral_grid = "#DCE5E6",
  teks_gelap  = "#304850"
)


# 4. Fungsi theme_tim()
theme_tim <- function(base_size = 11, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.background   = element_rect(fill = palet_tim$netral_bg, color = NA),
      panel.background  = element_rect(fill = palet_tim$netral_bg, color = NA),
      plot.title        = element_text(size = rel(1.3), face = "bold", color = palet_tim$utama, margin = margin(b = 6)),
      plot.subtitle     = element_text(size = rel(0.95), color = "#61727A", margin = margin(b = 10)),
      plot.caption      = element_text(size = rel(0.8), color = "#718087", hjust = 0, margin = margin(t = 10)),
      axis.title.x      = element_text(size = rel(0.85), face = "bold", color = palet_tim$teks_gelap, margin = margin(t = 6)),
      axis.title.y      = element_text(size = rel(0.85), face = "bold", color = palet_tim$teks_gelap, angle = 90, margin = margin(r = 6)),
      axis.text         = element_text(size = rel(0.8), color = palet_tim$teks_gelap),
      panel.grid.major  = element_line(color = palet_tim$netral_grid, linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      legend.background = element_rect(fill = palet_tim$netral_bg, color = NA),
      legend.title      = element_text(size = rel(0.85), face = "bold", color = palet_tim$teks_gelap),
      legend.text       = element_text(size = rel(0.8), color = palet_tim$teks_gelap),
      legend.position   = "bottom",
      plot.margin       = margin(12, 12, 12, 12)
    )
}


# ============================================================
# GALERI GRAFIK STATISTIK
# ============================================================

# --- Grafik 1: Bar Chart Horizontal Peringkat Kemiskinan ---
rata_sulsel <- mean(data_df$Kemiskinan[data_df$Tahun == 2025], na.rm = TRUE)

df_2025 <- data_df %>%
  filter(Tahun == 2025) %>%
  mutate(
    `Kabupaten/kota` = reorder(`Kabupaten/kota`, Kemiskinan),
    kategori_warna = case_when(
      Kemiskinan == max(Kemiskinan) ~ "Tersebesar",
      Kemiskinan == min(Kemiskinan) ~ "Terendah",
      Kemiskinan >= 10.0 ~ "Tinggi (>10%)",
      TRUE ~ "Sedang"
    )
  )

daerah_tertinggi <- as.character(df_2025$`Kabupaten/kota`[which.max(df_2025$Kemiskinan)])

g1 <- ggplot(df_2025, aes(x = Kemiskinan, y = `Kabupaten/kota`, fill = kategori_warna)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = rata_sulsel, linetype = "dashed", color = palet_tim$sorotan_max, linewidth = 0.8) +
  geom_text(
    aes(label = sprintf("%.2f%%", Kemiskinan)), 
    hjust = -0.15, 
    size = 2.8, 
    color = palet_tim$teks_gelap, 
    fontface = "bold"
  ) +
  annotate(
    "label", 
    x = rata_sulsel, 
    y = daerah_tertinggi, 
    label = sprintf("Rata-rata Sulsel %.2f%%", rata_sulsel), 
    color = palet_tim$sorotan_max, 
    fill = palet_tim$netral_bg,
    label.size = NA,
    fontface = "bold",
    hjust = -0.05,
    size = 3
  ) +
  scale_fill_manual(
    values = c(
      "Tersebesar"    = palet_tim$sorotan_max,
      "Tinggi (>10%)" = palet_tim$aksen,
      "Sedang"        = palet_tim$sekunder,
      "Terendah"      = palet_tim$utama
    )
  ) +
  scale_x_continuous(
    labels = percent_format(scale = 1), 
    limits = c(0, 13),
    breaks = seq(0, 12.5, by = 2.5),
    expand = c(0, 0)
  ) +
  labs(
    title = "Peringkat Kemiskinan Kabupaten/Kota (2025)",
    subtitle = "Persentase penduduk miskin menurut kabupaten/kota di Sulawesi Selatan",
    x = "Persentase penduduk miskin",
    y = NULL,
    caption = "Tertinggi: Pangkep (11.60%) • Terendah: Makassar (4.43%)\nSumber: BPS Provinsi Sulawesi Selatan | Diolah"
  ) +
  theme_tim() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )


# --- Grafik 2: Scatter Plot IPM vs Kemiskinan dengan Pita SK ---
g2 <- ggplot(df_2025, aes(x = IPM, y = Kemiskinan)) +
  geom_point(color = palet_tim$utama, size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = palet_tim$sorotan_max, fill = palet_tim$sekunder, alpha = 0.3, linewidth = 0.8) +
  labs(
    title = "Hubungan IPM dan Kemiskinan (2025)",
    subtitle = "Korelasi IPM dan Kemiskinan dilengkapi Pita Selang Kepercayaan 95%",
    x = "Indeks Pembangunan Manusia (IPM)",
    y = "Kemiskinan (%)"
  ) +
  theme_tim()


# --- Grafik 3: Tren Waktu Small Multiples 24 Wilayah ---
g3 <- ggplot(data_df, aes(x = factor(Tahun), y = Kemiskinan, group = `Kabupaten/kota`)) +
  geom_line(color = palet_tim$utama, linewidth = 0.8) +
  geom_point(color = palet_tim$sorotan_max, size = 1.5) +
  facet_wrap(~ `Kabupaten/kota`, ncol = 6) +
  labs(
    title = "Tren Kemiskinan 24 Kabupaten/Kota (2023–2025)",
    subtitle = "Perkembangan tren waktu (small multiples) per wilayah di Sulawesi Selatan",
    x = "Tahun",
    y = "Kemiskinan (%)"
  ) +
  theme_tim() +
  theme(
    strip.background = element_rect(fill = palet_tim$sekunder, color = NA),
    strip.text = element_text(color = "white", fontface = "bold", size = rel(0.7)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.65))
  )


# --- Grafik 4: Boxplot + Titik Jitter ---
g4 <- ggplot(data_df, aes(x = factor(Tahun), y = Kemiskinan, fill = factor(Tahun))) +
  geom_boxplot(alpha = 0.5, color = palet_tim$utama, outlier.shape = NA) +
  geom_jitter(color = palet_tim$sorotan_max, width = 0.15, size = 2, alpha = 0.7) +
  scale_fill_manual(values = c("2023" = "#72B7B2", "2024" = "#E9A03F", "2025" = "#123B4A")) +
  labs(
    title = "Distribusi Kemiskinan Antar-Tahun",
    subtitle = "Variasi sebaran data (Boxplot + titik lokasi individu)",
    x = "Tahun",
    y = "Kemiskinan (%)"
  ) +
  theme_tim() +
  theme(legend.position = "none")


# --- Grafik 5: Peta Spasial Kemiskinan (2025) ---
peta_data <- data_clean %>%
  filter(Tahun == 2025) %>%
  mutate(Kemiskinan_cat = factor(Kemiskinan))

g5 <- ggplot(peta_data) +
  geom_sf(aes(fill = Kemiskinan_cat), color = "black", linewidth = 0.3) +
  labs(
    title = "Peta Spasial Kemiskinan Sulsel (2025)",
    subtitle = "Kabupaten/Kota di Sulawesi Selatan, 2025",
    fill = "Kemiskinan (%)",
    caption = "Sumber: BPS Provinsi Sulawesi Selatan | Diolah Kelompok"
  ) +
  guides(fill = guide_legend(ncol = 2, byrow = FALSE)) +
  theme_tim() +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    axis.text        = element_text(size = rel(0.7), color = palet_tim$teks_gelap),
    legend.position  = "right",
    legend.title     = element_text(size = rel(0.85), face = "bold", color = palet_tim$teks_gelap),
    legend.text      = element_text(size = rel(0.75), color = palet_tim$teks_gelap)
  )


# ============================================================
# CETAK & SIMPAN MASING-MASING GRAFIK
# ============================================================

print(g1)
print(g2)
print(g3)
print(g4)
print(g5)

ggsave("galeri_grafik_1.png", g1, width = 10, height = 8, dpi = 300)
ggsave("galeri_grafik_2.png", g2, width = 8, height = 6, dpi = 300)
ggsave("galeri_grafik_3.png", g3, width = 12, height = 8, dpi = 300)
ggsave("galeri_grafik_4.png", g4, width = 8, height = 6, dpi = 300)
ggsave("galeri_peta_5.png", g5, width = 9, height = 8, dpi = 300)


# ============================================================
# KOMPOSISI MULTIPANEL DENGAN PATCHWORK
# ============================================================

komposisi_patchwork <- (g1 | g2) / 
  g3 / 
  (g4 | g5) +
  plot_layout(heights = c(1.2, 1.8, 1.2)) +
  plot_annotation(
    title = "GALERI GRAFIK STATISTIKA KEMISKINAN SULAWESI SELATAN",
    subtitle = "Komposisi Multipanel Analisis Kemiskinan (2023-2025)",
    caption = "Sumber Data: BPS Provinsi Sulawesi Selatan | Diolah Kelompok",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", color = palet_tim$utama, hjust = 0.5),
      plot.subtitle = element_text(size = 12, color = "#61727A", hjust = 0.5),
      plot.caption = element_text(size = 10, color = "#718087", hjust = 1)
    )
  )

# Tampilkan & Simpan Hasil Gabungan Patchwork
print(komposisi_patchwork)
ggsave("komposisi_patchwork_final.png", komposisi_patchwork, width = 18, height = 24, dpi = 300)
