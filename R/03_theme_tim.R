# ============================================================
# PROYEK KOMPUTASI STATISTIKA - CHECKPOINT 2
# HARI 4-6: THEME KELOMPOK, PALET WARNA, DAN GALERI GRAFIK
# ============================================================

library(ggplot2)
library(dplyr)
library(sf)
library(scales)

# 1. Load Data
data_clean <- readRDS("data_clean.rds") 

# 2. FIX NUMERIK (Mengatasi Error Continuous/Discrete)
data_clean <- data_clean %>%
  mutate(
    Kemiskinan = as.numeric(gsub(",", ".", as.character(Kemiskinan))),
    IPM        = as.numeric(gsub(",", ".", as.character(IPM)))
  )

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
      plot.background  = element_rect(fill = palet_tim$netral_bg, color = NA),
      panel.background = element_rect(fill = palet_tim$netral_bg, color = NA),
      plot.title    = element_text(size = rel(1.4), face = "bold", color = palet_tim$utama, margin = margin(b = 6)),
      plot.subtitle = element_text(size = rel(1.0), color = "#61727A", margin = margin(b = 15)),
      plot.caption  = element_text(size = rel(0.8), color = "#718087", hjust = 0, margin = margin(t = 12)),
      axis.title.x = element_text(size = rel(0.9), face = "bold", color = palet_tim$teks_gelap, margin = margin(t = 8)),
      axis.title.y = element_text(size = rel(0.9), face = "bold", color = palet_tim$teks_gelap, angle = 90, margin = margin(r = 8)),
      axis.text    = element_text(size = rel(0.85), color = palet_tim$teks_gelap),
      panel.grid.major = element_line(color = palet_tim$netral_grid, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = palet_tim$netral_bg, color = NA),
      legend.title      = element_text(size = rel(0.9), face = "bold", color = palet_tim$teks_gelap),
      legend.text       = element_text(size = rel(0.85), color = palet_tim$teks_gelap),
      legend.position   = "bottom",
      plot.margin = margin(15, 20, 15, 15)
    )
}


# ============================================================
# GALERI GRAFIK STATISTIK
# ============================================================

# Grafik 1: Bar Chart Horizontal

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
    size = 3.2, 
    color = palet_tim$teks_gelap, 
    fontface = "bold"
  ) +
  annotate(
    "label", 
    x = rata_sulsel, 
    y = daerah_tertinggi, 
    label = sprintf("Rata-rata Sulsel  %.2f%%", rata_sulsel), 
    color = palet_tim$sorotan_max, 
    fill = palet_tim$netral_bg,
    label.size = NA,
    fontface = "bold",
    hjust = -0.05,
    size = 3.3
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
    title = "1. Peringkat Kemiskinan Kabupaten/Kota (2025)",
    subtitle = "Persentase penduduk miskin menurut kabupaten/kota di Sulawesi Selatan, 2025",
    x = "Persentase penduduk miskin",
    y = NULL,
    caption = "Tertinggi: Pangkep (11.60%)  •  Terendah: Makassar (4.43%)\nSumber: BPS Provinsi Sulawesi Selatan | Diolah"
  ) +
  theme_tim() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = rel(1.4), face = "bold", color = palet_tim$utama),
    plot.subtitle = element_text(size = rel(0.95), color = "#718087")
  )

g2 <- ggplot(df_2025, aes(x = IPM, y = Kemiskinan)) +
  geom_point(color = palet_tim$utama, size = 3.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = palet_tim$sorotan_max, linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "2. Hubungan IPM dan Kemiskinan (2025)",
    subtitle = "Korelasi antara Indeks Pembangunan Manusia dan Persentase Kemiskinan",
    x = "Indeks Pembangunan Manusia (IPM)",
    y = "Kemiskinan (%)"
  ) +
  theme_tim()

# Grafik 3: Line Chart
df_tren <- data_df %>%
  group_by(Tahun) %>%
  summarise(Rata_Kemiskinan = mean(Kemiskinan, na.rm = TRUE))

g3 <- ggplot(df_tren, aes(x = factor(Tahun), y = Rata_Kemiskinan, group = 1)) +
  geom_line(color = palet_tim$utama, linewidth = 1.2) +
  geom_point(color = palet_tim$sorotan_max, size = 4) +
  geom_text(aes(label = sprintf("%.2f%%", Rata_Kemiskinan)), vjust = -1.2, fontface = "bold", color = palet_tim$utama) +
  scale_y_continuous(limits = c(min(df_tren$Rata_Kemiskinan) - 0.5, max(df_tren$Rata_Kemiskinan) + 0.5)) +
  labs(
    title = "3. Tren Rata-Rata Kemiskinan (2023–2025)",
    subtitle = "Perkembangan rata-rata kemiskinan kabupaten/kota di Sulsel",
    x = "Tahun",
    y = "Rata-Rata Kemiskinan (%)"
  ) +
  theme_tim()

# Grafik 4: Boxplot
g4 <- ggplot(data_df, aes(x = factor(Tahun), y = Kemiskinan, fill = factor(Tahun))) +
  geom_boxplot(alpha = 0.7, color = palet_tim$utama, outlier.color = palet_tim$sorotan_max) +
  scale_fill_manual(values = c("2023" = "#72B7B2", "2024" = "#E9A03F", "2025" = "#123B4A")) +
  labs(
    title = "4. Distribusi Kemiskinan Antar-Tahun",
    subtitle = "Melihat variasi dan pencilan tingkat kemiskinan",
    x = "Tahun",
    y = "Kemiskinan (%)"
  ) +
  theme_tim() +
  theme(legend.position = "none")

# Grafik 5: Peta Spasial

peta_data <- data_clean %>%
  filter(Tahun == 2025) %>%
  mutate(Kemiskinan_cat = factor(Kemiskinan))

g5 <- ggplot(peta_data) +
  geom_sf(aes(fill = Kemiskinan_cat), color = "black", linewidth = 0.3) +
  
  labs(
    title = "5. Persentase Penduduk Miskin (2025)",
    subtitle = "Kabupaten/Kota di Sulawesi Selatan, 2025",
    fill = "Kemiskinan (%)",
    caption = "Sumber: BPS Provinsi Sulawesi Selatan | Diolah Kelompok"
  ) +
  
  guides(fill = guide_legend(ncol = 2, byrow = FALSE)) +
  
  theme_tim() +
    theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    axis.text        = element_text(size = rel(0.75), color = palet_tim$teks_gelap),
        legend.position  = "right",
    legend.title     = element_text(size = rel(0.9), face = "bold", color = palet_tim$teks_gelap),
    legend.text      = element_text(size = rel(0.8), color = palet_tim$teks_gelap),
        plot.title       = element_text(size = rel(1.4), face = "bold", color = palet_tim$utama, hjust = 0),
    plot.subtitle    = element_text(size = rel(1.0), color = "#61727A", hjust = 0)
  )


# Cetak & Simpan
print(g1)
print(g2)
print(g3)
print(g4)
print(g5)

ggsave("galeri_grafik_1.png", g1, width = 10, height = 8, dpi = 300)
ggsave("galeri_peta_5.png", g5, width = 9, height = 8, dpi = 300)

