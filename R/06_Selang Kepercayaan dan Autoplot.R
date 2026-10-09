# ============================================================
# HARI 10-11
# GRAFIK KETIDAKPASTIAN DENGAN SK BOOTSTRAP 95%
# ============================================================

library(dplyr)
library(ggplot2)
library(sf)

# ------------------------------------------------------------
# 1. Membaca data
# ------------------------------------------------------------

data_clean <- readRDS("03_Data Clean.rds")


# ------------------------------------------------------------
# 2. Fungsi Bootstrap
# ------------------------------------------------------------

set.seed(123)

bootstrap_sk <- function(x, B = 2000, conf = 0.95) {
  
  x <- x[!is.na(x)]
  
  hasil_boot <- replicate(
    B,
    mean(sample(
      x,
      size = length(x),
      replace = TRUE
    ))
  )
  
  alpha <- 1 - conf
  
  tibble(
    estimasi = mean(x),
    bawah = quantile(hasil_boot, alpha / 2),
    atas = quantile(hasil_boot, 1 - alpha / 2)
  )
}


# ------------------------------------------------------------
# 3. Bootstrap untuk setiap kabupaten/kota
# ------------------------------------------------------------

hasil_bootstrap <- data_clean %>%
  st_drop_geometry() %>%
  filter(
    Tahun %in% c(2023, 2024, 2025),
    !is.na(`Kabupaten/kota`),
    !is.na(Kemiskinan)
  ) %>%
  group_by(`Kabupaten/kota`) %>%
  summarise(
    bootstrap_sk(Kemiskinan),
    .groups = "drop"
  ) %>%
  arrange(estimasi)

print(hasil_bootstrap, n = 24)


# ------------------------------------------------------------
# 4. Grafik ketidakpastian
# ------------------------------------------------------------

grafik_bootstrap <- ggplot(
  hasil_bootstrap,
  aes(
    x = estimasi,
    y = reorder(`Kabupaten/kota`, estimasi)
  )
) +
  geom_errorbar(
    aes(
      xmin = bawah,
      xmax = atas
    ),
    height = 0.18,
    linewidth = 0.9,
    color = "#72B7B2"
  ) +
  geom_point(
    size = 3.2,
    color = "#123B4A"
  ) +
  scale_x_continuous(
    labels = function(x) {
      paste0(sprintf("%.1f", x), "%")
    },
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  labs(
    title = "Ketidakpastian Estimasi Kemiskinan",
    subtitle = paste(
      "Rata-rata 2023–2025 dengan Selang Kepercayaan",
      "Bootstrap 95% (B = 2.000)"
    ),
    x = "Persentase penduduk miskin",
    y = NULL,
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
      color = "#123B4A"
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#61727A"
    ),
    axis.text.y = element_text(
      size = 9.5,
      color = "#304850"
    ),
    axis.text.x = element_text(
      color = "#61727A"
    ),
    axis.title.x = element_text(
      size = 11,
      face = "bold",
      color = "#304850"
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      color = "#DCE5E6",
      linewidth = 0.4
    ),
    plot.caption = element_text(
      size = 9,
      color = "#718087",
      hjust = 0
    )
  )

print(grafik_bootstrap)


# ------------------------------------------------------------
# 5. Menyimpan grafik
# ------------------------------------------------------------

ggsave(
  "H10-11_ketidakpastian_bootstrap.png",
  grafik_bootstrap,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "#F7F9F8"
)

# ============================================================
# HARI 10–11
# METODE S3 DAN AUTOPLOT
# ============================================================

library(dplyr)
library(ggplot2)
library(sf)

# Membaca data hasil data cleaning
data_clean <- readRDS("03_Data Clean.rds")


# ============================================================
# 1. MEMBUAT OBJEK RINGKASAN WILAYAH
# ============================================================

# Objek ringkasan wilayah digunakan untuk merangkum rata-rata
# IPM dan rata-rata kemiskinan pada setiap kabupaten/kota
# selama tahun 2023–2025.

ringkasan_wilayah <- data_clean %>%
  st_drop_geometry() %>%
  filter(
    Tahun %in% c(2023, 2024, 2025),
    !is.na(`Kabupaten/kota`),
    !is.na(Kemiskinan),
    !is.na(IPM)
  ) %>%
  group_by(`Kabupaten/kota`) %>%
  summarise(
    rata_kemiskinan = mean(Kemiskinan),
    rata_IPM = mean(IPM),
    .groups = "drop"
  )


# ============================================================
# 2. MEMBERIKAN KELAS S3
# ============================================================

# Objek ringkasan diberikan kelas khusus "ringkasan_wilayah".
# Kelas ini memungkinkan R mengenali objek sebagai objek
# analisis khusus dan memilih metode S3 yang sesuai.

class(ringkasan_wilayah) <- c(
  "ringkasan_wilayah",
  class(ringkasan_wilayah)
)

# Memeriksa kelas objek
class(ringkasan_wilayah)


# ============================================================
# 3. METODE S3 AUTOPLOT
# ============================================================

# Fungsi autoplot.ringkasan_wilayah() merupakan metode S3
# khusus untuk objek dengan kelas "ringkasan_wilayah".
#
# Input:
#   object : objek dengan kelas "ringkasan_wilayah"
#
# Output:
#   objek ggplot yang menampilkan hubungan antara rata-rata
#   IPM dan rata-rata kemiskinan antar kabupaten/kota.
#
# Fungsi ini dipanggil secara otomatis oleh autoplot()
# ketika objek yang diberikan memiliki kelas
# "ringkasan_wilayah".

autoplot.ringkasan_wilayah <- function(object, ...) {
  
  ggplot(
    object,
    aes(
      x = rata_IPM,
      y = rata_kemiskinan
    )
  ) +
    geom_point(
      size = 3,
      color = "#123B4A"
    ) +
    geom_smooth(
      method = "lm",
      se = TRUE,
      color = "#D95F59",
      fill = "#72B7B2",
      alpha = 0.3
    ) +
    labs(
      title = "Hubungan Rata-rata IPM dan Kemiskinan",
      subtitle = "Kabupaten/Kota Sulawesi Selatan, 2023–2025",
      x = "Rata-rata IPM",
      y = "Rata-rata Kemiskinan (%)",
      caption = "Sumber: BPS Provinsi Sulawesi Selatan | Diolah"
    ) +
    theme_minimal(base_size = 12)
}


# ============================================================
# 4. MENJALANKAN AUTOPLOT DAN MENGUJI S3 DISPATCH
# ============================================================

# Pemanggilan menggunakan fungsi generik autoplot().
# R akan memeriksa kelas objek "ringkasan_wilayah" dan
# secara otomatis menjalankan metode
# autoplot.ringkasan_wilayah() yang telah dibuat.

grafik_s3 <- autoplot(ringkasan_wilayah)

print(grafik_s3)


# ============================================================
# 5. MEMERIKSA METODE S3
# ============================================================

# Menampilkan metode autoplot yang tersedia di R.
# Pastikan autoplot.ringkasan_wilayah tercantum dalam hasil.

methods("autoplot")


# Memeriksa secara langsung metode S3 untuk kelas
# "ringkasan_wilayah".

getS3method(
  "autoplot",
  "ringkasan_wilayah"
)


# ============================================================
# 6. MENYIMPAN GRAFIK
# ============================================================

# Menyimpan hasil visualisasi metode S3 dalam format PNG.

ggsave(
  "H10-11_autoplot_S3.png",
  grafik_s3,
  width = 10,
  height = 7,
  dpi = 300
)
