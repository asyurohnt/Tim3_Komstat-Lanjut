# ============================================================
# PROYEK KOMPUTASI STATISTIKA
# DATA CLEANING & QUALITY CHECK
# Kemiskinan dan IPM Sulawesi Selatan, 2023–2025
# ============================================================

# ------------------------------------------------------------
# 1. Load package
# ------------------------------------------------------------
install.packages('writexl')
library(readxl)
library(dplyr)
library(writexl)


# ------------------------------------------------------------
# 2. Membaca data raw
# ------------------------------------------------------------

data_raw <- read_excel(
  "raw.xlsx",
  sheet = "Sheet1"
)

# Melihat data
View(data_raw)


# ------------------------------------------------------------
# 3. Cek struktur data
# ------------------------------------------------------------

str(data_raw)

names(data_raw)

dim(data_raw)


# ------------------------------------------------------------
# 4. Cek tipe setiap variabel
# ------------------------------------------------------------

sapply(data_raw, class)


# ------------------------------------------------------------
# 5. Cek missing value
# ------------------------------------------------------------

colSums(is.na(data_raw))


# ------------------------------------------------------------
# 6. Cek duplikasi
# ------------------------------------------------------------

# Duplikasi seluruh baris
sum(duplicated(data_raw))

# Duplikasi berdasarkan Kabupaten/kota dan Tahun
sum(
  duplicated(
    data_raw[, c("Kabupaten/kota", "Tahun")]
  )
)


# ------------------------------------------------------------
# 7. Cek jumlah kabupaten/kota
# ------------------------------------------------------------

n_distinct(data_raw$`Kabupaten/kota`)

sort(unique(data_raw$`Kabupaten/kota`))


# ------------------------------------------------------------
# 8. Cek tahun
# ------------------------------------------------------------

sort(unique(data_raw$Tahun))

table(data_raw$Tahun)


# ------------------------------------------------------------
# 9. Cek jumlah tahun pada setiap kabupaten/kota
# ------------------------------------------------------------

data_raw %>%
  count(`Kabupaten/kota`) %>%
  arrange(n)


# ------------------------------------------------------------
# 10. Cek nilai Kemiskinan dan IPM
# ------------------------------------------------------------

summary(
  data_raw[, c("Kemiskinan", "IPM")]
)


# Nilai minimum dan maksimum
data_raw %>%
  summarise(
    Kemiskinan_min = min(Kemiskinan, na.rm = TRUE),
    Kemiskinan_max = max(Kemiskinan, na.rm = TRUE),
    IPM_min = min(IPM, na.rm = TRUE),
    IPM_max = max(IPM, na.rm = TRUE)
  )


# ------------------------------------------------------------
# 11. Statistik deskriptif berdasarkan tahun
# ------------------------------------------------------------

data_raw %>%
  group_by(Tahun) %>%
  summarise(
    rata_kemiskinan = mean(Kemiskinan, na.rm = TRUE),
    min_kemiskinan = min(Kemiskinan, na.rm = TRUE),
    max_kemiskinan = max(Kemiskinan, na.rm = TRUE),
    
    rata_ipm = mean(IPM, na.rm = TRUE),
    min_ipm = min(IPM, na.rm = TRUE),
    max_ipm = max(IPM, na.rm = TRUE)
  )


# ------------------------------------------------------------
# 12. Membuat data bersih
# ------------------------------------------------------------

data_clean <- data_raw %>%
  select(
    `Kabupaten/kota`,
    Tahun,
    Kemiskinan,
    IPM
  ) %>%
  arrange(`Kabupaten/kota`, Tahun)


# Melihat data bersih
View(data_clean)


# ------------------------------------------------------------
# 13. Quality check akhir
# ------------------------------------------------------------

cat("Jumlah observasi :", nrow(data_clean), "\n")
cat("Jumlah wilayah   :", n_distinct(data_clean$`Kabupaten/kota`), "\n")
cat("Jumlah tahun     :", n_distinct(data_clean$Tahun), "\n")
cat("Missing value    :", sum(is.na(data_clean)), "\n")
cat(
  "Duplikasi wilayah-tahun :",
  sum(
    duplicated(
      data_clean[, c("Kabupaten/kota", "Tahun")]
    )
  ),
  "\n"
)


# ------------------------------------------------------------
# 14. Menyimpan data bersih
# ------------------------------------------------------------

write_xlsx(
  data_clean,
  "data_clean.xlsx"
)

# ============================================================
# 15. MENAMBAHKAN KODE WILAYAH BPS
# ============================================================

data_clean <- data_clean %>%
  mutate(
    kode_bps = case_when(
      `Kabupaten/kota` == "Kepulauan Selayar" ~ "7301",
      `Kabupaten/kota` == "Bulukumba" ~ "7302",
      `Kabupaten/kota` == "Bantaeng" ~ "7303",
      `Kabupaten/kota` == "Jeneponto" ~ "7304",
      `Kabupaten/kota` == "Takalar" ~ "7305",
      `Kabupaten/kota` == "Gowa" ~ "7306",
      `Kabupaten/kota` == "Sinjai" ~ "7307",
      `Kabupaten/kota` == "Maros" ~ "7308",
      `Kabupaten/kota` == "Pangkajene Dan Kepulauan" ~ "7309",
      `Kabupaten/kota` == "Barru" ~ "7310",
      `Kabupaten/kota` == "Bone" ~ "7311",
      `Kabupaten/kota` == "Soppeng" ~ "7312",
      `Kabupaten/kota` == "Wajo" ~ "7313",
      `Kabupaten/kota` == "Sidenreng Rappang" ~ "7314",
      `Kabupaten/kota` == "Pinrang" ~ "7315",
      `Kabupaten/kota` == "Enrekang" ~ "7316",
      `Kabupaten/kota` == "Luwu" ~ "7317",
      `Kabupaten/kota` == "Tana Toraja" ~ "7318",
      `Kabupaten/kota` == "Luwu Utara" ~ "7322",
      `Kabupaten/kota` == "Luwu Timur" ~ "7325",
      `Kabupaten/kota` == "Toraja Utara" ~ "7326",
      `Kabupaten/kota` == "Kota Makassar" ~ "7371",
      `Kabupaten/kota` == "Kota Parepare" ~ "7372",
      `Kabupaten/kota` == "Kota Palopo" ~ "7373",
      TRUE ~ NA_character_
    )
  )


# Melihat hasil
View(data_clean)

saveRDS(data_clean, "data_clean.rds")

# ============================================================
# SELESAI
# ============================================================


