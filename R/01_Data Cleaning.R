# ============================================================
# PROYEK KOMPUTASI STATISTIKA
# DATA CLEANING, QUALITY CHECK, DAN BATAS WILAYAH
# Kemiskinan dan IPM Sulawesi Selatan, 2023–2025
# ============================================================


# ============================================================
# 1. LOAD PACKAGE
# ============================================================

# Jalankan sekali saja jika package belum tersedia:
# install.packages(c("readxl", "dplyr", "writexl", "sf", "ggplot2"))

library(readxl)
library(dplyr)
library(writexl)
library(sf)
library(ggplot2)


# ============================================================
# 2. MEMBACA DATA RAW
# ============================================================

data_raw <- read_excel(
  "raw.xlsx",
  sheet = "Sheet1"
)

View(data_raw)


# ============================================================
# 3. QUALITY CHECK DATA AWAL
# ============================================================

# Struktur data
str(data_raw)

# Nama kolom
names(data_raw)

# Ukuran data
dim(data_raw)

# Tipe setiap variabel
sapply(data_raw, class)


# ------------------------------------------------------------
# 3.1 Missing value
# ------------------------------------------------------------

colSums(is.na(data_raw))


# ------------------------------------------------------------
# 3.2 Duplikasi seluruh baris
# ------------------------------------------------------------

sum(duplicated(data_raw))


# ------------------------------------------------------------
# 3.3 Duplikasi wilayah dan tahun
# ------------------------------------------------------------

sum(
  duplicated(
    data_raw[, c("Kabupaten/kota", "Tahun")]
  )
)


# ------------------------------------------------------------
# 3.4 Jumlah kabupaten/kota
# ------------------------------------------------------------

n_distinct(
  data_raw$`Kabupaten/kota`
)

sort(
  unique(data_raw$`Kabupaten/kota`)
)


# ------------------------------------------------------------
# 3.5 Tahun
# ------------------------------------------------------------

sort(unique(data_raw$Tahun))

table(data_raw$Tahun)


# ------------------------------------------------------------
# 3.6 Jumlah observasi setiap wilayah
# ------------------------------------------------------------

data_raw %>%
  count(`Kabupaten/kota`) %>%
  arrange(n)


# ------------------------------------------------------------
# 3.7 Statistik Kemiskinan dan IPM
# ------------------------------------------------------------

summary(
  data_raw[, c("Kemiskinan", "IPM")]
)


data_raw %>%
  summarise(
    Kemiskinan_min = min(
      Kemiskinan,
      na.rm = TRUE
    ),
    
    Kemiskinan_max = max(
      Kemiskinan,
      na.rm = TRUE
    ),
    
    IPM_min = min(
      IPM,
      na.rm = TRUE
    ),
    
    IPM_max = max(
      IPM,
      na.rm = TRUE
    )
  )


# ============================================================
# 4. MEMBUAT DATA BERSIH
# ============================================================

data_clean <- data_raw %>%
  select(
    `Kabupaten/kota`,
    Tahun,
    Kemiskinan,
    IPM
  ) %>%
  arrange(
    `Kabupaten/kota`,
    Tahun
  )


# ============================================================
# 5. MENAMBAHKAN KODE WILAYAH BPS
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


# ============================================================
# 6. QUALITY CHECK SETELAH CLEANING
# ============================================================

cat(
  "Jumlah observasi :",
  nrow(data_clean),
  "\n"
)

cat(
  "Jumlah wilayah   :",
  n_distinct(data_clean$`Kabupaten/kota`),
  "\n"
)

cat(
  "Jumlah tahun     :",
  n_distinct(data_clean$Tahun),
  "\n"
)

cat(
  "Missing value    :",
  sum(is.na(data_clean)),
  "\n"
)

cat(
  "Duplikasi wilayah-tahun :",
  sum(
    duplicated(
      data_clean[, c(
        "Kabupaten/kota",
        "Tahun"
      )]
    )
  ),
  "\n"
)


# Cek kode BPS yang kosong
data_clean %>%
  filter(is.na(kode_bps)) %>%
  select(
    `Kabupaten/kota`,
    Tahun,
    kode_bps
  )


# Cek jumlah observasi setiap wilayah
data_clean %>%
  count(`Kabupaten/kota`) %>%
  arrange(n)


# ============================================================
# 7. MEMBACA FILE BATAS WILAYAH GEOBOUNDARIES
# ============================================================

file_peta <- "geoBoundaries-IDN-ADM2_simplified.geojson"

peta <- st_read(
  file_peta,
  quiet = TRUE
)


# Cek struktur peta
names(peta)

dim(peta)

st_crs(peta)


# ============================================================
# 8. MENYIAPKAN NAMA WILAYAH UNTUK JOIN
# ============================================================

# Nama wilayah pada data
data_clean <- data_clean %>%
  mutate(
    nama_wilayah_join = tolower(
      trimws(`Kabupaten/kota`)
    ),
    
    nama_wilayah_join = gsub(
      "^kabupaten[[:space:]]+",
      "",
      nama_wilayah_join
    ),
    
    nama_wilayah_join = gsub(
      "^kota[[:space:]]+",
      "",
      nama_wilayah_join
    )
  )


# Nama wilayah pada GeoJSON
peta <- peta %>%
  mutate(
    nama_wilayah_join = tolower(
      trimws(shapeName)
    ),
    
    nama_wilayah_join = gsub(
      "^kabupaten[[:space:]]+",
      "",
      nama_wilayah_join
    ),
    
    nama_wilayah_join = gsub(
      "^kota[[:space:]]+",
      "",
      nama_wilayah_join
    )
  )


# ============================================================
# 9. DAFTAR WILAYAH SULAWESI SELATAN
# ============================================================

wilayah_sulsel <- c(
  "kepulauan selayar",
  "bulukumba",
  "bantaeng",
  "jeneponto",
  "takalar",
  "gowa",
  "sinjai",
  "maros",
  "pangkajene dan kepulauan",
  "barru",
  "bone",
  "soppeng",
  "wajo",
  "sidenreng rappang",
  "pinrang",
  "enrekang",
  "luwu",
  "tana toraja",
  "luwu utara",
  "luwu timur",
  "toraja utara",
  "makassar",
  "parepare",
  "palopo"
)


# ============================================================
# 10. MENGAMBIL 24 WILAYAH SULAWESI SELATAN
# ============================================================

peta_sulsel <- peta %>%
  filter(
    nama_wilayah_join %in% wilayah_sulsel
  )


# Cek jumlah wilayah
cat(
  "Jumlah wilayah pada peta :",
  nrow(peta_sulsel),
  "\n"
)


# Cek wilayah yang tidak ditemukan
wilayah_tidak_ditemukan <- setdiff(
  wilayah_sulsel,
  peta_sulsel$nama_wilayah_join
)

wilayah_tidak_ditemukan


# ============================================================
# 11. GABUNGKAN DATA CLEAN DENGAN BATAS WILAYAH
# ============================================================

data_spasial <- peta_sulsel %>%
  left_join(
    data_clean,
    by = "nama_wilayah_join"
  )


# ============================================================
# 12. QUALITY CHECK DATA SPASIAL
# ============================================================

# Jumlah baris
cat(
  "Jumlah baris data spasial :",
  nrow(data_spasial),
  "\n"
)


# Jumlah wilayah
cat(
  "Jumlah wilayah :",
  n_distinct(
    data_spasial$`Kabupaten/kota`
  ),
  "\n"
)


# Jumlah tahun
cat(
  "Jumlah tahun :",
  n_distinct(data_spasial$Tahun),
  "\n"
)


# ------------------------------------------------------------
# 12.1 Cek data yang gagal bergabung
# ------------------------------------------------------------

data_spasial %>%
  filter(
    is.na(Kemiskinan) |
      is.na(IPM)
  ) %>%
  select(
    shapeName,
    `Kabupaten/kota`,
    Tahun,
    Kemiskinan,
    IPM
  )


# ------------------------------------------------------------
# 12.2 Cek missing value
# ------------------------------------------------------------

colSums(
  is.na(
    st_drop_geometry(data_spasial)
  )
)


# ------------------------------------------------------------
# 12.3 Cek duplikasi wilayah-tahun
# ------------------------------------------------------------

data_spasial %>%
  st_drop_geometry() %>%
  count(
    `Kabupaten/kota`,
    Tahun
  ) %>%
  filter(n > 1)


# ------------------------------------------------------------
# 12.4 Melihat data akhir
# ------------------------------------------------------------

View(data_spasial)


# ============================================================
# 13. MENYIMPAN OUTPUT
# ============================================================

# Versi tabel biasa
write_xlsx(
  st_drop_geometry(data_spasial),
  "data_clean.xlsx"
)


# Versi utama:
# data + geometry/batas wilayah
saveRDS(
  data_spasial,
  "data_clean.rds"
)


# ============================================================
# 14. VISUALISASI PETA SEDERHANA
# ============================================================

peta_2025 <- data_spasial %>%
  filter(Tahun == 2025)


ggplot(peta_2025) +
  geom_sf(
    aes(fill = Kemiskinan),
    color = "black",
    linewidth = 0.2
  ) +
  labs(
    title = "Persentase Penduduk Miskin",
    subtitle = "Kabupaten/Kota di Sulawesi Selatan, 2025",
    fill = "Kemiskinan (%)"
  ) +
  theme_minimal()


# ============================================================
# SELESAI
# ============================================================
