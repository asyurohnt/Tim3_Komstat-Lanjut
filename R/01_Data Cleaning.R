# ============================================================
# DATA CLEANING DAN DATA SPASIAL
# Kabupaten/Kota Sulawesi Selatan, 2023-2025
# ============================================================


# ============================================================
# 1. LOAD PACKAGE
# ============================================================

library(dplyr)
library(sf)


# ============================================================
# 2. MEMBACA DATA RAW
# ============================================================

# Sesuaikan dengan nama file data raw yang digunakan
data_raw <- read_xlsx(
  "raw.xlsx",)

# ============================================================
# 3. MEMBERSIHKAN DATA
# ============================================================

data_clean <- data_raw %>%
  mutate(
    
    # Menyeragamkan nama kabupaten/kota
    `Kabupaten/kota` = case_when(
      `Kabupaten/kota` == "Pangkep" ~
        "Pangkajene Dan Kepulauan",
      
      `Kabupaten/kota` == "Sidrap" ~
        "Sidenreng Rappang",
      
      `Kabupaten/kota` == "Pare Pare" ~
        "Kota Parepare",
      
      `Kabupaten/kota` == "Palopo" ~
        "Kota Palopo",
      
      `Kabupaten/kota` == "Makassar" ~
        "Kota Makassar",
      
      TRUE ~ `Kabupaten/kota`
    ),
    
    # Mengubah variabel numerik
    Kemiskinan = as.numeric(
      gsub(
        ",",
        ".",
        gsub(
          "[^0-9,.-]",
          "",
          as.character(Kemiskinan)
        )
      )
    ),
    
    IPM = as.numeric(
      gsub(
        ",",
        ".",
        gsub(
          "[^0-9,.-]",
          "",
          as.character(IPM)
        )
      )
    )
  )


# ============================================================
# 4. MEMILIH PERIODE PENELITIAN
# ============================================================

data_clean <- data_clean %>%
  filter(
    Tahun %in% c(2023, 2024, 2025)
  )


# ============================================================
# 5. MENYIMPAN HASIL DATA CLEANING
# ============================================================

saveRDS(
  data_clean,
  "data_clean.rds"
)


# ============================================================
# 6. MEMBACA DATA BATAS WILAYAH
# ============================================================

# Sesuaikan dengan nama file shapefile yang digunakan
peta <- st_read(
  "geoBoundaries-IDN-ADM2_simplified.geojson",
  quiet = TRUE
)


# ============================================================
# 7. DAFTAR WILAYAH SULAWESI SELATAN
# ============================================================

wilayah_sulsel <- c(
  "Kepulauan Selayar",
  "Bulukumba",
  "Bantaeng",
  "Jeneponto",
  "Takalar",
  "Gowa",
  "Sinjai",
  "Maros",
  "Pangkajene Dan Kepulauan",
  "Barru",
  "Bone",
  "Soppeng",
  "Wajo",
  "Sidenreng Rappang",
  "Pinrang",
  "Enrekang",
  "Luwu",
  "Tana Toraja",
  "Luwu Utara",
  "Luwu Timur",
  "Toraja Utara",
  "Kota Makassar",
  "Kota Parepare",
  "Kota Palopo"
)


# ============================================================
# 8. MEMILIH BATAS WILAYAH SULAWESI SELATAN
# ============================================================

peta_sulsel <- peta %>%
  filter(
    shapeName %in% wilayah_sulsel
  ) %>%
  mutate(
    `Kabupaten/kota` = shapeName
  )


# ============================================================
# 9. MENYIAPKAN DATA UNTUK JOIN SPASIAL
# ============================================================

data_df <- data_clean %>%
  select(
    `Kabupaten/kota`,
    Tahun,
    Kemiskinan,
    IPM,
    everything()
  ) %>%
  st_drop_geometry()


# ============================================================
# 10. MENGGABUNGKAN DATA STATISTIK DENGAN BATAS WILAYAH
# ============================================================

data_spasial <- peta_sulsel %>%
  left_join(
    data_df,
    by = "Kabupaten/kota"
  )


# ============================================================
# 11. MENYUSUN DATA
# ============================================================

data_spasial <- data_spasial %>%
  arrange(
    `Kabupaten/kota`,
    Tahun
  )


# ============================================================
# 12. MENYIMPAN DATA SPASIAL
# ============================================================

saveRDS(
  data_spasial,
  "03_Data Clean.rds"
)
