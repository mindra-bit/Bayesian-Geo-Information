# ============================================
# SYNTAX LENGKAP TESIS (FINAL - REVISI TOTAL)
# Pemodelan Bayesian INLA–SPDE untuk Simulasi 
# Probabilistik Dampak Gempa Sesar Lembang
# Pendekatan: Bayesian Hierarchical Spatio-Temporal
# ============================================
# TIDAK ADA OUTPUT YANG DISIMPAN KE DISK
# ============================================

# ============================================
# MODUL 00: LIBRARY & SETUP AWAL
# ============================================
cat("\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("  MODUL 00: LIBRARY & SETUP AWAL\n")
cat("═══════════════════════════════════════════════════════════\n\n")

library(sf)
library(dplyr)
library(tidyr)
library(zoo)
library(stats)
library(psych)
library(INLA)
library(ggplot2)
library(viridis)
library(patchwork)
library(gridExtra)
library(purrr)
library(ggspatial)
library(ggrepel)
library(knitr)

cat("✅ Library dimuat\n\n")

# ═══════════════════════════════════════════
# KONFIGURASI GLOBAL
# ═══════════════════════════════════════════
CFG <- list()
CFG$tahun_hazard_start <- 2009
CFG$tahun_hazard_end   <- 2024
CFG$tahun_exp_start    <- 2019
CFG$tahun_exp_end      <- 2025
CFG$tahun_exp_seq      <- CFG$tahun_exp_start:CFG$tahun_exp_end
CFG$n_tahun            <- length(CFG$tahun_exp_seq)

CFG$gmpe <- list(
  c0 = -0.53804, c1 = 0.59850, c2 = -0.01218,
  c3 = -1.30709, c4 = 0.10648,
  Mh = 6.0, Rref = 1.0,
  c7 = -0.36, Vref = 760, sigma = 0.55,
  V1 = 225, V2 = 300, b1 = -0.7, b2 = -0.1
)

CFG$n_eff          <- 3
CFG$pc_rho0_km     <- 20
CFG$pc_sigma0      <- 1.5
CFG$pc_rho0_prob   <- 0.05
CFG$pc_sigma0_prob <- 0.05
CFG$mesh_inner_pct  <- 0.03
CFG$mesh_outer_pct  <- 0.08
CFG$mesh_cutoff_pct <- 0.005
CFG$threshold_MMI <- list(VI  = 0.092, VII = 0.180, VIII = 0.340)
# Skenario gempa Sesar Lembang
CFG$scenario_Mw <- 6.8
# Threshold wilayah terdampak minimum
# MMI VI = PGA 0.092 g
CFG$scenario_PGA_threshold <- CFG$threshold_MMI$VI
# Threshold hazard untuk model Bayesian
CFG$hazard_MMI_threshold <- CFG$threshold_MMI$VI
CFG$data_dir <- "D:/Kuliah/S2/~ TESIS ~/Data/DATA FIX"
CFG$sesar_kmz <- "titik_sesar_lembang.kmz"
CFG$R_earth_km <- 6371

# Bobot Risk (original dari naskah akademik, tanpa komponen jalan)
CFG$w_hazard <- 0.60
CFG$w_exposure <- 0.30
CFG$w_vulnerability <- 0.10

set.seed(2025)

# ============================================
# FUNGSI BANTU
# ============================================
read_fault_from_kmz <- function(kmz_path) {
  tmp_dir <- tempfile(); dir.create(tmp_dir)
  unzip(kmz_path, exdir = tmp_dir)
  kml_file <- list.files(tmp_dir, pattern = "\\.kml$", full.names = TRUE)
  if(length(kml_file) == 0) { cat("⚠️ Tidak ada file KML\n"); return(NULL) }
  fault_sf <- st_read(kml_file[1], quiet = TRUE)
  unlink(tmp_dir, recursive = TRUE)
  return(fault_sf)
}

haversine_distance <- function(lat1, lon1, lat2, lon2) {
  lat1 <- lat1 * pi / 180; lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180; lon2 <- lon2 * pi / 180
  dlat <- lat2 - lat1; dlon <- lon2 - lon1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  return(CFG$R_earth_km * c)
}

# ------------------------------------------------------------
# Jarak 3D dari hiposenter ke titik terdekat pada Sesar Lembang
# ------------------------------------------------------------

distance_hypocenter_to_fault <- function(
    lat_eq,
    lon_eq,
    depth_km,
    fault_lat,
    fault_lon,
    R_earth = 6371
) {
  
  # Ubah derajat ke radian
  lat1 <- lat_eq * pi / 180
  lon1 <- lon_eq * pi / 180
  lat2 <- fault_lat * pi / 180
  lon2 <- fault_lon * pi / 180
  
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  # Surface arc distance / great-circle distance
  a <- sin(dlat / 2)^2 +
    cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  
  Repi <- 2 * R_earth * asin(sqrt(a))
  
  # Horizontal chord distance / R_JB proxy
  RJB <- 2 * R_earth * sin(Repi / (2 * R_earth))
  
  # 3D hypocentral distance
  Ri <- sqrt(RJB^2 + depth_km^2)
  
  list(
    Repi = Repi,
    RJB = RJB,
    Ri = Ri
  )
}

convert_to_Mw <- function(M, mag_type) {
  Mw <- rep(NA, length(M))
  for(i in 1:length(M)) {
    mt <- toupper(trimws(mag_type[i])); m <- M[i]
    if(is.na(m) | is.na(mt)) next
    if(mt == "MW") Mw[i] <- m
    else if(mt == "M") Mw[i] <- 1.0166 * m - 0.2207
    else if(mt == "MB") Mw[i] <- if(m <= 6.2) 0.933 * m + 0.52 else 1.273 * m - 1.2
    else if(mt %in% c("MLV","ML")) Mw[i] <- 0.756 * m + 1.060
    else if(mt == "MW(MB)") Mw[i] <- 0.898 * m + 0.597
    else Mw[i] <- m
  }
  return(Mw)
}

calculate_distance_to_fault <- function(points_sf, fault_sf) {
  dist_matrix <- st_distance(points_sf, fault_sf)
  return(as.numeric(apply(dist_matrix, 1, min)) / 1000)
}

# ============================================
# MODUL 01: PREPROCESSING SPASIAL
# ============================================
cat("═══════════════════════════════════════════════════════════\n")
cat("  MODUL 01: PREPROCESSING SPASIAL\n")
cat("═══════════════════════════════════════════════════════════\n\n")

setwd(CFG$data_dir)

cat(">>> 1.1 Load shapefile...\n")
desa_raw <- st_read("Batas_Desa_Kelurahan_JABAR.GPKG", layer="Batas_Desa_Kelurahan_JABAR", quiet=TRUE)
kode_target <- c("32.04","32.17","32.03","32.05","32.14","32.13","32.11","32.73","32.77")
desa_filtered <- desa_raw %>% filter(kd_kab %in% kode_target) %>% st_make_valid()

cat(">>> 1.2 Agregasi kecamatan & kabupaten...\n")
sf_use_s2(FALSE)
desa_filtered <- desa_filtered %>% st_zm(drop=TRUE, what="ZM") %>% st_make_valid()

kecamatan_sf <- desa_filtered %>% group_by(kabupaten_kota, kecamatan) %>%
  summarise(geom=st_union(geom), .groups="drop") %>% st_make_valid() %>%
  rename(geometry=geom) %>% st_set_geometry("geometry")
kecamatan_sf$luas_km2 <- as.numeric(st_area(kecamatan_sf$geometry)) / 1e6
kecamatan_sf$ID_Kec <- 1:nrow(kecamatan_sf)

kabkota_sf <- desa_filtered %>% group_by(kabupaten_kota) %>%
  summarise(geom=st_union(geom), .groups="drop") %>% st_make_valid() %>%
  rename(geometry=geom) %>% st_set_geometry("geometry")
kabkota_sf$kabupaten_kota_label <- ifelse(grepl("^Kota", kabkota_sf$kabupaten_kota),
                                          kabkota_sf$kabupaten_kota, paste("Kab.", kabkota_sf$kabupaten_kota))
sf_use_s2(TRUE)

lookup_kec <- kecamatan_sf %>% st_drop_geometry() %>%
  dplyr::select(ID_Kec, kabupaten=kabupaten_kota, kecamatan)

cat(sprintf("  Kecamatan: %d, Kab/Kota: %d\n", nrow(kecamatan_sf), nrow(kabkota_sf)))

cat(">>> 1.3 Sesar Lembang...\n")
sesar_raw <- read_fault_from_kmz(CFG$sesar_kmz)
if(is.null(sesar_raw)) {
  sesar_raw <- st_linestring(cbind(c(107.4758,107.7341), c(-6.8502,-6.8024))) %>%
    st_sfc(crs=4326) %>% st_as_sf()
}
sesar_sf <- st_transform(sesar_raw, crs=st_crs(kecamatan_sf))

cat(">>> 1.4 Jarak kecamatan ke Sesar Lembang...\n")
dist_kec_fault_km <- calculate_distance_to_fault(
  st_centroid(kecamatan_sf),
  sesar_sf
)
kecamatan_sf$Distance_to_fault_km <- dist_kec_fault_km

cat(">>> 1.5 Vs30...\n")
vs30_raw <- read.csv("data_vs30.csv")
vs30_points <- vs30_raw %>% filter(!is.na(longitude) & !is.na(latitude)) %>%
  st_as_sf(coords=c("longitude","latitude"), crs=4326) %>% st_transform(crs=st_crs(kecamatan_sf))
vs30_kec <- st_join(vs30_points, kecamatan_sf, join=st_within) %>% st_drop_geometry() %>%
  filter(!is.na(ID_Kec)) %>% group_by(ID_Kec) %>%
  summarise(Vs30_mean=mean(vs30_mosaic,na.rm=TRUE), Vs30_sd=sd(vs30_mosaic,na.rm=TRUE), n_pengukuran=n(), .groups="drop")
kecamatan_sf <- kecamatan_sf %>% left_join(vs30_kec, by="ID_Kec")

cat(">>> 1.6 Katalog gempa (2009-2024)...\n")
gempa_raw <- read.csv("data_gempa.csv")
gempa_raw$Mw <- convert_to_Mw(gempa_raw$Magnitude, gempa_raw$Mag.Type)
gempa_sf <- gempa_raw %>% filter(Tahun >= CFG$tahun_hazard_start & Tahun <= CFG$tahun_hazard_end) %>%
  st_as_sf(coords=c("Longitude","Latitude"), crs=4326) %>% st_transform(crs=st_crs(kecamatan_sf))

# ------------------------------------------------------------
# Distance earthquake source to nearest point on Lembang Fault
# Repi -> RJB -> Ri
# ------------------------------------------------------------

sesar_coords <- st_coordinates(
  st_transform(sesar_sf, 4326)
)

gempa_coords <- st_coordinates(
  st_transform(gempa_sf, 4326)
)

depth_col <- grep(
  "[Dd]epth",
  names(gempa_sf),
  value = TRUE
)[1]

dist_gempa_repi <- numeric(nrow(gempa_sf))
dist_gempa_rjb  <- numeric(nrow(gempa_sf))
dist_gempa_ri   <- numeric(nrow(gempa_sf))

for (i in seq_len(nrow(gempa_sf))) {
  
  eq_lat <- gempa_coords[i, "Y"]
  eq_lon <- gempa_coords[i, "X"]
  eq_depth <- gempa_sf[[depth_col]][i]
  
  dist_list <- distance_hypocenter_to_fault(
    lat_eq = eq_lat,
    lon_eq = eq_lon,
    depth_km = eq_depth,
    fault_lat = sesar_coords[, "Y"],
    fault_lon = sesar_coords[, "X"]
  )
  
  # Titik fault terdekat berdasarkan RJB
  idx_nearest <- which.min(
    dist_list$RJB
  )
  
  dist_gempa_repi[i] <- dist_list$Repi[idx_nearest]
  dist_gempa_rjb[i]  <- dist_list$RJB[idx_nearest]
  dist_gempa_ri[i]   <- dist_list$Ri[idx_nearest]
}

gempa_sf$Distance_surface_km <- dist_gempa_repi
gempa_sf$Distance_RJB_km <- dist_gempa_rjb
gempa_sf$Distance_to_fault_km <- dist_gempa_ri

cat(">>> 1.7 Load panel exposure-vulnerability MENTAH...\n")
panel_raw <- read.csv("data_exposure_vulnerability.csv")
if("Distance_km" %in% names(panel_raw)) panel_raw$Distance_km <- NULL

panel_raw <- panel_raw %>% 
  filter(Tahun >= CFG$tahun_exp_start & Tahun <= CFG$tahun_exp_end) %>%
  mutate(
    kabupaten = case_when(
      Kab.Kota == "Kab. Bandung" ~ "Bandung", Kab.Kota == "Kab. Bandung Barat" ~ "Bandung Barat",
      Kab.Kota == "Kab. Cianjur" ~ "Cianjur", Kab.Kota == "Kab. Garut" ~ "Garut",
      Kab.Kota == "Kab. Purwakarta" ~ "Purwakarta", Kab.Kota == "Kab. Subang" ~ "Subang",
      Kab.Kota == "Kab. Sumedang" ~ "Sumedang", Kab.Kota == "Kota Bandung" ~ "Kota Bandung",
      Kab.Kota == "Kota Cimahi" ~ "Kota Cimahi", TRUE ~ Kab.Kota
    ),
    kecamatan = case_when(
      Kecamatan == "Cikalong Wetan" ~ "Cikalongwetan", Kecamatan == "Blubur Limbangan" ~ "Bl. Limbangan",
      Kecamatan == "Pondok Salam" ~ "Pondoksalam", Kecamatan == "Tegal Waru" ~ "Tegalwaru",
      Kecamatan == "Ujung Jaya" ~ "Ujungjaya", Kecamatan == "Ujung Berung" ~ "Ujungberung",
      Kecamatan == "Astanaanyar" ~ "Astana Anyar", Kecamatan == "Solokan Jeruk" ~ "Solokanjeruk",
      TRUE ~ Kecamatan
    )
  )

panel_clean <- panel_raw %>% left_join(lookup_kec, by=c("kabupaten","kecamatan"))
cat("✅ MODUL 01 SELESAI\n\n")

# ============================================
# MODUL 02: PREPROCESSING EXPOSURE & VULNERABILITY (REVISI)
# ============================================
cat("═══════════════════════════════════════════════════════════\n")
cat("  MODUL 02: PREPROCESSING EXPOSURE & VULNERABILITY\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# STEP A: SALIN DATA ASLI & BUAT RAW BACKUP
cat(">>> STEP A: DATA VALIDATION – RAW BACKUP\n\n")
panel_validated <- panel_clean
# Simpan nilai asli sebelum perubahan apapun
panel_validated <- panel_validated %>%
  mutate(
    Kepadatan_raw = Kepadatan,
    Faskes_raw    = Faskes,
    Pendidikan_raw = Pendidikan,
    Rasio_raw     = Rasio,
    Sinyal_raw    = Sinyal
  )

# A.1 Missing per variabel per tahun
cat("A.1 Missing values per tahun:\n")
missing_summary <- panel_validated %>% group_by(Tahun) %>%
  summarise(n=n(), miss_K=sum(is.na(Kepadatan)), miss_F=sum(is.na(Faskes)),
            miss_P=sum(is.na(Pendidikan)), miss_R=sum(is.na(Rasio)), miss_S=sum(is.na(Sinyal)), .groups="drop")
print(kable(missing_summary, format="simple"))

# A.2 Sinyal – deteksi tahun dengan seluruh data missing
cat("\nA.2 Sinyal – missing total per tahun:\n")
sc <- panel_validated %>% group_by(Tahun) %>% summarise(t=n(), m=sum(is.na(Sinyal)), pct=100*m/t, .groups="drop")
print(kable(sc, format="simple"))
sinyal_all_missing_years <- sc$Tahun[sc$pct==100]
if(length(sinyal_all_missing_years)>0) cat(sprintf("  ⚠️ Sinyal missing total: %s\n", paste(sinyal_all_missing_years,collapse=", ")))

# ============================================
# STEP B: DETEKSI OUTLIER (IQR PER TAHUN) + TEMPORAL CONSISTENCY
# ============================================
cat("\n>>> STEP B: DETEKSI OUTLIER & TEMPORAL FLAGGING\n")

# Fungsi deteksi outlier IQR per tahun
flag_outlier_iqr_by_year <- function(var_vec, year_vec) {
  result <- rep("OK", length(var_vec))
  years <- unique(year_vec)
  for(yr in years) {
    idx <- which(year_vec == yr)
    x <- var_vec[idx]
    if(all(is.na(x))) next
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR_val
    upper <- Q3 + 1.5 * IQR_val
    is_out <- x < lower | x > upper
    is_out[is.na(is_out)] <- FALSE
    result[idx[is_out]] <- "IQR_OUTLIER"
  }
  return(result)
}

# Deteksi outlier IQR untuk setiap variabel (per tahun)
panel_validated$flag_Kepadatan_IQR <- flag_outlier_iqr_by_year(panel_validated$Kepadatan, panel_validated$Tahun)
panel_validated$flag_Faskes_IQR    <- flag_outlier_iqr_by_year(panel_validated$Faskes, panel_validated$Tahun)
panel_validated$flag_Pendidikan_IQR <- flag_outlier_iqr_by_year(panel_validated$Pendidikan, panel_validated$Tahun)
panel_validated$flag_Rasio_IQR     <- flag_outlier_iqr_by_year(panel_validated$Rasio, panel_validated$Tahun)
panel_validated$flag_Sinyal_IQR    <- flag_outlier_iqr_by_year(panel_validated$Sinyal, panel_validated$Tahun)

# Fungsi temporal consistency check (per kecamatan, deteksi perubahan drastis antar tahun)
flag_temporal_jump <- function(df, var_name) {
  df <- df %>% arrange(Tahun)
  n <- nrow(df)
  vals <- df[[var_name]]
  flags <- rep("OK", n)
  for(i in 2:n) {
    if(is.na(vals[i]) | is.na(vals[i-1])) next
    # Perubahan relatif > 300% dianggap mencurigakan (dapat disesuaikan)
    if(vals[i-1] > 0 && abs(vals[i] - vals[i-1]) / vals[i-1] > 3) {
      flags[i] <- "TEMPORAL_JUMP"
      flags[i-1] <- "TEMPORAL_JUMP"
    }
  }
  return(flags)
}

# Terapkan temporal flag untuk setiap variabel (per kecamatan)
temp_flags_K <- panel_validated %>%
  group_by(ID_Kec) %>%
  group_modify(~ data.frame(Tahun = .x$Tahun, flag_Kepadatan_temporal = flag_temporal_jump(.x, "Kepadatan"))) %>%
  ungroup()
temp_flags_F <- panel_validated %>%
  group_by(ID_Kec) %>%
  group_modify(~ data.frame(Tahun = .x$Tahun, flag_Faskes_temporal = flag_temporal_jump(.x, "Faskes"))) %>%
  ungroup()
temp_flags_P <- panel_validated %>%
  group_by(ID_Kec) %>%
  group_modify(~ data.frame(Tahun = .x$Tahun, flag_Pendidikan_temporal = flag_temporal_jump(.x, "Pendidikan"))) %>%
  ungroup()
temp_flags_R <- panel_validated %>%
  group_by(ID_Kec) %>%
  group_modify(~ data.frame(Tahun = .x$Tahun, flag_Rasio_temporal = flag_temporal_jump(.x, "Rasio"))) %>%
  ungroup()
temp_flags_S <- panel_validated %>%
  group_by(ID_Kec) %>%
  group_modify(~ data.frame(Tahun = .x$Tahun, flag_Sinyal_temporal = flag_temporal_jump(.x, "Sinyal"))) %>%
  ungroup()

# Gabungkan flag temporal
panel_validated <- panel_validated %>%
  left_join(temp_flags_K, by = c("ID_Kec","Tahun")) %>%
  left_join(temp_flags_F, by = c("ID_Kec","Tahun")) %>%
  left_join(temp_flags_P, by = c("ID_Kec","Tahun")) %>%
  left_join(temp_flags_R, by = c("ID_Kec","Tahun")) %>%
  left_join(temp_flags_S, by = c("ID_Kec","Tahun"))

# Hapus flag density temporal yang lama (tidak dipakai lagi)
panel_validated$flag_density_suspect <- NULL

cat("Outlier & temporal flags added.\n")

# ============================================
# STEP C: KLASIFIKASI OUTLIER & SUSPECTED ERROR (TIDAK MENGHAPUS NILAI)
# ============================================
cat("\n>>> STEP C: KLASIFIKASI OUTLIER\n")
# Buat kolom klasifikasi untuk setiap observasi yang terdeteksi outlier/temporal jump
# Secara default diisi "VALID_EXTREME" jika hanya IQR, "POSSIBLE_DATA_ERROR" jika temporal jump,
# dapat diperhalus di kemudian hari.
panel_validated <- panel_validated %>%
  mutate(
    outlier_class_K = case_when(
      flag_Kepadatan_IQR == "IQR_OUTLIER" & flag_Kepadatan_temporal == "TEMPORAL_JUMP" ~ "POSSIBLE_DATA_ERROR",
      flag_Kepadatan_IQR == "IQR_OUTLIER" ~ "VALID_EXTREME",
      TRUE ~ "OK"
    ),
    outlier_class_F = case_when(
      flag_Faskes_IQR == "IQR_OUTLIER" & flag_Faskes_temporal == "TEMPORAL_JUMP" ~ "POSSIBLE_DATA_ERROR",
      flag_Faskes_IQR == "IQR_OUTLIER" ~ "VALID_EXTREME",
      TRUE ~ "OK"
    ),
    outlier_class_P = case_when(
      flag_Pendidikan_IQR == "IQR_OUTLIER" & flag_Pendidikan_temporal == "TEMPORAL_JUMP" ~ "POSSIBLE_DATA_ERROR",
      flag_Pendidikan_IQR == "IQR_OUTLIER" ~ "VALID_EXTREME",
      TRUE ~ "OK"
    ),
    outlier_class_R = case_when(
      flag_Rasio_IQR == "IQR_OUTLIER" & flag_Rasio_temporal == "TEMPORAL_JUMP" ~ "POSSIBLE_DATA_ERROR",
      flag_Rasio_IQR == "IQR_OUTLIER" ~ "VALID_EXTREME",
      TRUE ~ "OK"
    ),
    outlier_class_S = case_when(
      flag_Sinyal_IQR == "IQR_OUTLIER" & flag_Sinyal_temporal == "TEMPORAL_JUMP" ~ "POSSIBLE_DATA_ERROR",
      flag_Sinyal_IQR == "IQR_OUTLIER" ~ "VALID_EXTREME",
      TRUE ~ "OK"
    )
  )

# ═══════════════════════════════════════════
# STEP D: PENANGANAN MISSING – AUDIT STRUKTURAL SEBELUM IMPUTASI
# ═══════════════════════════════════════════
cat("\n>>> STEP D: AUDIT STRUKTURAL MISSING\n")

# Rasio: periksa jumlah observed per kecamatan
rasio_obs_per_kec <- panel_validated %>%
  group_by(ID_Kec) %>%
  summarise(n_obs = sum(!is.na(Rasio)), .groups = "drop")
cat("Rasio – kecamatan dengan 0 observasi:\n")
print(rasio_obs_per_kec %>% filter(n_obs == 0) %>% nrow())
cat("Rasio – kecamatan dengan 1-2 observasi:\n")
print(rasio_obs_per_kec %>% filter(n_obs <= 2) %>% nrow())

# Sinyal: sudah ditangani dengan sinyal_all_missing_years, 
# tetapi untuk tahun 2024-2025 periksa missing per kecamatan
sinyal_obs_2024 <- panel_validated %>% filter(Tahun == 2024) %>% summarise(miss = sum(is.na(Sinyal)), n=n())
cat(sprintf("Sinyal 2024: missing %d dari %d\n", sinyal_obs_2024$miss, sinyal_obs_2024$n))

# ═══════════════════════════════════════════
# STEP E: IMPUTASI (TANPA FALLBACK MEDIAN, SINYAL TIDAK DIIMPUTASI)
# ═══════════════════════════════════════════
cat("\n>>> STEP E: IMPUTASI MISSING (TANPA FALLBACK MEDIAN)\n")

# Fungsi imputasi temporal yang hanya menangani internal gap
impute_temporal_safe <- function(x) {
  n <- length(x)
  res <- x
  method <- rep("observed", n)
  non_na <- which(!is.na(x))
  
  if(length(non_na) == 0) {
    # Semua missing: jangan diimputasi, tandai unresolved
    method <- rep("unresolved", n)
    return(list(val = x, met = method))
  }
  
  for(i in 1:n) {
    if(is.na(x[i])) {
      dist_to <- abs(i - non_na)
      nearest <- non_na[order(dist_to)]
      
      if(length(nearest) >= 2) {
        before <- max(nearest[nearest < i], na.rm = TRUE)
        after  <- min(nearest[nearest > i], na.rm = TRUE)
        if(!is.infinite(before) && !is.infinite(after)) {
          frac <- (i - before) / (after - before)
          res[i] <- x[before] + frac * (x[after] - x[before])
          method[i] <- "linear_interpolation"
        } else if(!is.infinite(before)) {
          res[i] <- x[before]
          method[i] <- "LOCF"
        } else {
          res[i] <- x[after]
          method[i] <- "NOCB"
        }
      } else if(length(nearest) == 1) {
        res[i] <- x[nearest[1]]
        method[i] <- ifelse(nearest[1] < i, "LOCF", "NOCB")
      }
    }
  }
  
  # JANGAN fallback median
  return(list(val = res, met = method))
}

# Salin data tervalidasi sebagai dataset yang akan diproses
panel_imputed <- panel_validated

# Inisialisasi metode imputasi
for(var in c("Kepadatan","Faskes","Pendidikan","Rasio","Sinyal")) {
  panel_imputed[[paste0("imp_",var)]] <- "observed"
}

# Terapkan imputasi untuk Kepadatan, Faskes, Pendidikan
for(var in c("Kepadatan","Faskes","Pendidikan")) {
  cat(sprintf("  %s...\n", var))
  imp_list <- panel_imputed %>%
    group_by(ID_Kec) %>%
    group_modify(~ {
      res <- impute_temporal_safe(.x[[var]])
      data.frame(Tahun = .x$Tahun, temp_val = res$val, temp_met = res$met)
    }) %>%
    ungroup()
  
  panel_imputed <- panel_imputed %>%
    left_join(imp_list, by = c("ID_Kec","Tahun")) %>%
    mutate(
      !!var := ifelse(is.na(.data[[var]]), temp_val, .data[[var]]),
      !!paste0("imp_",var) := ifelse(.data[[paste0("imp_",var)]] == "observed",
                                     temp_met, .data[[paste0("imp_",var)]])
    ) %>%
    dplyr::select(-temp_val, -temp_met)
}

# ============================================================
# RASIO USIA RENTAN
# 1. Temporal imputation jika tersedia data
# 2. Spatial imputation untuk kecamatan yang tetap missing
# ============================================================

cat("  Rasio: temporal + spatial imputation...\n")

# ------------------------------------------------------------
# STEP 1: temporal imputation
# ------------------------------------------------------------

imp_rasio <- panel_imputed %>%
  group_by(ID_Kec) %>%
  group_modify(~ {
    res <- impute_temporal_safe(.x$Rasio)
    
    data.frame(
      Tahun = .x$Tahun,
      temp_val = res$val,
      temp_met = res$met
    )
  }) %>%
  ungroup()

panel_imputed <- panel_imputed %>%
  left_join(
    imp_rasio,
    by = c("ID_Kec", "Tahun")
  ) %>%
  mutate(
    Rasio = ifelse(
      is.na(Rasio) & !is.na(temp_val),
      temp_val,
      Rasio
    ),
    imp_Rasio = ifelse(
      is.na(Rasio) == FALSE & imp_Rasio == "observed",
      ifelse(
        !is.na(temp_val),
        temp_met,
        "observed"
      ),
      imp_Rasio
    )
  ) %>%
  select(-temp_val, -temp_met)

# ============================================================
# SPATIAL IMPUTATION RASIO
# ============================================================

cat("  Spatial imputation Rasio...\n")

# Data Rasio baseline 2023
rasio_2023 <- panel_imputed %>%
  left_join(
    rasio_2023 %>%
      select(ID_Kec, Rasio_2023 = Rasio_imputed),
    by = "ID_Kec"
  ) %>%
  mutate(
    Rasio = ifelse(
      is.na(Rasio),
      Rasio_2023,
      Rasio
    )
  ) %>%
  select(-Rasio_2023)

# Koordinat centroid kecamatan
kec_cent <- st_centroid(kecamatan_sf)

coords <- st_coordinates(kec_cent)

# Fungsi spatial weighted nearest neighbours
spatial_impute <- function(target_id, values, coords, k = 5) {
  
  target_idx <- which(kecamatan_sf$ID_Kec == target_id)
  
  valid_idx <- which(!is.na(values))
  
  if(length(valid_idx) == 0) {
    return(NA_real_)
  }
  
  d <- sqrt(
    (coords[target_idx,1] - coords[valid_idx,1])^2 +
      (coords[target_idx,2] - coords[valid_idx,2])^2
  )
  
  # ambil k tetangga terdekat
  nn <- valid_idx[order(d)[1:min(k, length(d))]]
  
  d_nn <- d[order(d)[1:min(k, length(d))]]
  
  # hindari pembagian nol
  d_nn[d_nn < 1] <- 1
  
  w <- 1 / d_nn
  w <- w / sum(w)
  
  sum(values[nn] * w, na.rm = TRUE)
}

# Nilai Rasio 2023
rasio_values <- rasio_2023$Rasio

missing_rasio <- which(is.na(rasio_values))

for(i in missing_rasio) {
  
  target_id <- rasio_2023$ID_Kec[i]
  
  rasio_values[i] <- spatial_impute(
    target_id = target_id,
    values = rasio_values,
    coords = coords,
    k = 5
  )
}

# Masukkan kembali ke panel
rasio_2023$Rasio_imputed <- rasio_values

panel_imputed <- panel_imputed %>%
  select(-Rasio) %>%
  left_join(
    rasio_2023 %>% select(ID_Kec, Rasio = Rasio_imputed),
    by = "ID_Kec"
  )

# ------------------------------------------------------------
# SINYAL
# 2019–2023 = historical proxy dari data 2024
# 2024–2025 = observed
# ------------------------------------------------------------

cat("  Sinyal: 2019–2023 menggunakan historical proxy dari 2024...\n")

# Ambil nilai Sinyal 2024 per kecamatan
sinyal_2024 <- panel_imputed %>%
  filter(Tahun == 2024) %>%
  select(ID_Kec, Sinyal_2024 = Sinyal)

# Isi seluruh tahun 2019–2023 dengan nilai 2024
panel_imputed <- panel_imputed %>%
  left_join(sinyal_2024, by = "ID_Kec") %>%
  mutate(
    Sinyal = ifelse(
      Tahun %in% 2019:2023 & is.na(Sinyal),
      Sinyal_2024,
      Sinyal
    ),
    imp_Sinyal = case_when(
      Tahun %in% 2019:2023 & !is.na(Sinyal_2024) ~ "historical_proxy",
      Tahun %in% 2024:2025 ~ "observed",
      TRUE ~ imp_Sinyal
    )
  ) %>%
  select(-Sinyal_2024)

# ============================================
# STEP F: TRANSFORMASI
# ============================================

cat("\n>>> STEP F: TRANSFORMASI\n")

panel_transformed <- panel_imputed

# Exposure
for(var in c("Kepadatan","Faskes","Pendidikan")) {
  sk <- psych::skew(
    panel_transformed %>% filter(Tahun == 2023) %>% pull(.data[[var]]),
    na.rm = TRUE
  )
  
  cat(sprintf(
    "  %s: skewness = %.2f %s\n",
    var, sk,
    ifelse(abs(sk) > 1, "→ log1p", "→ OK")
  ))
}

panel_transformed <- panel_transformed %>%
  mutate(
    log_Kepadatan  = log1p(Kepadatan),
    log_Faskes     = log1p(Faskes),
    log_Pendidikan = log1p(Pendidikan)
  )

# Sinyal
sk_sinyal <- psych::skew(
  panel_transformed %>%
    filter(Tahun == 2023) %>%
    pull(Sinyal),
  na.rm = TRUE
)

cat(sprintf("  Sinyal: skewness = %.2f\n", sk_sinyal))

if(abs(sk_sinyal) > 1) {
  panel_transformed <- panel_transformed %>%
    mutate(log_Sinyal = log1p(Sinyal))
} else {
  panel_transformed <- panel_transformed %>%
    mutate(log_Sinyal = Sinyal)
}

# ═══════════════════════════════════════════
# STEP G: STANDARDISASI (MENGGUNAKAN BASELINE 2023 YANG SUDAH LENGKAP)
# ═══════════════════════════════════════════
cat("\n>>> STEP G: STANDARDISASI\n")
bl <- panel_transformed %>% filter(Tahun >= 2019, Tahun <= 2025)

# Hitung mean dan sd untuk setiap variabel (log version)
ms <- list(
  m_K = mean(bl$log_Kepadatan, na.rm = TRUE),   sd_K = sd(bl$log_Kepadatan, na.rm = TRUE),
  m_F = mean(bl$log_Faskes, na.rm = TRUE),      sd_F = sd(bl$log_Faskes, na.rm = TRUE),
  m_P = mean(bl$log_Pendidikan, na.rm = TRUE),   sd_P = sd(bl$log_Pendidikan, na.rm = TRUE),
  m_R = mean(bl$Rasio, na.rm = TRUE),            sd_R = sd(bl$Rasio, na.rm = TRUE),
  m_S = mean(bl$log_Sinyal, na.rm = TRUE),       sd_S = sd(bl$log_Sinyal, na.rm = TRUE)
)

panel_std <- panel_transformed %>%
  mutate(
    z_Kepadatan   = (log_Kepadatan - ms$m_K) / ms$sd_K,
    z_Faskes      = (log_Faskes - ms$m_F) / ms$sd_F,
    z_Pendidikan  = (log_Pendidikan - ms$m_P) / ms$sd_P,
    z_Rasio       = (Rasio - ms$m_R) / ms$sd_R,
    z_Sinyal      = (log_Sinyal - ms$m_S) / ms$sd_S
  )

# ═══════════════════════════════════════════════════════════════
# STEP H: PCA DAN SENSITIVITY BERDASARKAN PERIODE DATA
# ═══════════════════════════════════════════════════════════════

cat("\n>>> STEP H: PCA SENSITIVITY ANALYSIS\n")

# ---------------------------------------------------------------
# FUNGSI PCA EXPOSURE
# ---------------------------------------------------------------

run_pca_exposure <- function(data, periode_label) {
  
  cat("\n====================================================\n")
  cat("EXPOSURE PCA:", periode_label, "\n")
  cat("====================================================\n")
  
  vars <- c(
    "z_Kepadatan",
    "z_Faskes",
    "z_Pendidikan"
  )
  
  complete <- complete.cases(data[, vars])
  
  cat(
    sprintf(
      "Complete cases: %d / %d\n",
      sum(complete),
      nrow(data)
    )
  )
  
  mat <- as.matrix(data[complete, vars])
  
  # KMO dan Bartlett
  kmo <- KMO(mat)
  bart <- cortest.bartlett(
    cor(mat),
    n = nrow(mat)
  )
  
  cat(sprintf(
    "KMO = %.3f | Bartlett p = %.5f\n",
    kmo$MSA,
    bart$p.value
  ))
  
  # PCA
  pca <- prcomp(
    mat,
    center = FALSE,
    scale. = FALSE
  )
  
  variance_pc1 <-
    100 * pca$sdev[1]^2 /
    sum(pca$sdev^2)
  
  cat(sprintf(
    "PC1 explained variance = %.2f%%\n",
    variance_pc1
  ))
  
  load <- pca$rotation[,1]
  
  cat("\nLoading awal:\n")
  print(load)
  
  # Semua indikator exposure harus memiliki
  # arah positif terhadap exposure.
  if (sum(load > 0) < sum(load < 0)) {
    
    pca$rotation[,1] <- -pca$rotation[,1]
    pca$x[,1]        <- -pca$x[,1]
    
    cat("\n⚠️ Orientasi Exposure PC1 dibalik.\n")
  }
  
  load_final <- pca$rotation[,1]
  
  cat("\nLoading FINAL:\n")
  print(load_final)
  
  # Score
  score <- rep(NA_real_, nrow(data))
  score[complete] <- pca$x[,1]
  
  # Min-Max 0-1
  index <- rep(NA_real_, nrow(data))
  
  index[complete] <-
    (score[complete] - min(score[complete])) /
    (max(score[complete]) - min(score[complete]))
  
  return(
    list(
      pca = pca,
      complete = complete,
      index = index,
      loading = load_final,
      variance = variance_pc1,
      KMO = kmo$MSA,
      Bartlett_p = bart$p.value
    )
  )
}


# ---------------------------------------------------------------
# FUNGSI PCA VULNERABILITY
# ---------------------------------------------------------------

run_pca_vulnerability <- function(data, periode_label) {
  
  cat("\n====================================================\n")
  cat("VULNERABILITY PCA:", periode_label, "\n")
  cat("====================================================\n")
  
  vars <- c(
    "z_Rasio",
    "z_Sinyal"
  )
  
  complete <- complete.cases(data[, vars])
  
  cat(
    sprintf(
      "Complete cases: %d / %d\n",
      sum(complete),
      nrow(data)
    )
  )
  
  mat <- as.matrix(data[complete, vars])
  
  # KMO dan Bartlett
  kmo <- KMO(mat)
  bart <- cortest.bartlett(
    cor(mat),
    n = nrow(mat)
  )
  
  cat(sprintf(
    "KMO = %.3f | Bartlett p = %.5f\n",
    kmo$MSA,
    bart$p.value
  ))
  
  # PCA
  pca <- prcomp(
    mat,
    center = FALSE,
    scale. = FALSE
  )
  
  variance_pc1 <-
    100 * pca$sdev[1]^2 /
    sum(pca$sdev^2)
  
  cat(sprintf(
    "PC1 explained variance = %.2f%%\n",
    variance_pc1
  ))
  
  load <- pca$rotation[,1]
  
  cat("\nLoading awal:\n")
  print(load)
  
  # z_Rasio:
  # rasio usia rentan tinggi -> vulnerability tinggi
  #
  # z_Sinyal:
  # sudah dibalik sebelumnya
  # sehingga sinyal rendah -> vulnerability tinggi
  #
  # Jadi keduanya harus POSITIF.
  
  if (load["z_Rasio"] < 0) {
    
    pca$rotation[,1] <- -pca$rotation[,1]
    pca$x[,1]        <- -pca$x[,1]
    
    cat("\n⚠️ Orientasi Vulnerability PC1 dibalik.\n")
  }
  
  load_final <- pca$rotation[,1]
  
  cat("\nLoading FINAL:\n")
  print(load_final)
  
  # Score
  score <- rep(NA_real_, nrow(data))
  score[complete] <- pca$x[,1]
  
  # Min-Max 0-1
  index <- rep(NA_real_, nrow(data))
  
  index[complete] <-
    (score[complete] - min(score[complete])) /
    (max(score[complete]) - min(score[complete]))
  
  return(
    list(
      pca = pca,
      complete = complete,
      index = index,
      loading = load_final,
      variance = variance_pc1,
      KMO = kmo$MSA,