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
      Bartlett_p = bart$p.value
    )
  )
}


# ===============================================================
# 1. RUN PCA UNTUK TIGA PERIODE
# ===============================================================

pca_periods <- list(
  
  "2019_2025" =
    panel_std %>%
    filter(Tahun >= 2019, Tahun <= 2025),
  
  "2023_2025" =
    panel_std %>%
    filter(Tahun >= 2023, Tahun <= 2025),
  
  "2024_2025" =
    panel_std %>%
    filter(Tahun >= 2024, Tahun <= 2025)
)


# ===============================================================
# 2. SIMPAN HASIL PCA
# ===============================================================

pca_results <- list()

for (periode in names(pca_periods)) {
  
  dat <- pca_periods[[periode]]
  
  exp_res <-
    run_pca_exposure(
      dat,
      periode
    )
  
  vul_res <-
    run_pca_vulnerability(
      dat,
      periode
    )
  
  pca_results[[periode]] <-
    list(
      exposure = exp_res,
      vulnerability = vul_res
    )
}


# ===============================================================
# 3. PILIH 2019-2025 SEBAGAI BASELINE
# ===============================================================

baseline_period <- "2019_2025"

cat("\n============================================\n")
cat("BASELINE PCA:", baseline_period, "\n")
cat("============================================\n")

# ===============================================================
# PCA DIAGNOSTIC OUTPUT - LOADINGS, EIGENVALUES,
# VARIANCE EXPLAINED, AND ORIENTATION
# ===============================================================

pca_diagnostic_rows <- list()

for (periode in names(pca_results)) {
  
  # -------------------------
  # EXPOSURE
  # -------------------------
  
  exp_pca <- pca_results[[periode]]$exposure$pca
  
  exp_eigenvalues <- exp_pca$sdev^2
  
  exp_var_pct <- 100 * exp_eigenvalues /
    sum(exp_eigenvalues)
  
  exp_loadings <- pca_results[[periode]]$exposure$loading
  
  for (j in seq_along(exp_loadings)) {
    
    pca_diagnostic_rows[[length(pca_diagnostic_rows) + 1]] <-
      data.frame(
        Period = periode,
        Component = "Exposure",
        Variable = names(exp_loadings)[j],
        Loading_PC1 = exp_loadings[j],
        Eigenvalue_PC1 = exp_eigenvalues[1],
        Explained_Variance_PC1 = exp_var_pct[1],
        KMO = pca_results[[periode]]$exposure$KMO,
        Bartlett_p = pca_results[[periode]]$exposure$Bartlett_p
      )
  }
  
  # -------------------------
  # VULNERABILITY
  # -------------------------
  
  vul_pca <- pca_results[[periode]]$vulnerability$pca
  
  vul_eigenvalues <- vul_pca$sdev^2
  
  vul_var_pct <- 100 * vul_eigenvalues /
    sum(vul_eigenvalues)
  
  vul_loadings <- pca_results[[periode]]$vulnerability$loading
  
  for (j in seq_along(vul_loadings)) {
    
    pca_diagnostic_rows[[length(pca_diagnostic_rows) + 1]] <-
      data.frame(
        Period = periode,
        Component = "Vulnerability",
        Variable = names(vul_loadings)[j],
        Loading_PC1 = vul_loadings[j],
        Eigenvalue_PC1 = vul_eigenvalues[1],
        Explained_Variance_PC1 = vul_var_pct[1],
        KMO = pca_results[[periode]]$vulnerability$KMO,
        Bartlett_p = pca_results[[periode]]$vulnerability$Bartlett_p
      )
  }
}

pca_diagnostic_table <- bind_rows(pca_diagnostic_rows)

cat("\n============================================================\n")
cat(" PCA DIAGNOSTIC RESULTS\n")
cat("============================================================\n")

print(
  kable(
    pca_diagnostic_table,
    format = "simple",
    digits = 4,
    caption = "PCA loadings, eigenvalues, explained variance, and diagnostics"
  )
)

# ---------------------------------------------------------------
# BASELINE PCA OUTPUT ONLY
# ---------------------------------------------------------------

baseline_pca_output <- bind_rows(
  
  data.frame(
    Component = "Exposure",
    Variable = names(
      pca_results[["2019_2025"]]$exposure$loading
    ),
    Loading_PC1 =
      pca_results[["2019_2025"]]$exposure$loading,
    Eigenvalue_PC1 =
      pca_results[["2019_2025"]]$exposure$pca$sdev[1]^2,
    Explained_Variance_PC1 =
      pca_results[["2019_2025"]]$exposure$variance,
    KMO =
      pca_results[["2019_2025"]]$exposure$KMO,
    Bartlett_p =
      pca_results[["2019_2025"]]$exposure$Bartlett_p
  ),
  
  data.frame(
    Component = "Vulnerability",
    Variable = names(
      pca_results[["2019_2025"]]$vulnerability$loading
    ),
    Loading_PC1 =
      pca_results[["2019_2025"]]$vulnerability$loading,
    Eigenvalue_PC1 =
      pca_results[["2019_2025"]]$vulnerability$pca$sdev[1]^2,
    Explained_Variance_PC1 =
      pca_results[["2019_2025"]]$vulnerability$variance,
    KMO =
      pca_results[["2019_2025"]]$vulnerability$KMO,
    Bartlett_p =
      pca_results[["2019_2025"]]$vulnerability$Bartlett_p
  )
)

cat("\nBASELINE PCA 2019-2025:\n")

print(
  kable(
    baseline_pca_output,
    format = "simple",
    digits = 4
  )
)

# ===============================================================
# 4. BENTUK LOOKUP INDEX BASELINE
# ===============================================================

pca_base_data <- pca_periods[[baseline_period]]

lookup_idx <- data.frame(
  ID_Kec = pca_base_data$ID_Kec,
  Tahun  = pca_base_data$Tahun,
  
  Exp_Index =
    pca_results[[baseline_period]]$exposure$index,
  
  Vul_Index =
    pca_results[[baseline_period]]$vulnerability$index
)


# ===============================================================
# 5. GABUNGKAN KE PANEL
# ===============================================================

panel_final <- panel_std %>%
  left_join(
    lookup_idx,
    by = c("ID_Kec", "Tahun")
  )


# ============================================================
# MODUL 03: PROYEKSI UTM + DISTANCE VARIABLES
# ============================================================
cat("\n═══════════════════════════════════════════════════════════\n")
cat("  MODUL 03: PROYEKSI UTM + DISTANCE VARIABLES\n")
cat("═══════════════════════════════════════════════════════════\n\n")

utm_crs <- 32748

kecamatan_utm <- st_transform(kecamatan_sf, crs = utm_crs)
sesar_utm <- st_transform(sesar_sf, crs = utm_crs)
gempa_utm <- st_transform(gempa_sf, crs = utm_crs)

kec_cent_utm <- st_centroid(kecamatan_utm)
coords_utm <- st_coordinates(kec_cent_utm)
n_kec <- nrow(coords_utm)

bbox_utm <- st_bbox(kecamatan_utm)
diag_m <- sqrt(
  (bbox_utm$xmax - bbox_utm$xmin)^2 +
    (bbox_utm$ymax - bbox_utm$ymin)^2
)

# ------------------------------------------------------------
# Vs30 per kecamatan: observed + spatial nearest-neighbour fill
# ------------------------------------------------------------
vs30_vec <- kecamatan_utm$Vs30_mean
vs30_source <- rep("observed", length(vs30_vec))

na_vs30 <- which(is.na(vs30_vec))

if (length(na_vs30) > 0) {
  for (i in na_vs30) {
    valid <- which(!is.na(vs30_vec))
    if (length(valid) == 0) stop("Tidak ada nilai Vs30 yang dapat digunakan untuk imputasi.")
    
    d <- sqrt(
      (coords_utm[i, 1] - coords_utm[valid, 1])^2 +
        (coords_utm[i, 2] - coords_utm[valid, 2])^2
    )
    
    d[d < 1] <- 1
    nn <- order(d)[seq_len(min(3, length(d)))]
    w <- (1 / d[nn]) / sum(1 / d[nn])
    vs30_vec[i] <- sum(vs30_vec[valid[nn]] * w)
  }
  vs30_source[na_vs30] <- "spatial_imputed"
}

# Floor Vs30 untuk mencegah nilai tidak fisik di transformasi GMPE
vs30_vec[vs30_vec < 100] <- 100

# ------------------------------------------------------------
# Jarak kecamatan ke Sesar Lembang
# ------------------------------------------------------------
kec_coords_wgs84 <- st_coordinates(st_transform(st_centroid(kecamatan_sf), 4326))
sesar_coords_wgs84 <- st_coordinates(st_transform(sesar_sf, 4326))

Distance_to_fault_km <- numeric(nrow(kec_coords_wgs84))

for (i in seq_len(nrow(kec_coords_wgs84))) {
  d_vec <- haversine_distance(
    kec_coords_wgs84[i, "Y"],
    kec_coords_wgs84[i, "X"],
    sesar_coords_wgs84[, "Y"],
    sesar_coords_wgs84[, "X"]
  )
  Distance_to_fault_km[i] <- min(d_vec, na.rm = TRUE)
}

kecamatan_sf$Distance_to_fault_km <- Distance_to_fault_km

cat(sprintf("Jumlah kecamatan = %d\n", n_kec))
cat(sprintf("Vs30 missing diimputasi = %d\n", length(na_vs30)))
cat(sprintf("Mean distance to fault = %.2f km\n", mean(Distance_to_fault_km, na.rm = TRUE)))

# ============================================================
# MODUL 04: GMPE + SCENARIO PGA Mw 6.8
# ============================================================
cat("\n═══════════════════════════════════════════════════════════\n")
cat("  MODUL 04: GMPE + SCENARIO Mw 6.8\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# ------------------------------------------------------------
# GMPE yang konsisten dengan syntax awal penelitian
# ------------------------------------------------------------
gmpe_boore2014_simplified <- function(
    M,
    R,
    Depth,
    Vs30,
    coeff
) {
  dm <- ifelse(M > coeff$Mh, M - coeff$Mh, 0)
  
  FE <- coeff$c0 +
    coeff$c1 * M +
    coeff$c2 * M^2 +
    ifelse(M <= coeff$Mh, 0, coeff$c4 * dm)
  
  FP <- coeff$c3 * log(R / coeff$Rref) +
    ifelse(
      M <= coeff$Mh,
      coeff$c4 * M * log(R / coeff$Rref),
      coeff$c4 * coeff$Mh * log(R / coeff$Rref)
    )
  
  FS_lin <- coeff$c7 * log(Vs30 / coeff$Vref)
  
  PGA_ref <- exp(FE + FP)
  
  f2 <- ifelse(
    Vs30 <= coeff$V1,
    coeff$b1 * log(PGA_ref / 0.1),
    ifelse(
      Vs30 <= coeff$V2,
      coeff$b1 * log(PGA_ref / 0.1) +
        coeff$b2 * log(PGA_ref / 0.1) *
        log(Vs30 / coeff$V1) / log(coeff$V2 / coeff$V1),
      0
    )
  )
  
  FS <- FS_lin + f2
  log_pga <- FE + FP + FS
  
  list(
    log_pga = log_pga,
    PGA_median = exp(log_pga),
    PGA_ref = PGA_ref
  )
}

# ------------------------------------------------------------
# Scenario Mw 6.8
# ------------------------------------------------------------
scenario_Mw <- CFG$scenario_Mw
depth_scenario_km <- 10

# Jarak 3D proxy dari rupture Sesar Lembang ke centroid kecamatan
R_scenario_km <- sqrt(
  Distance_to_fault_km^2 +
    depth_scenario_km^2
)

pga_scenario_result <- gmpe_boore2014_simplified(
  M = scenario_Mw,
  R = R_scenario_km,
  Depth = depth_scenario_km,
  Vs30 = vs30_vec,
  coeff = CFG$gmpe
)

pga_scenario <- pga_scenario_result$PGA_median

kecamatan_sf$PGA_scenario_Mw68 <- pga_scenario

kecamatan_sf$wilayah_terdampak <- ifelse(
  kecamatan_sf$PGA_scenario_Mw68 >= CFG$scenario_PGA_threshold,
  "Terdampak",
  "Tidak terdampak"
)

wilayah_terdampak <- kecamatan_sf %>%
  filter(wilayah_terdampak == "Terdampak")

cat(sprintf("Scenario Mw = %.1f\n", scenario_Mw))
cat(sprintf("Affected threshold = %.3f g (MMI VI)\n", CFG$scenario_PGA_threshold))
cat(sprintf("Affected districts = %d / %d\n", nrow(wilayah_terdampak), nrow(kecamatan_sf)))
cat(sprintf("PGA scenario mean = %.4f g\n", mean(pga_scenario, na.rm = TRUE)))
cat(sprintf("PGA scenario max = %.4f g\n", max(pga_scenario, na.rm = TRUE)))

cat("✅ MODUL 03-04 SELESAI\n\n")

# ============================================================
# OUTPUT PETA PGA - FORMAT SELURUH JAWA BARAT
# ============================================================

# ------------------------------------------------------------
# 1. Membentuk batas kecamatan seluruh Jawa Barat
# ------------------------------------------------------------

kecamatan_jabar <- desa_raw %>%
  group_by(
    kabupaten_kota,
    kecamatan
  ) %>%
  summarise(
    geom = st_union(geom),
    .groups = "drop"
  ) %>%
  st_make_valid() %>%
  rename(
    geometry = geom
  ) %>%
  st_set_geometry("geometry")


# ------------------------------------------------------------
# 2. Membentuk batas kabupaten/kota seluruh Jawa Barat
# ------------------------------------------------------------

kabkota_jabar <- desa_raw %>%
  group_by(
    kabupaten_kota
  ) %>%
  summarise(
    geom = st_union(geom),
    .groups = "drop"
  ) %>%
  st_make_valid() %>%
  rename(
    geometry = geom
  ) %>%
  st_set_geometry("geometry")


# ------------------------------------------------------------
# 3. Ambil hasil PGA dari wilayah penelitian saja
# ------------------------------------------------------------

pga_target <- kecamatan_sf %>%
  st_drop_geometry() %>%
  dplyr::select(
    kabupaten_kota,
    kecamatan,
    PGA_scenario_Mw68
  )


# ------------------------------------------------------------
# 4. Gabungkan PGA ke seluruh kecamatan Jawa Barat
#
# Wilayah penelitian:
#   → mempunyai nilai PGA
#
# Wilayah di luar penelitian:
#   → NA
#   → ditampilkan putih
# ------------------------------------------------------------

kecamatan_map <- kecamatan_jabar %>%
  left_join(
    pga_target,
    by = c(
      "kabupaten_kota",
      "kecamatan"
    )
  )


# ------------------------------------------------------------
# 5. Batas kecamatan dan kabupaten/kota
# ------------------------------------------------------------

kec_outline <- st_transform(
  kecamatan_jabar,
  crs = st_crs(kecamatan_map)
)

kabkota_outline <- st_transform(
  kabkota_jabar,
  crs = st_crs(kecamatan_map)
)

sesar_map <- st_transform(
  sesar_sf,
  crs = st_crs(kecamatan_map)
)


# ============================================================
# 6. THEME PETA
# ============================================================

theme_peta_tesis <- function() {
  
  theme_minimal(
    base_size = 10
  ) +
    
    theme(
      panel.grid = element_blank(),
      
      axis.text = element_blank(),
      
      axis.ticks = element_blank(),
      
      axis.title = element_blank(),
      
      legend.position = "right",
      
      plot.title = element_text(
        face = "bold",
        size = 13
      ),
      
      plot.subtitle = element_text(
        size = 10
      ),
      
      plot.caption = element_text(
        size = 8
      )
    )
}


# ============================================================
# 7. PETA PGA
# ============================================================

p_pga_tesis <- ggplot() +
  
  # ----------------------------------------------------------
# SELURUH JAWA BARAT
#
# 11 wilayah penelitian = warna PGA
# Wilayah di luar penelitian = putih
# ----------------------------------------------------------

geom_sf(
  data = kecamatan_map,
  
  aes(
    fill = PGA_scenario_Mw68
  ),
  
  color = NA
) +
  
  # ----------------------------------------------------------
# BATAS KECAMATAN
# ----------------------------------------------------------

geom_sf(
  data = kec_outline,
  
  fill = NA,
  
  color = "grey75",
  
  linewidth = 0.10
) +
  
  # ----------------------------------------------------------
# BATAS KABUPATEN/KOTA
# ----------------------------------------------------------

geom_sf(
  data = kabkota_outline,
  
  fill = NA,
  
  color = "black",
  
  linewidth = 0.60
) +
  
  # ----------------------------------------------------------
# SESAR LEMBANG
# ----------------------------------------------------------

geom_sf(
  data = sesar_map,
  
  color = "red",
  
  linewidth = 1.1
) +
  
  # ----------------------------------------------------------
# SKALA WARNA PGA
#
# NA = putih
# ----------------------------------------------------------

scale_fill_viridis_c(
  name = "PGA (g)",
  
  na.value = "white"
) +
  
  # ----------------------------------------------------------
# JUDUL
# ----------------------------------------------------------

labs(
  title = "PGA Skenario Gempa Sesar Lembang Mw 6.8",
  
  subtitle =
    "Wilayah penelitian pada 11 kabupaten/kota di Jawa Barat",
  
  caption =
    "Garis merah: Sesar Lembang | Garis hitam: batas kabupaten/kota | Garis abu-abu: batas kecamatan"
) +
  
  theme_peta_tesis


# ============================================================
# 8. TAMPILKAN PETA
# ============================================================

print(
  p_pga_tesis
)

# ============================================================
# REVISI IJDRS - MODEL COMPARISON + SENSITIVITY + FINAL OUTPUT
# ============================================================
# Judul: Bayesian Hierarchical Spatio-Temporal Modeling of
# Earthquake Hazard and Relative Risk Prioritization around
# the Lembang Fault, Indonesia
#
# Revisi utama:
# 1. Threshold VI = 0.092 g digunakan konsisten untuk scenario
#    affected area dan Bayesian exceedance probability.
# 2. Baseline PCA = 2019-2025.
# 3. Model comparison: null, temporal, spatial, full.
# 4. Sensitivity PCA: 2019-2025, 2023-2025, 2024-2025.
# 5. Sensitivity bobot H:E:V = 60:30:10, 50:25:25,
#    50:30:20, 70:20:10.
# 6. Sensitivity n_eff = 2, 3, 5, 10.
# 7. Sensitivity PC prior rho0 = 10,20,40 km dan sigma0 =
#    0.5,1,1.5.
# 8. Ranking stability dinilai dengan Spearman rho dan Top-10 overlap.
# ============================================================

cat("\n============================================================\n")
cat(" REVISI IJDRS - ANALYSIS PIPELINE\n")
cat("============================================================\n\n")

# ------------------------------------------------------------
# 0. GLOBAL REVISED SETTINGS
# ------------------------------------------------------------

CFG$n_eff <- 3
CFG$scenario_PGA_threshold <- CFG$threshold_MMI$VI
CFG$hazard_MMI_threshold <- CFG$threshold_MMI$VI
CFG$baseline_period <- "2019_2025"

# Sensitivity settings
SENS <- list(
  pca_periods = c("2019_2025", "2023_2025", "2024_2025"),
  weights = list(
    "60:30:10" = c(H = 0.60, E = 0.30, V = 0.10),
    "50:25:25" = c(H = 0.50, E = 0.25, V = 0.25),
    "50:30:20" = c(H = 0.50, E = 0.30, V = 0.20),
    "70:20:10" = c(H = 0.70, E = 0.20, V = 0.10)
  ),
  n_eff = c(2, 3, 5, 10),
  rho0_km = c(10, 20, 40),
  sigma0 = c(0.5, 1.0, 1.5)
)

# Baseline weights
W_BASE <- SENS$weights[["60:30:10"]]

# ------------------------------------------------------------
# 1. HELPER FUNCTIONS
# ------------------------------------------------------------

normalize01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) {
    return(rep(0, length(x)))
  }
  (x - rng[1]) / diff(rng)
}

rank_stability <- function(base_df, alt_df,
                           id_col = "ID_Kec",
                           rank_col = "Rank") {
  z <- base_df %>%
    dplyr::select(all_of(id_col), Rank_base = all_of(rank_col)) %>%
    inner_join(
      alt_df %>% dplyr::select(all_of(id_col), Rank_alt = all_of(rank_col)),
      by = id_col
    )
  if (nrow(z) < 2) {
    return(list(spearman = NA_real_, top10_overlap = NA_real_, data = z))
  }
  rho <- suppressWarnings(
    cor(z$Rank_base, z$Rank_alt, method = "spearman", use = "complete.obs")
  )
  top_n <- min(10, nrow(z))
  top_base <- z %>% arrange(Rank_base) %>% slice_head(n = top_n) %>% pull(all_of(id_col))
  top_alt  <- z %>% arrange(Rank_alt)  %>% slice_head(n = top_n) %>% pull(all_of(id_col))
  overlap <- length(intersect(top_base, top_alt)) / top_n
  list(spearman = rho, top10_overlap = overlap, data = z)
}

rank_label <- function(rho) {
  dplyr::case_when(
    is.na(rho) ~ "Not available",
    rho >= 0.90 ~ "Highly stable",
    rho >= 0.75 ~ "Moderately stable",
    TRUE ~ "Sensitive"
  )
}

# ------------------------------------------------------------
# 2. PCA INDEX BY KECAMATAN FOR EACH TIME WINDOW
# ------------------------------------------------------------

cat("\n>>> PCA INDEX BY KECAMATAN\n")

pca_index_by_kec <- list()
pca_summary_rows <- list()

for (per in SENS$pca_periods) {
  dat <- pca_periods[[per]]
  exp_res <- pca_results[[per]]$exposure
  vul_res <- pca_results[[per]]$vulnerability
  
  tmp <- data.frame(
    ID_Kec = dat$ID_Kec,
    Exp_Index = exp_res$index,
    Vul_Index = vul_res$index
  ) %>%
    group_by(ID_Kec) %>%
    summarise(
      Exp_Index = mean(Exp_Index, na.rm = TRUE),
      Vul_Index = mean(Vul_Index, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Exp_Index = ifelse(is.nan(Exp_Index), NA, Exp_Index),
      Vul_Index = ifelse(is.nan(Vul_Index), NA, Vul_Index)
    )
  
  pca_index_by_kec[[per]] <- tmp
  
  pca_summary_rows[[per]] <- data.frame(
    Component = c("Exposure", "Vulnerability"),
    Period = per,
    Explained_Variance_PC1 = c(
      pca_results[[per]]$exposure$variance,
      pca_results[[per]]$vulnerability$variance
    ),
    KMO = c(
      pca_results[[per]]$exposure$KMO,
      pca_results[[per]]$vulnerability$KMO
    ),
    Bartlett_p = c(
      pca_results[[per]]$exposure$Bartlett_p,
      pca_results[[per]]$vulnerability$Bartlett_p
    )
  )
}

pca_summary_table <- bind_rows(pca_summary_rows)
print(kable(pca_summary_table, format = "simple", digits = 3,
            caption = "PCA sensitivity across alternative temporal windows"))

# ============================================================
# 3. HAZARD: REBUILD HISTORICAL PROBABILITY USING 0.092 g
# ============================================================

cat("\n>>> HAZARD REBUILD - THRESHOLD 0.092 g / MMI VI\n")

# ------------------------------------------------------------
# 3.1 Dimensi data hazard
# ------------------------------------------------------------

n_kec <- nrow(kecamatan_sf)
hazard_years <- CFG$tahun_hazard_start:CFG$tahun_hazard_end
n_hazard_years <- length(hazard_years)
hazard_threshold_g <- CFG$scenario_PGA_threshold

cat(sprintf("Jumlah kecamatan : %d\n", n_kec))
cat(sprintf("Periode hazard   : %d-%d\n", min(hazard_years), max(hazard_years)))
cat(sprintf("Jumlah tahun     : %d\n", n_hazard_years))
cat(sprintf("Threshold PGA    : %.3f g (MMI VI)\n", hazard_threshold_g))

# ------------------------------------------------------------
# 3.2 Matriks tahunan
# ------------------------------------------------------------
annual_pga_max <- matrix(
  NA_real_,
  nrow = n_kec,
  ncol = n_hazard_years
)

annual_p_prob <- matrix(
  NA_real_,
  nrow = n_kec,
  ncol = n_hazard_years
)

# ------------------------------------------------------------
# 3.3 Hitung PGA maksimum dan exceedance probability
# ------------------------------------------------------------
for (t in seq_along(hazard_years)) {
  
  year_t <- hazard_years[t]
  
  cat(sprintf(
    "Processing hazard year: %d (%d/%d)\n",
    year_t, t, n_hazard_years
  ))
  
  gempa_t <- gempa_utm %>%
    dplyr::filter(Tahun == year_t)
  
  if (nrow(gempa_t) == 0) {
    annual_pga_max[, t] <- 0
    annual_p_prob[, t] <- 0
    next
  }
  
  pga_all <- matrix(
    NA_real_,
    nrow = n_kec,
    ncol = nrow(gempa_t)
  )
  
  for (j in seq_len(nrow(gempa_t))) {
    
    # Koordinat episentrum gempa j
    event_xy <- st_coordinates(gempa_t[j, ])
    
    # ----------------------------------------------------------
    # Jarak horizontal episentrum -> centroid kecamatan
    # ----------------------------------------------------------
    distance_to_event_km <- sqrt(
      (coords_utm[, 1] - event_xy[1, 1])^2 +
        (coords_utm[, 2] - event_xy[1, 2])^2
    ) / 1000
    
    # ----------------------------------------------------------
    # Jarak 3D hiposenter -> centroid kecamatan
    # ----------------------------------------------------------
    R_event_km <- sqrt(
      distance_to_event_km^2 +
        gempa_t$Depth[j]^2
    )
    
    # ----------------------------------------------------------
    # PGA per kecamatan
    # ----------------------------------------------------------
    pga_result <- gmpe_boore2014_simplified(
      M = gempa_t$Mw[j],
      R = R_event_km,
      Depth = gempa_t$Depth[j],
      Vs30 = vs30_vec,
      coeff = CFG$gmpe
    )
    
    pga_all[, j] <- pga_result$PGA_median
  }
  
  # PGA maksimum kecamatan pada tahun t
  annual_pga_max[, t] <- apply(
    pga_all,
    1,
    max,
    na.rm = TRUE
  )
  
  # Indikator exceedance:
  # 1 = PGA >= threshold
  # 0 = PGA < threshold
  p_all <- ifelse(
    pga_all >= hazard_threshold_g,
    1,
    0
  )
  
  # Proporsi kejadian gempa yang melampaui threshold
  annual_p_prob[, t] <- rowMeans(
    p_all,
    na.rm = TRUE
  )

  # Hindari nilai tepat 0 atau 1 untuk likelihood Binomial
  annual_p_prob[, t] <- pmin(
    pmax(annual_p_prob[, t], 0.001),
    0.999
  )
}

# ------------------------------------------------------------
# 3.4 HAZARD DISTRICT-YEAR
# ------------------------------------------------------------
# annual_p_prob:
#   baris    = kecamatan
#   kolom    = tahun hazard 2009-2024
#
# Untuk model Bayesian, hazard dipertahankan pada
# level kecamatan-tahun, bukan dirata-ratakan lintas tahun.
# ------------------------------------------------------------

cat("\n>>> CHECK HAZARD VARIABILITY\n")

cat("\nSummary annual_p_prob:\n")
print(summary(as.vector(annual_p_prob)))

cat("\nJumlah nilai unik hazard:\n")
print(length(unique(round(as.vector(annual_p_prob), 6))))

cat("\nRange hazard probability:\n")
print(range(annual_p_prob, na.rm = TRUE))

cat("\nRange annual PGA max:\n")
print(range(annual_pga_max, na.rm = TRUE))

cat("\nProporsi hazard probability = 0:\n")
print(mean(annual_p_prob == 0, na.rm = TRUE))

cat("\nProporsi hazard probability = 1:\n")
print(mean(annual_p_prob == 1, na.rm = TRUE))

cat("\n>>> MEMBENTUK HAZARD DISTRICT-YEAR\n")

# Tahun hazard yang tersedia
hazard_years_available <- hazard_years

# Pastikan tahun model tersedia
model_years <- CFG$tahun_exp_seq

# ------------------------------------------------------------
# 3.4.1 Ambil hazard untuk tahun 2019-2024
# ------------------------------------------------------------

idx_hazard_model <- match(
  model_years,
  hazard_years_available
)

# Matriks hazard untuk tahun model
annual_p <- matrix(
  NA_real_,
  nrow = n_kec,
  ncol = length(model_years),
  dimnames = list(
    NULL,
    model_years
  )
)

annual_pga <- matrix(
  NA_real_,
  nrow = n_kec,
  ncol = length(model_years),
  dimnames = list(
    NULL,
    model_years
  )
)

# Isi tahun yang memang tersedia di katalog
for (tt in seq_along(model_years)) {

  idx_h <- idx_hazard_model[tt]

  if (!is.na(idx_h)) {

    annual_p[, tt] <-
      annual_p_prob[, idx_h]

    annual_pga[, tt] <-
      annual_pga_max[, idx_h]

  }
}


# ------------------------------------------------------------
# 3.4.2 Tahun 2025
# ------------------------------------------------------------
# Katalog gempa tersedia sampai 2024.
# Karena model exposure-vulnerability berjalan sampai 2025,
# hazard 2025 sementara menggunakan carry-forward 2024.
#
# CATATAN:
# Ini adalah asumsi temporal proxy dan harus dijelaskan
# eksplisit pada manuscript jika tetap dipertahankan.
# ------------------------------------------------------------

idx_2024_model <- which(model_years == 2024)
idx_2025_model <- which(model_years == 2025)

if (
  length(idx_2025_model) == 1 &&
  length(idx_2024_model) == 1
) {

  annual_p[, idx_2025_model] <-
    annual_p[, idx_2024_model]

  annual_pga[, idx_2025_model] <-
    annual_pga[, idx_2024_model]

  cat(
    "⚠️ Hazard 2025 tidak tersedia dalam katalog; ",
    "digunakan carry-forward dari 2024.\n"
  )
}


# ------------------------------------------------------------
# 3.4.3 Cek struktur hazard
# ------------------------------------------------------------

cat("\nHazard matrix:\n")
cat(
  "  Kecamatan :", nrow(annual_p), "\n"
)
cat(
  "  Tahun     :", ncol(annual_p), "\n"
)
cat(
  "  Tahun     :", paste(colnames(annual_p), collapse = ", "), "\n"
)

cat("\nMean hazard probability per tahun:\n")

print(
  round(
    colMeans(annual_p, na.rm = TRUE),
    4
  )
)

cat("\nMean PGA per tahun:\n")

print(
  round(
    colMeans(annual_pga, na.rm = TRUE),
    4
  )
)


# ------------------------------------------------------------
# 3.5 VALIDASI HAZARD DISTRICT-YEAR
# ------------------------------------------------------------

cat("\n>>> VALIDASI HAZARD DISTRICT-YEAR\n")

cat(
  "Minimum hazard probability = ",
  round(
    min(annual_p, na.rm = TRUE),
    4
  ),
  "\n"
)

cat(
  "Maximum hazard probability = ",
  round(
    max(annual_p, na.rm = TRUE),
    4
  ),
  "\n"
)

cat(
  "Jumlah NA hazard = ",
  sum(is.na(annual_p)),
  "\n"
)

# ============================================================
# 3.6 PGA BASELINE UNTUK RINGKASAN HAZARD
# ============================================================

# Baseline hazard mengikuti periode analisis utama 2019-2025
baseline_years <- as.character(
  CFG$baseline_period
)

# Pastikan hanya tahun yang benar-benar tersedia di annual_pga
baseline_cols <- intersect(
  baseline_years,
  colnames(annual_pga)
)

# Rata-rata PGA historis per kecamatan selama periode baseline
pga_baseline <- rowMeans(
  annual_pga[, baseline_cols, drop = FALSE],
  na.rm = TRUE
)

# Jika seluruh tahun untuk suatu kecamatan NA,
# rowMeans(..., na.rm = TRUE) menghasilkan NaN
pga_baseline[is.nan(pga_baseline)] <- NA_real_

cat("\nPGA baseline 2019-2025:\n")
print(summary(pga_baseline))
print(range(pga_baseline, na.rm = TRUE))

cat("\nCHECK PGA per tahun:\n")

for (yy in colnames(annual_pga)) {
  x <- annual_pga[, yy]
  
  cat(
    yy,
    " | valid =", sum(is.finite(x)),
    " | NA =", sum(is.na(x)),
    " | min =", suppressWarnings(min(x, na.rm = TRUE)),
    " | max =", suppressWarnings(max(x, na.rm = TRUE)),
    "\n"
  )
}

# ------------------------------------------------------------
# 4. FUNCTION TO BUILD INLA DATA FOR DIFFERENT n_eff
# ------------------------------------------------------------

summary(
  as.vector(
    annual_p_prob
  )
)

cat(
  "\nProporsi p >= 0.95 = ",
  mean(
    annual_p_prob >= 0.95,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Proporsi p >= 0.99 = ",
  mean(
    annual_p_prob >= 0.99,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "\nPseudo-count distribution:\n"
)

table(
  round(
    3 * annual_p_prob
  )
)

make_inla_data <- function(n_eff_value) {
  dat <- expand.grid(
    ID_Kec = 1:n_kec,
    Tahun = CFG$tahun_exp_seq
  ) %>%
    arrange(ID_Kec, Tahun) %>%
    mutate(
      ID_Tahun = match(Tahun, CFG$tahun_exp_seq)
    )
  
  dat$PGA <- NA_real_
  dat$p_hazard <- NA_real_
  
  for (tt in seq_len(CFG$n_tahun)) {
    idx_t <- dat$Tahun == CFG$tahun_exp_seq[tt]
    dat$PGA[idx_t] <- annual_pga[, tt]
    dat$p_hazard[idx_t] <- annual_p[, tt]
  }
  
  dat$p_hazard <- pmin(pmax(dat$p_hazard, 0.001), 0.999)
  dat$Y <- round(n_eff_value * dat$p_hazard)
  dat$Trials <- n_eff_value
  
  dat
}

cat("\n>>> CHECK BINOMIAL RESPONSE\n")

dat_check <- make_inla_data(CFG$n_eff)

cat("\nSummary Y:\n")
print(summary(dat_check$Y))

cat("\nSummary Trials:\n")
print(summary(dat_check$Trials))

cat("\nProporsi Y = 0:\n")
print(mean(dat_check$Y == 0, na.rm = TRUE))

cat("\nProporsi Y = Trials:\n")
print(mean(dat_check$Y == dat_check$Trials, na.rm = TRUE))

cat("\nUnique Y:\n")
print(sort(unique(dat_check$Y)))

# ------------------------------------------------------------
# 5. BUILD MESH ONCE
# ------------------------------------------------------------

cat("\n>>> BUILD BASE MESH\n")

max_edge_inner <- CFG$mesh_inner_pct * diag_m
max_edge_outer <- CFG$mesh_outer_pct * diag_m
cutoff_dist <- CFG$mesh_cutoff_pct * diag_m

mesh <- inla.mesh.2d(
  loc = coords_utm,
  boundary = list(
    inla.nonconvex.hull(coords_utm, convex = -0.05),
    inla.nonconvex.hull(coords_utm, convex = -0.15)
  ),
  max.edge = c(max_edge_inner, max_edge_outer),
  cutoff = cutoff_dist
)

# ------------------------------------------------------------
# 6. GENERIC INLA FIT FUNCTION
# ------------------------------------------------------------

fit_bayes_model <- function(
    model_type = c("null", "temporal", "spatial", "full"),
    n_eff_value = 3,
    rho0_km = CFG$pc_rho0_km,
    sigma0 = CFG$pc_sigma0,
    rho0_prob = CFG$pc_rho0_prob,
    sigma0_prob = CFG$pc_sigma0_prob,
    threshold_g = CFG$scenario_PGA_threshold
) {

  # ----------------------------------------------------------
  # A. MODEL TYPE
  # ----------------------------------------------------------

  model_type <- match.arg(model_type)

  # ----------------------------------------------------------
  # B. DATA MODEL
  # ----------------------------------------------------------

  dat <- make_inla_data(
    n_eff_value
  )

  n_obs <- nrow(dat)

  # ==========================================================
  # C. SPATIAL / FULL MODEL
  # ==========================================================

  if (model_type %in% c("spatial", "full")) {

    # --------------------------------------------------------
    # C.1 SPDE MODEL
    # --------------------------------------------------------

    spde_local <- inla.spde2.pcmatern(
      mesh,
      alpha = 2,

      prior.range = c(
        rho0_km * 1000,
        rho0_prob
      ),

      prior.sigma = c(
        sigma0,
        sigma0_prob
      )
    )

    # --------------------------------------------------------
    # C.2 A MATRIX SPATIAL
    # --------------------------------------------------------

    if (model_type == "full") {

      A_local <- inla.spde.make.A(
        mesh,

        loc = coords_utm[
          dat$ID_Kec,
          ,
          drop = FALSE
        ],

        group = dat$ID_Tahun,
        n.group = CFG$n_tahun
      )

    } else {

      A_local <- inla.spde.make.A(
        mesh,

        loc = coords_utm[
          dat$ID_Kec,
          ,
          drop = FALSE
        ]
      )
    }

    # ========================================================
    # C.3 FULL SPATIO-TEMPORAL MODEL
    # ========================================================

    if (model_type == "full") {

      # ------------------------------------------------------
      # Spatial index
      # ------------------------------------------------------

      s_index <- inla.spde.make.index(
        name = "spatial",
        n.spde = spde_local$n.spde,
        n.group = CFG$n_tahun
      )

      # ------------------------------------------------------
      # Fixed + temporal effects
      # ------------------------------------------------------

      effects_fixed <- data.frame(
        Intercept = rep(1, n_obs),
        ID_Tahun = dat$ID_Tahun
      )

      # ------------------------------------------------------
      # Effects list
      # ------------------------------------------------------

      effects_list <- list(
        s_index,
        effects_fixed
      )

      # ------------------------------------------------------
      # A list
      # ------------------------------------------------------

      A_list <- list(
        A_local,
        1
      )

      # ------------------------------------------------------
      # Formula
      # ------------------------------------------------------

      formula_local <- Y ~ -1 +
        Intercept +

        f(
          spatial,
          model = spde_local,

          group = spatial.group,

          control.group = list(
            model = "iid"
          )
        ) +

        f(
          ID_Tahun,
          model = "rw1"
        )

    # ========================================================
    # C.4 SPATIAL ONLY MODEL
    # ========================================================

    } else {

      # ------------------------------------------------------
      # Spatial index
      # ------------------------------------------------------

      s_index <- inla.spde.make.index(
        name = "spatial",
        n.spde = spde_local$n.spde
      )

      # ------------------------------------------------------
      # Fixed effects
      # ------------------------------------------------------

      effects_fixed <- data.frame(
        Intercept = rep(1, n_obs)
      )

      # ------------------------------------------------------
      # Effects list
      # ------------------------------------------------------

      effects_list <- list(
        s_index,
        effects_fixed
      )

      # ------------------------------------------------------
      # A list
      # ------------------------------------------------------

      A_list <- list(
        A_local,
        1
      )

      # ------------------------------------------------------
      # Formula
      # ------------------------------------------------------

      formula_local <- Y ~ -1 +
        Intercept +

        f(
          spatial,
          model = spde_local
        )
    }

  # ==========================================================
  # D. TEMPORAL / NULL MODEL
  # ==========================================================

  } else {

    # --------------------------------------------------------
    # D.1 Fixed effects + temporal index
    # --------------------------------------------------------

    effects_fixed <- data.frame(
      Intercept = rep(1, n_obs),
      ID_Tahun = dat$ID_Tahun
    )

    effects_list <- list(
      effects_fixed
    )

    A_list <- list(
      1
    )

    # ========================================================
    # D.2 TEMPORAL MODEL
    # ========================================================

    if (model_type == "temporal") {

      formula_local <- Y ~ -1 +
        Intercept +

        f(
          ID_Tahun,
          model = "rw1"
        )

    # ========================================================
    # D.3 NULL MODEL
    # ========================================================

    } else {

      formula_local <- Y ~ -1 +
        Intercept
    }
  }

  # ==========================================================
  # E. CHECK STACK COMPONENTS
  # ==========================================================

  # Cek dimensi sebelum INLA
  cat("\n--------------------------------------------\n")
  cat("CHECK INLA STACK\n")
  cat(sprintf("Model      : %s\n", model_type))
  cat(sprintf("Observations: %d\n", n_obs))
  cat(sprintf("A blocks   : %d\n", length(A_list)))
  cat(sprintf("Effect blocks: %d\n", length(effects_list)))
  cat("--------------------------------------------\n")

  # ==========================================================
  # F. INLA STACK
  # ==========================================================

  stack_local <- inla.stack(
    data = list(
      Y = dat$Y
    ),

    A = A_list,

    effects = effects_list,

    tag = "est"
  )

  # ==========================================================
  # G. FIT INLA
  # ==========================================================

  fit <- inla(

    formula_local,

    data = inla.stack.data(
      stack_local
    ),

    family = "binomial",

    Ntrials = dat$Trials,

    control.predictor = list(
      A = inla.stack.A(
        stack_local
      ),

      compute = TRUE,

      link = 1
    ),

    control.compute = list(

      dic = TRUE,

      waic = TRUE,

      cpo = TRUE,

      config = TRUE
    ),

    control.inla = list(

      strategy = "gaussian",

      int.strategy = "eb"
    ),

    verbose = FALSE
  )

  # ==========================================================
  # H. POSTERIOR LINEAR PREDICTOR
  # ==========================================================

  idx_local <-
    inla.stack.index(
      stack_local,
      "est"
    )$data

  eta <-
    fit$summary.linear.predictor$mean[
      idx_local
    ]

  # ==========================================================
  # I. POSTERIOR EXCEEDANCE PROBABILITY
  # ==========================================================

  p <- plogis(
    eta
  )

  # ==========================================================
  # J. POSTERIOR PER KECAMATAN
  # ==========================================================

  posterior_kec_local <-
    data.frame(

      ID_Kec =
        dat$ID_Kec,

      p_mean =
        p

    ) %>%

    group_by(
      ID_Kec
    ) %>%

    summarise(

      p_mean_avg =
        mean(
          p_mean,
          na.rm = TRUE
        ),

      .groups = "drop"
    )

  # ==========================================================
  # K. CPO FAILURE
  # ==========================================================

  cpo_fail <-

    if (!is.null(
      fit$cpo$failure
    )) {

      sum(
        fit$cpo$failure > 0,
        na.rm = TRUE
      )

    } else {

      NA_integer_
    }

  # ==========================================================
  # L. MODEL METRICS
  # ==========================================================

  dic_value <-
    if (!is.null(fit$dic)) {
      fit$dic$dic
    } else {
      NA_real_
    }

  waic_value <-
    if (!is.null(fit$waic)) {
      fit$waic$waic
    } else {
      NA_real_
    }

  sum_log_cpo <-

    if (!is.null(fit$cpo$cpo)) {

      sum(
        log(
          fit$cpo$cpo
        ),
        na.rm = TRUE
      )

    } else {

      NA_real_
    }

  metrics_local <- data.frame(

    Model =
      model_type,

    n_eff =
      n_eff_value,

    rho0_km =
      rho0_km,

    sigma0 =
      sigma0,

    DIC =
      dic_value,

    WAIC =
      waic_value,

    SumLogCPO =
      sum_log_cpo,

    CPO_Failure =
      cpo_fail
  )

  # ==========================================================
  # M. RETURN
  # ==========================================================

  return(
    list(

      model =
        fit,

      data =
        dat,

      stack =
        stack_local,

      posterior_kec =
        posterior_kec_local,

      metrics =
        metrics_local
    )
  )
}

# ------------------------------------------------------------
# 7. MODEL COMPARISON
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" MODEL COMPARISON\n")
cat("============================================================\n")

# ------------------------------------------------------------
# Model yang tersedia di fit_bayes_model()
# ------------------------------------------------------------
#
# M0 = Fixed effects only
#      Intercept + rupture distance
#
# M1 = Fixed + temporal
#      Intercept + rupture distance + RW1 temporal
#
# M2 = Fixed + spatial
#      Intercept + rupture distance + SPDE spatial
#
# M3 = Full spatial-temporal
#      Intercept + rupture distance + SPDE spatial + RW1 temporal
# ------------------------------------------------------------

model_types <- c(
  "null",
  "temporal",
  "spatial",
  "full"
)

model_labels <- c(
  null =
    "M0: Fixed effects only",

  temporal =
    "M1: Fixed + temporal",

  spatial =
    "M2: Fixed + spatial",

  full =
    "M3: Full spatial-temporal"
)

# ------------------------------------------------------------
# Fit all candidate models
# ------------------------------------------------------------

model_fits <- list()
model_metrics <- list()

for (mt in model_types) {

  cat(
    sprintf(
      "\nFitting %s ...\n",
      model_labels[[mt]]
    )
  )

  fit_obj <- fit_bayes_model(
    model_type = mt,
    n_eff_value = CFG$n_eff,
    rho0_km = CFG$pc_rho0_km,
    sigma0 = CFG$pc_sigma0
  )

  model_fits[[mt]] <-
    fit_obj

  model_metrics[[mt]] <-
    fit_obj$metrics
}

# ------------------------------------------------------------
# Combine model comparison results
# ------------------------------------------------------------

table1_model_comparison <-

  bind_rows(
    model_metrics
  ) %>%

  mutate(
    Model = recode(
      Model,

      null =
        "M0: Fixed effects only",

      temporal =
        "M1: Fixed + temporal",

      spatial =
        "M2: Fixed + spatial",

      full =
        "M3: Full spatial-temporal"
    )
  ) %>%

  arrange(DIC)

# ------------------------------------------------------------
# Display results
# ------------------------------------------------------------

cat(
  "\nTABLE 1 - MODEL COMPARISON\n"
)

print(
  kable(
    table1_model_comparison,
    format = "simple",
    digits = 2,
    caption =
      "Comparison of alternative Bayesian hierarchical model specifications"
  )
)

# ------------------------------------------------------------
# 8. FINAL BASELINE MODEL = FULL MODEL
# ------------------------------------------------------------

final_fit <- model_fits[["full"]]
model_inla <- final_fit$model
stack_est <- final_fit$stack
data_inla <- final_fit$data

cat("\nFinal model: M3 Full spatial-temporal\n")
cat(sprintf("DIC = %.2f\n", model_inla$dic$dic))
cat(sprintf("WAIC = %.2f\n", model_inla$waic$waic))
cat(sprintf("Sum log CPO = %.2f\n", sum(log(model_inla$cpo$cpo), na.rm = TRUE)))

# ------------------------------------------------------------
# 9. FINAL POSTERIOR
# ------------------------------------------------------------

idx_final <- inla.stack.index(stack_est, "est")$data
eta_mean <- model_inla$summary.linear.predictor$mean[idx_final]
eta_sd <- model_inla$summary.linear.predictor$sd[idx_final]
eta_q025 <- model_inla$summary.linear.predictor$`0.025quant`[idx_final]
eta_q975 <- model_inla$summary.linear.predictor$`0.975quant`[idx_final]

p_mean <- plogis(eta_mean)
p_q025 <- plogis(eta_q025)
p_q975 <- plogis(eta_q975)
p_sd_delta <- p_mean * (1 - p_mean) * eta_sd

n_samp <- 500
set.seed(123)
samples_final <- inla.posterior.sample(n_samp, model_inla)

eta_samp <- matrix(
  NA_real_,
  nrow = length(idx_final),
  ncol = n_samp
)

latent_names <- rownames(samples_final[[1]]$latent)
pred_idx <- grep("Predictor", latent_names)

if (length(pred_idx) >= length(idx_final)) {
  for (ss in seq_len(n_samp)) {
    eta_samp[, ss] <- samples_final[[ss]]$latent[pred_idx[idx_final], 1]
  }
} else {
  for (ii in seq_len(length(idx_final))) {
    eta_samp[ii, ] <- rnorm(
      n_samp,
      eta_mean[ii],
      eta_sd[ii]
    )
  }
}

p_samp <- plogis(eta_samp)

posterior_kec <- data.frame(
  ID_Kec = data_inla$ID_Kec,
  p_mean = p_mean,
  p_sd = p_sd_delta,
  p_q025 = p_q025,
  p_q975 = p_q975
) %>%
  group_by(ID_Kec) %>%
  summarise(
    p_mean_avg = mean(p_mean, na.rm = TRUE),
    p_sd_avg = mean(p_sd, na.rm = TRUE),
    ci_width_avg = mean(p_q975 - p_q025, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 10. FINAL RISK USING BASELINE PCA 2019-2025
# ------------------------------------------------------------

pca_base <- pca_index_by_kec[[CFG$baseline_period]]

risk_base <- posterior_kec %>%
  left_join(pca_base, by = "ID_Kec") %>%
  left_join(
    lookup_kec,
    by = "ID_Kec"
  ) %>%
  left_join(
    kecamatan_sf %>%
      st_drop_geometry() %>%
      dplyr::select(
        ID_Kec,
        PGA_scenario_Mw68,
        Distance_to_fault_km,
        Vs30_mean
      ),
    by = "ID_Kec"
  )

cat("\n>>> CHECK POSTERIOR HAZARD\n")

print(
  summary(risk_base$p_mean_avg)
)

cat("\nUnique posterior hazard values:\n")
print(
  length(unique(round(risk_base$p_mean_avg, 6)))
)

cat("\nRange posterior hazard:\n")
print(
  range(risk_base$p_mean_avg, na.rm = TRUE)
)

risk_base <- risk_base %>%
  mutate(
    Hazard_Index = normalize01(p_mean_avg),
    Exposure_Index = normalize01(Exp_Index),
    Vulnerability_Index = normalize01(Vul_Index),
    Risk_Score = 100 * (
      W_BASE["H"] * Hazard_Index +
        W_BASE["E"] * Exposure_Index +
        W_BASE["V"] * Vulnerability_Index
    )
  ) %>%
  arrange(desc(Risk_Score)) %>%
  mutate(Rank = row_number())

risk_ranked <- risk_base

# ------------------------------------------------------------
# 10B. MISSINGNESS SENSITIVITY
# Exclude highly incomplete vulnerability variables
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" MISSINGNESS SENSITIVITY\n")
cat("============================================================\n")

# Baseline risk
risk_missingness <- risk_base %>%
  select(
    ID_Kec,
    Hazard_Index,
    Exposure_Index
  )

# ------------------------------------------------------------
# Sensitivity 1:
# Exclude vulnerable-age ratio
# Vulnerability = telecommunication coverage only
# ------------------------------------------------------------

vul_no_rasio <- pca_index_by_kec[[CFG$baseline_period]] %>%
  select(ID_Kec, Vul_Index) %>%
  rename(Vul_full = Vul_Index)

# Ambil standardized telecommunication variable
sinyal_index <- panel_std %>%
  filter(
    Tahun >= 2019,
    Tahun <= 2025
  ) %>%
  group_by(ID_Kec) %>%
  summarise(
    Sinyal_Index = mean(z_Sinyal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Sinyal_Index = normalize01(Sinyal_Index)
  )

risk_no_rasio <- risk_missingness %>%
  left_join(sinyal_index, by = "ID_Kec") %>%
  mutate(
    # Renormalized H:E weights because vulnerability component is removed
    Risk_Score = 100 * (
      (0.60 / 0.90) * Hazard_Index +
        (0.30 / 0.90) * Exposure_Index
    )
  ) %>%
  arrange(desc(Risk_Score)) %>%
  mutate(Rank = row_number())

# ------------------------------------------------------------
# Sensitivity 2:
# Exclude telecommunication coverage
# Vulnerability = vulnerable-age ratio only
# ------------------------------------------------------------

rasio_index <- panel_std %>%
  filter(
    Tahun >= 2019,
    Tahun <= 2025
  ) %>%
  group_by(ID_Kec) %>%
  summarise(
    Rasio_Index = mean(z_Rasio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rasio_Index = normalize01(Rasio_Index)
  )

risk_no_sinyal <- risk_missingness %>%
  left_join(rasio_index, by = "ID_Kec") %>%
  mutate(
    Risk_Score = 100 * (
      (0.60 / 0.90) * Hazard_Index +
        (0.30 / 0.90) * Exposure_Index
    )
  ) %>%
  arrange(desc(Risk_Score)) %>%
  mutate(Rank = row_number())

# ------------------------------------------------------------
# Sensitivity 3:
# Exclude both highly incomplete vulnerability variables
# ------------------------------------------------------------

risk_no_vulnerability <- risk_missingness %>%
  mutate(
    Risk_Score = 100 * (
      (0.60 / 0.90) * Hazard_Index +
        (0.30 / 0.90) * Exposure_Index
    )
  ) %>%
  arrange(desc(Risk_Score)) %>%
  mutate(Rank = row_number())

# ------------------------------------------------------------
# Compare with baseline
# ------------------------------------------------------------

stab_no_rasio <- rank_stability(
  risk_ranked,
  risk_no_rasio
)

stab_no_sinyal <- rank_stability(
  risk_ranked,
  risk_no_sinyal
)

stab_no_vulnerability <- rank_stability(
  risk_ranked,
  risk_no_vulnerability
)

missingness_sensitivity <- bind_rows(
  
  data.frame(
    Scenario = "Exclude vulnerable-age ratio",
    Spearman_Rho = stab_no_rasio$spearman,
    Top10_Overlap = stab_no_rasio$top10_overlap,
    Stability = rank_label(stab_no_rasio$spearman)
  ),
  
  data.frame(
    Scenario = "Exclude telecommunication coverage",
    Spearman_Rho = stab_no_sinyal$spearman,
    Top10_Overlap = stab_no_sinyal$top10_overlap,
    Stability = rank_label(stab_no_sinyal$spearman)
  ),
  
  data.frame(
    Scenario = "Exclude both vulnerability variables",
    Spearman_Rho = stab_no_vulnerability$spearman,
    Top10_Overlap = stab_no_vulnerability$top10_overlap,
    Stability = rank_label(stab_no_vulnerability$spearman)
  )
)

cat("\nMissingness sensitivity results:\n")

print(
  kable(
    missingness_sensitivity,
    format = "simple",
    digits = 3
  )
)

# ------------------------------------------------------------
# 11. PCA PERIOD SENSITIVITY - RANK STABILITY
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" PCA PERIOD SENSITIVITY\n")
cat("============================================================\n")

pca_sensitivity_rows <- list()

for (per in SENS$pca_periods) {
  idx_per <- pca_index_by_kec[[per]]
  
  alt_risk <- posterior_kec %>%
    left_join(idx_per, by = "ID_Kec") %>%
    mutate(
      Hazard_Index = normalize01(p_mean_avg),
      Exposure_Index = normalize01(Exp_Index),
      Vulnerability_Index = normalize01(Vul_Index),
      Risk_Score = 100 * (
        W_BASE["H"] * Hazard_Index +
          W_BASE["E"] * Exposure_Index +
          W_BASE["V"] * Vulnerability_Index
      )
    ) %>%
    arrange(desc(Risk_Score)) %>%
    mutate(Rank = row_number())
  
  stab <- rank_stability(risk_ranked, alt_risk)
  
  pca_sensitivity_rows[[per]] <- data.frame(
    Component = "Exposure + Vulnerability",
    Period = per,
    Spearman_Rho = stab$spearman,
    Top10_Overlap = stab$top10_overlap,
    Stability = rank_label(stab$spearman)
  )
}

table2a_pca <- bind_rows(pca_sensitivity_rows)

cat("\nPCA period sensitivity:\n")
print(kable(table2a_pca, format = "simple", digits = 3))

# ------------------------------------------------------------
# 12. H:E:V WEIGHT SENSITIVITY
# ------------------------------------------------------------
# Main analysis remains based on the originally specified
# 60:30:10 weighting.
#
# The four candidate schemes are compared pairwise to assess
# ranking stability. Alternative schemes do NOT automatically
# replace the primary 60:30:10 specification.
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" H:E:V WEIGHT SENSITIVITY - PAIRWISE\n")
cat("============================================================\n")

weight_ranked <- list()

# Build risk ranking for each candidate weight scheme
for (wn in names(SENS$weights)) {
  
  ww <- SENS$weights[[wn]]
  
  alt <- risk_base %>%
    mutate(
      Risk_Score = 100 * (
        ww["H"] * Hazard_Index +
          ww["E"] * Exposure_Index +
          ww["V"] * Vulnerability_Index
      )
    ) %>%
    arrange(desc(Risk_Score)) %>%
    mutate(Rank = row_number())
  
  weight_ranked[[wn]] <- alt
}

# Pairwise comparisons: each scheme is compared with the
# OTHER schemes, never with itself.
weight_names <- names(weight_ranked)
pairwise_rows <- list()
kk <- 1

for (i in seq_along(weight_names)) {
  for (j in seq_along(weight_names)) {
    
    if (i >= j) next
    
    stab <- rank_stability(
      weight_ranked[[weight_names[i]]],
      weight_ranked[[weight_names[j]]]
    )
    
    pairwise_rows[[kk]] <- data.frame(
      Scheme_A = weight_names[i],
      Scheme_B = weight_names[j],
      Spearman_Rho = stab$spearman,
      Top10_Overlap = stab$top10_overlap,
      stringsAsFactors = FALSE
    )
    
    kk <- kk + 1
  }
}

table2b_pairwise <- bind_rows(pairwise_rows)

cat("\nPairwise H:E:V comparisons:\n")
print(
  kable(
    table2b_pairwise,
    format = "simple",
    digits = 3
  )
)

# Overall stability for each candidate across the OTHER schemes.
weight_stability <- lapply(weight_names, function(wn) {
  
  other <- table2b_pairwise %>%
    filter(Scheme_A == wn | Scheme_B == wn)
  
  mean_rho <- mean(other$Spearman_Rho, na.rm = TRUE)
  mean_top10 <- mean(other$Top10_Overlap, na.rm = TRUE)
  
  data.frame(
    HEV_Weight = wn,
    Mean_Spearman_Rho = mean_rho,
    Mean_Top10_Overlap = mean_top10,
    Stability_Score =
      0.5 * mean_rho +
      0.5 * mean_top10,
    stringsAsFactors = FALSE
  )
}) %>%
  bind_rows() %>%
  arrange(
    desc(Stability_Score),
    desc(Mean_Spearman_Rho),
    desc(Mean_Top10_Overlap)
  ) %>%
  mutate(
    Stability_Rank = row_number(),
    Main_Specification =
      ifelse(
        HEV_Weight == "60:30:10",
        "Primary specification",
        "Alternative"
      )
  )

cat("\nOverall H:E:V stability across candidate schemes:\n")
print(
  kable(
    weight_stability,
    format = "simple",
    digits = 3
  )
)

# Keep the ORIGINAL main-analysis weighting.
W_BASE <- SENS$weights[["60:30:10"]]

# Compact Table 2 Panel B
table2b_weights <- weight_stability %>%
  transmute(
    HEV_Weight,
    Mean_Spearman_Rho,
    Mean_Top10_Overlap,
    Stability_Score,
    Stability_Rank,
    Main_Specification
  )

# ------------------------------------------------------------
# 13. n_eff SENSITIVITY
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" n_eff SENSITIVITY\n")
cat("============================================================\n")

neff_fits <- list()
neff_rows <- list()
neff_ranked <- list()

for (ne in SENS$n_eff) {
  cat(sprintf("\nFitting n_eff = %d ...\n", ne))
  
  # Reuse baseline prior and full spatial-temporal specification.
  fit_ne <- fit_bayes_model(
    model_type = "full",
    n_eff_value = ne,
    rho0_km = CFG$pc_rho0_km,
    sigma0 = CFG$pc_sigma0
  )
  
  neff_fits[[as.character(ne)]] <- fit_ne
  
  risk_ne <- fit_ne$posterior_kec %>%
    left_join(pca_base, by = "ID_Kec") %>%
    mutate(
      Hazard_Index = normalize01(p_mean_avg),
      Exposure_Index = normalize01(Exp_Index),
      Vulnerability_Index = normalize01(Vul_Index),
      Risk_Score = 100 * (
        W_BASE["H"] * Hazard_Index +
          W_BASE["E"] * Exposure_Index +
          W_BASE["V"] * Vulnerability_Index
      )
    ) %>%
    arrange(desc(Risk_Score)) %>%
    mutate(Rank = row_number())
  
  neff_ranked[[as.character(ne)]] <- risk_ne
  
  stab <- rank_stability(risk_ranked, risk_ne)
  
  neff_rows[[as.character(ne)]] <- data.frame(
    n_eff = ne,
    DIC = fit_ne$metrics$DIC,
    WAIC = fit_ne$metrics$WAIC,
    SumLogCPO = fit_ne$metrics$SumLogCPO,
    Spearman_Rho = stab$spearman,
    Top10_Overlap = stab$top10_overlap,
    Stability = rank_label(stab$spearman)
  )
}

table2c_neff <- bind_rows(neff_rows)

cat("\nn_eff sensitivity:\n")
print(kable(table2c_neff, format = "simple", digits = 3))

# ------------------------------------------------------------
# 14. PC PRIOR SENSITIVITY
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" PC PRIOR SENSITIVITY\n")
cat("============================================================\n")

prior_sensitivity_rows <- list()
prior_fits <- list()

# Vary rho0 while holding sigma0 at baseline
for (rr in SENS$rho0_km) {
  cat(sprintf("\nPC prior rho0 = %d km, sigma0 = %.1f ...\n", rr, CFG$pc_sigma0))
  
  key <- paste0("rho", rr)
  fit_pr <- fit_bayes_model(
    model_type = "full",
    n_eff_value = CFG$n_eff,
    rho0_km = rr,
    sigma0 = CFG$pc_sigma0
  )
  prior_fits[[key]] <- fit_pr
  
  risk_pr <- fit_pr$posterior_kec %>%
    left_join(pca_base, by = "ID_Kec") %>%
    mutate(
      Hazard_Index = normalize01(p_mean_avg),
      Exposure_Index = normalize01(Exp_Index),
      Vulnerability_Index = normalize01(Vul_Index),
      Risk_Score = 100 * (
        W_BASE["H"] * Hazard_Index +
          W_BASE["E"] * Exposure_Index +
          W_BASE["V"] * Vulnerability_Index
      )
    ) %>%
    arrange(desc(Risk_Score)) %>%
    mutate(Rank = row_number())
  
  stab <- rank_stability(risk_ranked, risk_pr)
  
  prior_sensitivity_rows[[key]] <- data.frame(
    Prior_Scenario = paste0("rho0=", rr, " km; sigma0=", CFG$pc_sigma0),
    rho0_km = rr,
    sigma0 = CFG$pc_sigma0,
    DIC = fit_pr$metrics$DIC,
    WAIC = fit_pr$metrics$WAIC,
    Spearman_Rho = stab$spearman,
    Top10_Overlap = stab$top10_overlap,
    Stability = rank_label(stab$spearman)
  )
}

# Vary sigma0 while holding rho0 at baseline
for (ss in SENS$sigma0) {
  if (ss == CFG$pc_sigma0) next
  cat(sprintf("\nPC prior rho0 = %d km, sigma0 = %.1f ...\n", CFG$pc_rho0_km, ss))
  
  key <- paste0("sigma", ss)
  fit_pr <- fit_bayes_model(
    model_type = "full",
    n_eff_value = CFG$n_eff,
    rho0_km = CFG$pc_rho0_km,
    sigma0 = ss
  )
  prior_fits[[key]] <- fit_pr
  
  risk_pr <- fit_pr$posterior_kec %>%
    left_join(pca_base, by = "ID_Kec") %>%
    mutate(
      Hazard_Index = normalize01(p_mean_avg),
      Exposure_Index = normalize01(Exp_Index),
      Vulnerability_Index = normalize01(Vul_Index),
      Risk_Score = 100 * (
        W_BASE["H"] * Hazard_Index +
          W_BASE["E"] * Exposure_Index +
          W_BASE["V"] * Vulnerability_Index
      )
    ) %>%
    arrange(desc(Risk_Score)) %>%
    mutate(Rank = row_number())
  
  stab <- rank_stability(risk_ranked, risk_pr)
  
  prior_sensitivity_rows[[key]] <- data.frame(
    Prior_Scenario = paste0("rho0=", CFG$pc_rho0_km, " km; sigma0=", ss),
    rho0_km = CFG$pc_rho0_km,
    sigma0 = ss,
    DIC = fit_pr$metrics$DIC,
    WAIC = fit_pr$metrics$WAIC,
    Spearman_Rho = stab$spearman,
    Top10_Overlap = stab$top10_overlap,
    Stability = rank_label(stab$spearman)
  )
}

table2d_prior <- bind_rows(prior_sensitivity_rows)

cat("\nPC prior sensitivity:\n")
print(kable(table2d_prior, format = "simple", digits = 3))

# ------------------------------------------------------------
# 15. TABLE 2 FINAL - COMBINED SENSITIVITY TABLE
# ------------------------------------------------------------

# To keep the journal table compact, write the main numerical
# sensitivity results to one combined object.

table2_sensitivity <- bind_rows(
  table2a_pca %>%
    transmute(
      Panel = "A. PCA period",
      Scenario = Period,
      DIC = NA_real_,
      WAIC = NA_real_,
      Spearman_Rho,
      Top10_Overlap,
      Stability
    ),
  table2b_weights %>%
    transmute(
      Panel = "B. H:E:V weights",
      Scenario = HEV_Weight,
      DIC = NA_real_,
      WAIC = NA_real_,
      Spearman_Rho = Mean_Spearman_Rho,
      Top10_Overlap = Mean_Top10_Overlap,
      Stability = paste0(
        "Rank ", Stability_Rank, " - ",
        Main_Specification
      )
    ),
  table2c_neff %>%
    transmute(
      Panel = "C. n_eff",
      Scenario = paste0("n_eff=", n_eff),
      DIC,
      WAIC,
      Spearman_Rho,
      Top10_Overlap,
      Stability
    ),
  table2d_prior %>%
    transmute(
      Panel = "D. PC prior",
      Scenario = Prior_Scenario,
      DIC,
      WAIC,
      Spearman_Rho,
      Top10_Overlap,
      Stability
    )
)

cat("\n============================================================\n")
cat(" TABLE 2 - SENSITIVITY ANALYSIS\n")
cat("============================================================\n")
print(
  kable(
    table2_sensitivity,
    format = "simple",
    digits = 3,
    caption = "Sensitivity analysis of PCA windows, risk weights, effective sample size, and PC priors"
  )
)

# ------------------------------------------------------------
# 16. TABLE 3 - TOP 10 RELATIVE RISK PRIORITIES
# ------------------------------------------------------------

table3_top10 <- risk_ranked %>%
  slice_head(n = 10) %>%
  transmute(
    Rank,
    District = kecamatan,
    `Regency/City` = kabupaten,
    `Relative Risk` = Risk_Score,
    `Hazard Index` = Hazard_Index,
    `Exposure Index` = Exposure_Index,
    `Vulnerability Index` = Vulnerability_Index,
    `Posterior Exceedance` = p_mean_avg
  )

cat("\n============================================================\n")
cat(" TABLE 3 - TOP 10 RISK PRIORITIES\n")
cat("============================================================\n")
print(
  kable(
    table3_top10,
    format = "simple",
    digits = 3,
    caption = "Ten districts with the highest posterior relative-risk scores"
  )
)

# ------------------------------------------------------------
# 17. FINAL POSTERIOR PARAMETER TABLE FOR INTERNAL CHECK
# ------------------------------------------------------------

fixed_df <- data.frame(
  Parameter = rownames(model_inla$summary.fixed),
  Mean = model_inla$summary.fixed$mean,
  SD = model_inla$summary.fixed$sd,
  Q025 = model_inla$summary.fixed$`0.025quant`,
  Q975 = model_inla$summary.fixed$`0.975quant`
)

hyper_df <- data.frame(
  Parameter = rownames(model_inla$summary.hyperpar),
  Mean = model_inla$summary.hyperpar$mean,
  SD = model_inla$summary.hyperpar$sd,
  Q025 = model_inla$summary.hyperpar$`0.025quant`,
  Q975 = model_inla$summary.hyperpar$`0.975quant`
)

cat("\nPosterior fixed effects:\n")
print(kable(fixed_df, format = "simple", digits = 4))

cat("\nPosterior hyperparameters:\n")
print(kable(hyper_df, format = "simple", digits = 4))

# ------------------------------------------------------------
# 18. PIT - RANDOMIZED DISCRETE PIT
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" PIT CALIBRATION\n")
cat("============================================================\n")

# Observed response
Y_test <- data_inla$Y

# Menyediakan vector PIT
pit_vals <- numeric(length(Y_test))

set.seed(123)

for (ii in seq_along(Y_test)) {
  
  y_i <- Y_test[ii]
  
  # Lower CDF: P(Y < y_i)
  F_lower <- mean(
    pbinom(
      y_i - 1,
      size = CFG$n_eff,
      prob = p_samp[ii, ]
    )
  )
  
  # Upper CDF: P(Y <= y_i)
  F_upper <- mean(
    pbinom(
      y_i,
      size = CFG$n_eff,
      prob = p_samp[ii, ]
    )
  )
  
  # Randomized PIT untuk distribusi diskrit
  pit_vals[ii] <-
    F_lower +
    runif(1) * (F_upper - F_lower)
}

# ------------------------------------------------------------
# Batasi PIT pada interval [0,1]
# ------------------------------------------------------------

pit_vals <- pmin(
  pmax(pit_vals, 0),
  1
)

# ------------------------------------------------------------
# KS TEST terhadap Uniform(0,1)
# ------------------------------------------------------------

ks_pit <- suppressWarnings(
  ks.test(
    pit_vals,
    "punif"
  )
)

cat(
  sprintf(
    "PIT KS statistic = %.4f\n",
    unname(ks_pit$statistic)
  )
)

cat(
  sprintf(
    "PIT KS p-value = %.4f\n",
    ks_pit$p.value
  )
)

# ------------------------------------------------------------
# 19. FIGURE 1 - STUDY AREA
# ------------------------------------------------------------

# File Sesar Lembang
fault_file <- file.path(
  CFG$data_dir,
  "titik_sesar_lembang.kmz"
)

# ------------------------------------------------------------
# KODE WILAYAH STUDI
# ------------------------------------------------------------

kode_study_area <- c(
  "32.04",   # Bandung Regency
  "32.17",   # West Bandung Regency
  "32.03",   # Cianjur Regency
  "32.05",   # Garut Regency
  "32.14",   # Purwakarta Regency
  "32.13",   # Subang Regency
  "32.11",   # Sumedang Regency
  "32.73",   # Bandung City
  "32.77"    # Cimahi City
)

# ------------------------------------------------------------
# KONSTRUKSI STUDY AREA
# ------------------------------------------------------------

study_desa <- desa_raw %>%
  filter(kd_kab %in% kode_study_area) %>%
  st_make_valid()

cat(">>> Constructing study-area boundaries...\n")

study_area <- study_desa %>%
  group_by(kabupaten_kota) %>%
  summarise(
    geometry = st_union(geom),
    .groups = "drop"
  ) %>%
  st_make_valid() %>%
  st_set_geometry("geometry") %>%
  mutate(
    label = case_when(
      
      kabupaten_kota == "Bandung" ~
        "Bandung Regency",
      
      kabupaten_kota == "Bandung Barat" ~
        "West Bandung Regency",
      
      kabupaten_kota == "Cianjur" ~
        "Cianjur Regency",
      
      kabupaten_kota == "Garut" ~
        "Garut Regency",
      
      kabupaten_kota == "Purwakarta" ~
        "Purwakarta Regency",
      
      kabupaten_kota == "Subang" ~
        "Subang Regency",
      
      kabupaten_kota == "Sumedang" ~
        "Sumedang Regency",
      
      kabupaten_kota == "Kota Bandung" ~
        "Bandung City",
      
      kabupaten_kota == "Kota Cimahi" ~
        "Cimahi City",
      
      TRUE ~ kabupaten_kota
    )
  )

# ------------------------------------------------------------
# CHECK JUMLAH WILAYAH
# ------------------------------------------------------------

cat("\n>>> Study area regions:\n")

print(
  study_area %>%
    st_drop_geometry() %>%
    select(
      kabupaten_kota,
      label
    )
)

# ------------------------------------------------------------
# LOADING LEMBANG FAULT
# ------------------------------------------------------------

cat(">>> Loading Lembang Fault...\n")

# Folder sementara
tmp_dir <- tempfile()
dir.create(tmp_dir)

# Ekstrak KMZ
unzip(
  fault_file,
  exdir = tmp_dir
)

# Cari file KML
kml_file <- list.files(
  tmp_dir,
  pattern = "\\.kml$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(kml_file) == 0) {
  stop(
    "KML tidak ditemukan di dalam file titik_sesar_lembang.kmz"
  )
}

cat("KML found:\n")
print(kml_file)

# Baca KML
fault_raw <- st_read(
  kml_file[1],
  quiet = TRUE
)

cat(
  "Fault features:",
  nrow(fault_raw),
  "\n"
)

# ------------------------------------------------------------
# TRANSFORM CRS
# ------------------------------------------------------------

study_area <- st_transform(
  study_area,
  4326
)

fault_raw <- st_transform(
  fault_raw,
  4326
)

# ------------------------------------------------------------
# POTONG SESAR SESUAI STUDY AREA
# ------------------------------------------------------------

study_union <- study_area %>%
  summarise(
    geometry = st_union(geometry)
  )

fault_study <- st_intersection(
  fault_raw,
  study_union
)

# ------------------------------------------------------------
# BUAT TITIK LABEL
# ------------------------------------------------------------

label_points <- study_area %>%
  st_point_on_surface()

# ------------------------------------------------------------
# FIGURE 1
# ------------------------------------------------------------

figure1 <- ggplot() +
  
  # ----------------------------------------------------------
# Wilayah studi
# ----------------------------------------------------------

geom_sf(
  data = study_area,
  fill = "grey92",
  color = "black",
  linewidth = 0.65
) +
  
  # ----------------------------------------------------------
# Sesar Lembang
# ----------------------------------------------------------

geom_sf(
  data = fault_study,
  color = "red",
  linewidth = 1.2
) +
  
  # ----------------------------------------------------------
# LABEL SEMUA WILAYAH
# ----------------------------------------------------------

geom_sf_label(
  data = label_points,
  aes(
    label = label
  ),
  size = 3.0,
  fontface = "bold",
  fill = "white",
  color = "black",
  label.size = 0.25,
  label.padding = unit(
    0.15,
    "lines"
  )
) +
  
  # ----------------------------------------------------------
# North arrow
# ----------------------------------------------------------

annotation_north_arrow(
  location = "tl",
  which_north = "true",
  style = north_arrow_fancy_orienteering
) +
  
  # ----------------------------------------------------------
# Scale bar
# ----------------------------------------------------------

annotation_scale(
  location = "bl",
  width_hint = 0.25
) +
  
  # ----------------------------------------------------------
# Coordinate system
# ----------------------------------------------------------

coord_sf(
  expand = FALSE
) +
  
  # ----------------------------------------------------------
# Theme
# ----------------------------------------------------------

theme_minimal(
  base_size = 10
) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.title = element_blank(),
    
    axis.text = element_text(
      color = "black",
      size = 8
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    
    plot.subtitle = element_text(
      size = 10
    ),
    
    plot.caption = element_text(
      size = 8,
      hjust = 0
    ),
    
    legend.position = "none"
  )

# Tampilkan figure
print(figure1)

# ------------------------------------------------------------
# SIMPAN FIGURE - HIGH RESOLUTION
# ------------------------------------------------------------

CFG$output_dir <- "output"
if (!dir.exists(CFG$output_dir)) {
  dir.create(CFG$output_dir, recursive = TRUE)
}

ggsave(
  filename = file.path(
    CFG$output_dir,
    "Figure_1_Study_Area_Lembang_Fault.jpeg"
  ),
  plot = figure1,
  width = 10,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)

cat("\n>>> Figure 1 saved at 600 DPI.\n")

# ------------------------------------------------------------
# STUDY AREA CHECK
# ------------------------------------------------------------

cat("\n============================================\n")
cat("STUDY AREA CHECK\n")
cat("============================================\n")

print(
  study_area %>%
    st_drop_geometry() %>%
    select(
      kabupaten_kota,
      label
    )
)

# ------------------------------------------------------------
# LEMBANG FAULT CHECK
# ------------------------------------------------------------

cat("\n============================================\n")
cat("LEMBANG FAULT CHECK\n")
cat("============================================\n")

cat(
  "Number of fault features inside study area:",
  nrow(fault_study),
  "\n"
)

# ------------------------------------------------------------
# 20. FIGURE 2 - PGA + AFFECTED AREA
# ------------------------------------------------------------
kec_map_base <- kecamatan_sf
kabkota_outline <- st_transform(kabkota_sf, crs = st_crs(kec_map_base))
kec_outline <- st_transform(kecamatan_sf, crs = st_crs(kec_map_base))
sesar_map <- st_transform(sesar_sf, crs = st_crs(kec_map_base))

kabkota_cent <- st_centroid(kabkota_outline)
kabkota_coords <- st_coordinates(kabkota_cent)
kabkota_labels <- data.frame(
  label = kabkota_sf$kabupaten_kota_label,
  lon = kabkota_coords[, 1],
  lat = kabkota_coords[, 2]
)

theme_peta_journal <- function() {
  theme_minimal(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10),
      plot.caption = element_text(size = 8)
    )
}

p_pga <- ggplot() +
  geom_sf(
    data = kec_map_base,
    aes(fill = PGA_scenario_Mw68),
    color = NA
  ) +
  # Batas kecamatan = PUTIH
  geom_sf(
    data = kec_outline,
    fill = NA,
    color = "grey75",
    linewidth = 0.1
  ) +
  # Batas kabupaten/kota = ABU-ABU CERAH
  geom_sf(
    data = kabkota_outline,
    fill = NA,
    color = "black",
    linewidth = 0.7
  ) +
  geom_sf(
    data = sesar_map,
    color = "red",
    linewidth = 1.1
  ) +
  scale_fill_viridis_c(name = "PGA (g)") +
  labs(
    title = "Scenario-Based PGA for an Mw 6.8 Lembang Fault Earthquake",
    subtitle = "Affected area threshold: PGA ≥ 0.092 g (MMI VI)"
  ) +
  theme_peta_journal()


p_affected <- ggplot() +
  geom_sf(
    data = kec_map_base,
    aes(fill = PGA_scenario_Mw68 >= hazard_threshold_g),
    color = "grey70",
    linewidth = 0.1
  ) +
  # Batas kabupaten/kota = ABU-ABU CERAH
  geom_sf(
    data = kabkota_outline,
    fill = NA,
    color = "black",
    linewidth = 0.7
  ) +
  geom_sf(
    data = sesar_map,
    color = "red",
    linewidth = 1.1
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "grey90",
      "TRUE" = "steelblue"
    ),
    labels = c(
      "FALSE" = "Below threshold",
      "TRUE" = "Affected"
    ),
    name = "Status"
  ) +
  labs(
    title = "Affected Area",
    subtitle = "PGA ≥ 0.092 g (MMI VI)"
  ) +
  theme_peta_journal()


figure2 <- p_pga + p_affected +
  plot_annotation(
    title = "Earthquake Hazard and Affected Area"
  )

print(figure2)

figure2 <- p_pga + p_affected +
  plot_annotation(title = "Earthquake Hazard and Affected Area")

print(figure2)

ggsave(
  filename = file.path(
    CFG$output_dir,
    "Figure_2_PGA_Affected_Area.png"
  ),
  plot = figure2,
  width = 10,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = file.path(
    CFG$output_dir,
    "Figure_2_PGA_Affected_Area.jpeg"
  ),
  plot = figure2,
  width = 10,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)

# ------------------------------------------------------------
# 21. FIGURE 3 - POSTERIOR RISK
# ------------------------------------------------------------

risk_map <- kec_map_base %>%
  left_join(
    risk_ranked %>%
      dplyr::select(
        ID_Kec,
        Risk_Score,
        Rank,
        p_mean_avg
      ),
    by = "ID_Kec"
  )

kabkota_labels_fig3 <- kabkota_labels %>%
  mutate(
    label = case_when(
      grepl("^Kota ", label) ~
        paste0(sub("^Kota ", "", label), " City"),
      
      grepl("^Kab\\. ", label) ~
        paste0(sub("^Kab\\. ", "", label), " Regency"),
      
      TRUE ~ label
    )
  )

p_risk <- ggplot() +
  
  geom_sf(
    data = risk_map,
    aes(fill = Risk_Score),
    color = NA
  ) +
  
  geom_sf(
    data = kec_outline,
    fill = NA,
    color = "grey75",
    linewidth = 0.12
  ) +
  
  geom_sf(
    data = kabkota_outline,
    fill = NA,
    color = "black",
    linewidth = 0.6
  ) +
  
  geom_sf(
    data = sesar_map,
    color = "red",
    linewidth = 1.1
  ) +
  
  geom_text_repel(
    data = kabkota_labels_fig3,
    aes(
      x = lon,
      y = lat,
      label = label
    ),
    size = 2.6,
    fontface = "bold",
    color = "white",
    max.overlaps = 30,
    force = 1.5
  ) +
  
  scale_fill_viridis_c(
    name = "Relative risk"
  ) +
  
  labs(
    title = "Posterior Relative Risk",
    subtitle = "Baseline: H:E:V = 60:30:10; PCA window = 2019–2025"
  ) +
  
  theme_peta_journal()

figure3 <- p_risk +
  plot_annotation(
    title = "Posterior Relative Risk"
  )

print(figure3)

ggsave(
  filename = file.path(
    CFG$output_dir,
    "Figure_3_Posterior_Risk.png"
  ),
  plot = figure3,
  width = 10,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = file.path(
    CFG$output_dir,
    "Figure_3_Posterior_Risk.jpeg"
  ),
  plot = figure3,
  width = 10,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)

# ------------------------------------------------------------
# 22. FIGURE 4 - DESCRIPTIVE BOXPLOTS
# ------------------------------------------------------------

cat("\n━━━ A.5 Boxplot Variabel untuk Deteksi Outlier ━━━\n\n")

# ============================================================
# A. EXPOSURE & VULNERABILITY
# ============================================================

box_data <- panel_imputed %>%
  dplyr::select(Tahun, Kepadatan, Faskes, Pendidikan, Rasio, Sinyal) %>%
  tidyr::pivot_longer(
    -Tahun,
    names_to = "Variabel",
    values_to = "Nilai"
  ) %>%
  filter(!is.na(Nilai))

p_box <- ggplot(
  box_data,
  aes(x = Variabel, y = Nilai)
) +
  geom_boxplot(
    fill = "lightblue",
    outlier.color = "red",
    outlier.size = 1.5
  ) +
  facet_wrap(
    ~Variabel,
    scales = "free_y",
    nrow = 1,
    labeller = as_labeller(c(
      "Kepadatan" = "Population Density",
      "Faskes" = "Health Facilities",
      "Pendidikan" = "Education",
      "Rasio" = "Vulnerable-Age Ratio",
      "Sinyal" = "Telecommunication Coverage"
    ))
  ) +
  labs(
    x = NULL,
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(
      face = "bold",
      size = 10
    ),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank()
  )


# ============================================================
# B. HAZARD
# ============================================================

gempa_df <- gempa_sf %>%
  st_drop_geometry()

depth_col <- grep(
  "[Dd]epth",
  names(gempa_df),
  value = TRUE
)

gempa_box <- tibble(
  Magnitude = gempa_df$Mw,
  Depth = gempa_df[[depth_col[1]]],
  `Distance to Fault` = gempa_df$Distance_to_fault_km
) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variabel",
    values_to = "Nilai"
  ) %>%
  filter(is.finite(Nilai))


hazard_site_box <- tibble(
  PGA = pga_baseline,
  Vs30 = vs30_vec
) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variabel",
    values_to = "Nilai"
  ) %>%
  filter(is.finite(Nilai))


hazard_box_data <- bind_rows(
  gempa_box,
  hazard_site_box
)

hazard_box_data$Variabel <- factor(
  hazard_box_data$Variabel,
  levels = c(
    "Magnitude",
    "Depth",
    "Distance to Fault",
    "PGA",
    "Distance to Rupture",
    "Vs30"
  )
)

p_box_hazard <- ggplot(
  hazard_box_data,
  aes(x = Variabel, y = Nilai)
) +
  geom_boxplot(
    fill = "lightblue",
    outlier.color = "red",
    outlier.size = 1.5
  ) +
  facet_wrap(
    ~Variabel,
    scales = "free_y",
    nrow = 1
  ) +
  labs(
    x = NULL,
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(
      face = "bold",
      size = 10
    ),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank()
  )


# ============================================================
# C. GABUNGKAN MENJADI SATU FIGURE
# ============================================================

figure4 <- p_box / p_box_hazard

print(figure4)


# ============================================================
# D. SAVE FIGURE 4 - HIGH RESOLUTION
# ============================================================

ggsave(
  filename = file.path(
    CFG$output_dir,
    "Figure_4_Descriptive_Boxplots.png"
  ),
  plot = figure4,
  width = 12,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = file.path(
    CFG$output_dir,
    "Figure_4_Descriptive_Boxplots.jpeg"
  ),
  plot = figure4,
  width = 12,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)

cat("\n============================================================\n")
cat(" ANALYSIS SELESAI\n")
cat("============================================================\n")