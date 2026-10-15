\`%||%\` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

raw_paths <- list(
  BPS = file.path("data","raw","Data BPS.csv"),
  BMKG = file.path("data","raw","BMKG_Earthquake_Catalog.csv"),
  VS30 = file.path("data","raw","DATA_VS30_LENGKAP.csv"),
  BOUNDARY = file.path("data","raw","Batas_Desa_Kelurahan_JABAR.gpkg"),
  FAULT = file.path("data","raw","titik_sesar_lembang.kmz")
)

study_kab_codes <- c("32.04","32.17","32.03","32.05","32.14","32.13","32.11","32.73","32.77")

clean_key <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("^KAB\\.?\\s+", "", x)
  x <- gsub("[^A-Z0-9]+", "", x)
  x
}

load_bps <- function(path = raw_paths$BPS) {
  x <- data.table::fread(path, na.strings=c("", "NA", "NaN", "-"), encoding="UTF-8")
  for(v in intersect(c("Faskes","Kepadatan","Sinyal","Pendidikan"), names(x))) {
    x[[v]] <- suppressWarnings(as.numeric(gsub(",", "", as.character(x[[v]]), fixed=TRUE)))
  }
  x
}

load_bmkg <- function(path = raw_paths$BMKG) {
  x <- data.table::fread(path, na.strings=c("", "NA", "NaN"), encoding="UTF-8")
  if("Date" %in% names(x)) {
    x[, Date_parsed := as.Date(Date, tryFormats=c("%d-%b-%y","%d-%b-%Y","%Y-%m-%d","%d/%m/%Y"))]
  }
  x
}

load_vs30 <- function(path = raw_paths$VS30) {
  x <- data.table::fread(path, na.strings=c("", "NA", "NaN"), encoding="UTF-8")
  x[, vs30_mosaic := suppressWarnings(as.numeric(vs30_mosaic))]
  x
}

load_admin_raw <- function(path = raw_paths$BOUNDARY) {
  lyr <- sf::st_layers(path)$name[1]
  g <- sf::st_read(path, layer=lyr, quiet=TRUE)
  if(is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
  if(sf::st_crs(g)$epsg != 4326) g <- sf::st_transform(g, 4326)
  if("kd_kab" %in% names(g)) g <- g[g$kd_kab %in% study_kab_codes,]
  sf::st_make_valid(g)
}

build_districts <- function(g = load_admin_raw()) {
  gp <- sf::st_transform(g, 32748)
  dp <- gp |>
    dplyr::group_by(kabupaten_kota, kecamatan) |>
    dplyr::summarise(do_union=TRUE, .groups="drop") |>
    sf::st_make_valid()
  d <- sf::st_transform(dp, 4326)
  d$district_id <- seq_len(nrow(d))
  d$key <- paste(clean_key(d$kabupaten_kota), clean_key(d$kecamatan), sep="__")
  d
}

read_fault_kmz <- function(path = raw_paths$FAULT) {
  td <- tempfile("lembang_kmz_")
  dir.create(td)
  utils::unzip(path, exdir=td)
  kml <- list.files(td, pattern="\\.kml$", full.names=TRUE, recursive=TRUE, ignore.case=TRUE)
  if(!length(kml)) stop("No KML file found inside the Lembang Fault KMZ.")
  f <- sf::st_read(kml[1], quiet=TRUE)
  if(is.na(sf::st_crs(f))) sf::st_crs(f) <- 4326
  f <- sf::st_zm(f, drop=TRUE, what="ZM")
  sf::st_transform(sf::st_make_valid(f), 4326)
}

safe_sample <- function(x, n=5000) {
  if(nrow(x) <= n) return(x)
  x[sample.int(nrow(x), n)]
}

missingness_table <- function(x) {
  data.frame(
    variable=names(x),
    missing_n=vapply(x, function(z) sum(is.na(z)), integer(1)),
    missing_pct=vapply(x, function(z) mean(is.na(z))*100, numeric(1)),
    stringsAsFactors=FALSE
  ) |>
    dplyr::arrange(dplyr::desc(missing_pct))
}

numeric_summary <- function(x) {
  nums <- names(x)[vapply(x, is.numeric, logical(1))]
  if(!length(nums)) return(data.frame())
  dplyr::bind_rows(lapply(nums, function(v) {
    z <- x[[v]]
    z <- z[is.finite(z)]
    if(!length(z)) {
      return(data.frame(variable=v,n=0,mean=NA,sd=NA,min=NA,q25=NA,median=NA,q75=NA,max=NA))
    }
    data.frame(
      variable=v, n=length(z), mean=mean(z), sd=stats::sd(z),
      min=min(z), q25=as.numeric(stats::quantile(z,.25,names=FALSE)),
      median=stats::median(z), q75=as.numeric(stats::quantile(z,.75,names=FALSE)),
      max=max(z)
    )
  }))
}

aggregate_vs30_to_districts <- function(districts, vs30) {
  pts <- vs30[
    is.finite(vs30$longitude) & is.finite(vs30$latitude) &
    is.finite(vs30$vs30_mosaic) & vs30$vs30_mosaic > 0,]
  sfp <- sf::st_as_sf(pts, coords=c("longitude","latitude"), crs=4326, remove=FALSE)
  j <- sf::st_join(sfp, districts[,c("district_id")], join=sf::st_within, left=FALSE)
  a <- j |>
    sf::st_drop_geometry() |>
    dplyr::group_by(district_id) |>
    dplyr::summarise(
      Vs30_mean=mean(vs30_mosaic,na.rm=TRUE),
      Vs30_sd=stats::sd(vs30_mosaic,na.rm=TRUE),
      n_grid=dplyr::n(), .groups="drop"
    )
  dplyr::left_join(districts, a, by="district_id")
}

gmpe_proxy <- function(M, R, Vs30) {
  c0=-0.53804; c1=0.59850; c2=-0.01218
  c3=-1.30709; c4=0.10648; Mh=6.0
  Rref=1.0; c7=-0.36; Vref=760
  V1=225; V2=300; b1=-0.7; b2=-0.1

  dm <- ifelse(M > Mh, M-Mh, 0)
  FE <- c0 + c1*M + c2*M^2 + ifelse(M<=Mh,0,c4*dm)
  FP <- c3*log(pmax(R,1)/Rref) +
    ifelse(M<=Mh, c4*M*log(pmax(R,1)/Rref), c4*Mh*log(pmax(R,1)/Rref))
  FS_lin <- c7*log(Vs30/Vref)
  PGA_ref <- exp(FE+FP)
  f2 <- ifelse(
    Vs30<=V1,
    b1*log(pmax(PGA_ref,1e-8)/0.1),
    ifelse(
      Vs30<=V2,
      b1*log(pmax(PGA_ref,1e-8)/0.1) +
        b2*log(pmax(PGA_ref,1e-8)/0.1)*log(Vs30/V1)/log(V2/V1),
      0
    )
  )
  exp(FE+FP+FS_lin+f2)
}

compute_scenario_surface <- function(districts_vs30, fault_sf, mw=6.8, depth_km=10, threshold_g=.092) {
  d_utm <- sf::st_transform(districts_vs30, 32748)
  f_utm <- sf::st_transform(fault_sf, 32748)
  cent <- sf::st_centroid(d_utm)
  dm <- sf::st_distance(cent, f_utm)
  dist_km <- apply(as.matrix(dm),1,min)/1000
  R3 <- sqrt(dist_km^2 + depth_km^2)
  out <- districts_vs30
  out$Distance_to_fault_km <- dist_km
  out$PGA_scenario <- gmpe_proxy(mw, R3, out$Vs30_mean)
  out$Above_threshold <- out$PGA_scenario >= threshold_g
  out
}

join_bps_to_districts <- function(districts, bps, year, indicator) {
  b <- as.data.frame(bps)
  b <- b[b$Tahun == year, c("Kab/Kota","Kecamatan",indicator), drop=FALSE]
  b$key <- paste(clean_key(b[["Kab/Kota"]]), clean_key(b[["Kecamatan"]]), sep="__")
  names(b)[names(b)==indicator] <- "map_value"
  dplyr::left_join(districts, b[,c("key","map_value")], by="key")
}

add_basemap <- function(m, basemap) {
  cfg <- switch(
    basemap,
    sat=list(url="https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",attribution="Tiles &copy; Esri — Source: Esri, Maxar, Earthstar Geographics",max_zoom=19),
    topo=list(url="https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}",attribution="Tiles &copy; Esri — World Topographic Map",max_zoom=19),
    street=list(url="https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}",attribution="Tiles &copy; Esri — World Street Map",max_zoom=19),
    natgeo=list(url="https://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}",attribution="Tiles &copy; Esri — National Geographic",max_zoom=16),
    gray=list(url="https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",attribution="Tiles &copy; Esri — Light Gray Canvas",max_zoom=16),
    physical=list(url="https://server.arcgisonline.com/ArcGIS/rest/services/World_Physical_Map/MapServer/tile/{z}/{y}/{x}",attribution="Tiles &copy; Esri — World Physical Map",max_zoom=8),
    oceanmap=list(url="https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}",attribution="Tiles &copy; Esri — World Ocean Base",max_zoom=16),
    opentopo=list(url="https://a.tile.opentopomap.org/{z}/{x}/{y}.png",attribution="Map data &copy; OpenStreetMap contributors, SRTM | Map style &copy; OpenTopoMap",max_zoom=17),
    none=NULL,
    list(url="https://tile.openstreetmap.org/{z}/{x}/{y}.png",attribution="&copy; OpenStreetMap contributors",max_zoom=19)
  )
  if(is.null(cfg)) return(m)
  leaflet::addTiles(m,urlTemplate=cfg$url,attribution=cfg$attribution,options=leaflet::tileOptions(noWrap=TRUE,maxZoom=cfg$max_zoom))
}
