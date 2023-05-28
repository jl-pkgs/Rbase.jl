# Copyright (c) 2022 Dongdong Kong. All rights reserved.
# source('scripts/main_pkgs.R')
require2 <- function(pkg) {
  pkg = deparse(substitute(pkg))
  if (!require(pkg, character.only=TRUE, quietly=TRUE)) {
    install.packages(pkg)
    require(pkg, character.only = TRUE, quietly = TRUE)
  }
}

require2(sf)
require2(terra)
require2(exactextractr)
require2(data.table)
require2(ncdf4)
require2(magrittr)
library(sf2)
# library(nctools)

fix_range <- function(range, cellsize = 1, mid=TRUE) {
  if (length(cellsize) == 1) cellsize <- rep(cellsize, 2)
  if (length(mid) == 1) mid <- rep(mid, 2)

  cell_x = cellsize[1]
  cell_y = cellsize[2]
  range_x <- range[1:2] + c(-1, 1) * cell_x / 2 * !mid[1]
  range_y <- range[3:4] + c(-1, 1) * cell_y / 2 * !mid[2]
  range <- c(range_x, range_y)
  range
}

make_rast <- function(range = c(-180, 180, -90, 90), cellsize = 1, mid = TRUE, nlyrs = 1, ...) {
  range %<>% fix_range(cellsize, mid)
  e <- ext(range[1], range[2], range[3], range[4])
  rast(e, res = cellsize, nlyrs = nlyrs, ...)
}

ncread <- function(file, varnames = 1L, ...) {
  fid <- nc_open(file)
  on.exit(nc_close(fid))
  # varnames %<>% guess_varnames(fid, .)
  ncvar_get(fid, varnames, ...)
}

guess_range <- function(f, mid = TRUE) {
  lon <- ncread(f, "lon")
  lat <- ncread(f, "lat")
  
  cell_x <- median(diff(lon))
  cell_y <- median(diff(lat))
  cellsize = c(cell_x, cell_y)
  
  fix_range(c(range(lon), range(lat)), cellsize, mid)
}

make_gridFractArea <- function(r, poly, plot = FALSE) {
  fract <- exactextractr::coverage_fraction(r, poly)[[1]]
  area <- cellSize(fract) / 1e6 # m^2 to km^2
  r2 <- c(fract, area) %>% setNames(c("fract", "area"))

  if (plot) {
    terra::plot(r2)
  }
  info <- as.data.frame(r2, xy = TRUE) %>%
    as.data.table() %>%
    .[order(y, x), ] %>% # this step vital
    cbind(I = 1:nrow(.), .) %>%
    dplyr::mutate(area2 = fract * area) %>%
    .[area2 > 0]
  info
}


get_model <- function(file, prefix = "_", postfix = "_") {
  pattern <- sprintf("(?<=%s).*(?=%s)", prefix, postfix)
  stringr::str_extract(basename(file), pattern)
}

ChinaRegionMean <- function(f) {
  r2 <- exactextractr::exact_extract(terra::rast(f), sf2::bou1_4p,
    "weighted_mean", weights = "area")
  as.numeric(r2)
}

## yearly data
get_ChinaRegionalMean <- function(fs) {
  models <- get_model(fs, prefix = "_year_", postfix = "_his|_ssp")
  fs %<>% set_names(models)

  res <- foreach(f = fs, i = icount()) %do% {
    date <- nctools::nc_date(f)

    print(basename(f))
    # r1 = sf.extract::st_extract(f, bou1_4p)
    # v1 = r1[[1]][, -1] %>% as.numeric()
    r2 <- exactextractr::exact_extract(terra::rast(f), sf2::bou1_4p,
      "weighted_mean",
      weights = "area"
    )
    value <- as.numeric(r2)
    d <- data.table(date, value)
  }
  melt_list(res, "model")
}

extract_info <- get_ChinaRegionalMean
## 3d array, convert into rast?
