#############################################
# maj_donnees_meteofrance.R
# Fonctions de mise à jour hors application pour météoFrance
# depuis shiny, le programme appelle maj_meteo_france(dir_data = "data")
# et si pb, on passe en manuel en lançant ce programme puis dans la console : maj_meteo_france()
#
# MAJ 2026-08 — Correctif ressource mensuelle :
#   Météo-France a réorganisé le jeu de données "SIM mensuelle"
#   (id 65e040c50a5c6872ebebc711). Les données sont désormais servies
#   en UN FICHIER PAR ANNÉE (MENS_SIM2_2026.csv.gz, MENS_SIM2_2025.csv.gz…)
#   hébergés sur un bucket OVH S3 (meteofrance.s3.sbg.io.cloud.ovh.net).
#   L'ancien identifiant de ressource figé renvoyait un 404.
#   -> On ne fige plus l'identifiant : on résout dynamiquement les fichiers
#      des années utiles depuis l'API du dataset.
#   Idem pour la partie décadaire : le jeu "SIM quotidienne" est passé au même
#      découpage (QUOT_SIM2_AAAA.csv.gz + QUOT_SIM2_latest.csv.gz) ; l'ancien
#      QUOT_SIM2_previous-2020 n'existe plus.
#   NB proxy : le domaine OVH doit être joignable (whitelist DSI si besoin).
#############################################

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(readr)
library(sf)
library(purrr)
library(lubridate)

# =========================================================
# Fonctions utilitaires
# =========================================================

convertir_nombre <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- dplyr::na_if(x, "")
  x <- dplyr::na_if(x, "NA")
  x <- stringr::str_replace_all(x, "\\s+", "")
  x <- stringr::str_replace_all(x, ",", ".")
  as.numeric(x)
}

make_square <- function(x, y, demi_cote = 4000) {
  sf::st_polygon(list(matrix(
    c(
      x - demi_cote, y - demi_cote,
      x + demi_cote, y - demi_cote,
      x + demi_cote, y + demi_cote,
      x - demi_cote, y + demi_cote,
      x - demi_cote, y - demi_cote
    ),
    ncol = 2,
    byrow = TRUE
  )))
}

# Vérifie l'existence d'un fichier (utilisée par la préparation déploiement).
verifier <- function(path) {
  if (!file.exists(path)) stop("Fichier introuvable : ", path, call. = FALSE)
}

# TRUE si le fichier source Météo-France est "roulant" : mis à jour EN PLACE
# sous le même nom (QUOT/MENS_SIM2_latest, ou fichier de l'ANNÉE EN COURS).
# Ces fichiers doivent être re-téléchargés à chaque exécution, sinon on relit une
# version périmée (ex. décade 2 jamais construite). Les années passées sont
# stables : leur cache local est conservé.
doit_rafraichir_source <- function(nom) {
  annee_courante <- as.integer(format(Sys.Date(), "%Y"))
  est_latest <- isTRUE(stringr::str_detect(nom, "(?i)_latest"))
  annee_fic  <- suppressWarnings(
    as.integer(stringr::str_match(nom, "(?i)SIM2_(\\d{4})")[, 2])
  )
  est_latest || (!is.na(annee_fic) && annee_fic >= annee_courante)
}

# =========================================================
# Nettoyage des anciens fichiers téléchargés
# =========================================================
# Supprime, dans dir_data, tous les fichiers correspondant aux motifs de
# fichiers sources bruts (MENS_SIM2_*, QUOT_SIM2_*, DECAD_SIM2_*) qui ne
# figurent pas dans la liste des fichiers à conserver (ceux qui viennent
# d'être utilisés avec succès). Les fichiers .rds produits pour
# l'application ne sont jamais concernés par cette fonction.

nettoyer_anciens_fichiers <- function(
    dir_data,
    fichiers_a_garder,
    motifs = c(
      "^MENS_SIM2_.*\\.csv\\.gz$",
      "^QUOT_SIM2_.*\\.csv\\.gz$",
      "^DECAD_SIM2_.*\\.csv\\.gz$"
    )
) {
  
  if (!dir.exists(dir_data)) {
    return(invisible(character(0)))
  }
  
  tous_fichiers <- list.files(dir_data, full.names = FALSE)
  
  fichiers_concernes <- tous_fichiers[
    purrr::map_lgl(
      tous_fichiers,
      ~ any(stringr::str_detect(.x, motifs))
    )
  ]
  
  fichiers_a_supprimer <- setdiff(fichiers_concernes, fichiers_a_garder)
  
  if (length(fichiers_a_supprimer) > 0) {
    message(
      "Nettoyage : suppression de ", length(fichiers_a_supprimer),
      " ancien(s) fichier(s) source(s) :"
    )
    for (f in fichiers_a_supprimer) {
      message("  - ", f)
      file.remove(file.path(dir_data, f))
    }
  } else {
    message("Nettoyage : aucun ancien fichier source à supprimer.")
  }
  
  invisible(fichiers_a_supprimer)
}

# =========================================================
# Météo-France mensuel
# =========================================================
# Résolution dynamique des ressources depuis l'API du dataset.
# L'identifiant du DATASET est stable ; ce sont les identifiants des
# RESSOURCES (fichiers) qui changent quand Météo-France republie.

id_dataset_mensuel <- "65e040c50a5c6872ebebc711"

# Renvoie les URL des fichiers MENS_SIM2_AAAA.csv.gz pour les années demandées
resoudre_url_mensuelles <- function(dataset_id = id_dataset_mensuel, annees_utiles) {
  
  url_api <- sprintf("https://www.data.gouv.fr/api/1/datasets/%s/", dataset_id)
  
  res <- httr::GET(url_api, httr::timeout(120))
  httr::stop_for_status(res)
  
  d <- jsonlite::fromJSON(
    httr::content(res, as = "text", encoding = "UTF-8"),
    flatten = TRUE
  )
  
  ress <- d$resources |>
    dplyr::filter(
      stringr::str_detect(url, "(?i)\\.csv\\.gz$"),
      stringr::str_detect(basename(url), "(?i)^MENS_SIM2_")
    ) |>
    dplyr::mutate(
      nom   = basename(url),
      annee = as.integer(stringr::str_match(nom, "(?i)MENS_SIM2_(\\d{4})")[, 2])
    )
  
  if (nrow(ress) == 0) {
    stop(
      "Aucune ressource MENS_SIM2 trouvée sur le dataset ", dataset_id,
      " — la structure a peut-être encore changé."
    )
  }
  
  sel <- ress |> dplyr::filter(annee %in% annees_utiles)
  
  if (nrow(sel) == 0) {
    stop(
      "Aucun fichier mensuel pour ", paste(annees_utiles, collapse = "/"),
      ".\nAnnées disponibles : ", paste(sort(unique(ress$annee)), collapse = ", ")
    )
  }
  
  message("Fichiers mensuels retenus : ", paste(sel$nom, collapse = ", "))
  sel$url
}

maj_meteo_mensuelle <- function(
    dir_data = "data",
    dataset_id = id_dataset_mensuel,
    force_download = TRUE,
    nettoyer = TRUE
) {
  
  if (!dir.exists(dir_data)) {
    dir.create(dir_data, recursive = TRUE)
  }
  
  N <- as.integer(format(Sys.Date(), "%Y"))
  annees_utiles <- c(N - 1, N)
  message("Années conservées (mensuel) : ", paste(annees_utiles, collapse = ", "))
  
  urls <- resoudre_url_mensuelles(dataset_id, annees_utiles)
  
  # --- Téléchargement (un fichier par année) ---
  chemins <- character(0)
  for (u in urls) {
    nom    <- basename(sub("\\?.*$", "", u))   # nettoie un éventuel ?param
    chemin <- file.path(dir_data, nom)
    
    if (doit_rafraichir_source(nom) || !file.exists(chemin) || isTRUE(force_download)) {
      message("Téléchargement : ", nom)
      httr::GET(
        u,
        httr::write_disk(chemin, overwrite = TRUE),
        httr::config(followlocation = TRUE),
        httr::timeout(600)
      ) |>
        httr::stop_for_status()
    } else {
      message("Fichier déjà présent : ", chemin)
    }
    
    chemins <- c(chemins, chemin)
  }
  
  # --- Lecture + fusion ---
  message("Lecture du/des fichier(s) mensuel(s)...")
  
  sim_mens <- purrr::map_dfr(chemins, ~ readr::read_csv2(
    .x,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8"),
    col_types = readr::cols_only(
      LAMBX = readr::col_character(),
      LAMBY = readr::col_character(),
      DATE  = readr::col_character(),
      SSWI1 = readr::col_character()
    )
  ))
  
  sim_mens <- sim_mens |>
    dplyr::mutate(
      LAMBX = convertir_nombre(LAMBX),
      LAMBY = convertir_nombre(LAMBY),
      SSWI1 = convertir_nombre(SSWI1),
      DATE  = as.character(DATE),
      ANNEE = as.integer(substr(DATE, 1, 4))
    ) |>
    dplyr::filter(
      ANNEE %in% annees_utiles,
      !is.na(LAMBX),
      !is.na(LAMBY),
      !is.na(SSWI1),
      !is.na(DATE)
    ) |>
    dplyr::select(-ANNEE)
  
  message("Nombre de lignes mensuelles conservées : ", nrow(sim_mens))
  message("Dates mensuelles disponibles :")
  print(sort(unique(sim_mens$DATE)))
  
  message("Création des carrés météo mensuels...")
  
  geom <- purrr::map2(
    sim_mens$LAMBX * 100,
    sim_mens$LAMBY * 100,
    make_square
  )
  
  sf_meteo_carres <- sim_mens |>
    dplyr::select(DATE, SSWI1) |>
    sf::st_as_sf(
      geometry = sf::st_sfc(geom, crs = 27572)
    ) |>
    sf::st_transform(2154)
  
  fichier_sortie <- file.path(dir_data, "sf_meteo_carres.rds")
  
  saveRDS(
    sf_meteo_carres,
    file = fichier_sortie
  )
  
  message("Fichier créé : ", fichier_sortie)
  message(
    "Années disponibles mensuelles : ",
    paste(sort(unique(substr(sf_meteo_carres$DATE, 1, 4))), collapse = ", ")
  )
  
  message("Résumé SSWI1 :")
  print(summary(sf_meteo_carres$SSWI1))
  
  if (isTRUE(nettoyer)) {
    nettoyer_anciens_fichiers(
      dir_data = dir_data,
      fichiers_a_garder = basename(chemins),
      motifs = "^MENS_SIM2_.*\\.csv\\.gz$"
    )
  }
  
  invisible(fichier_sortie)
}

# =========================================================
# Météo-France décadaire à partir de SIM quotidienne
# =========================================================
# Cette partie résout déjà dynamiquement les ressources depuis le slug du
# dataset. followlocation ajouté pour suivre les redirections de stockage.

telecharger_sim_quotidien <- function(ressource, dir_data = "data") {
  
  url <- ressource$url[1]
  nom <- basename(url)
  chemin <- file.path(dir_data, nom)
  
  message("Fichier sélectionné : ", nom)
  
  if (doit_rafraichir_source(nom) || !file.exists(chemin)) {
    message("Téléchargement : ", nom)
    
    httr::GET(
      url,
      httr::write_disk(chemin, overwrite = TRUE),
      httr::config(followlocation = TRUE),
      httr::timeout(600)
    ) |>
      httr::stop_for_status()
    
  } else {
    message("Fichier déjà présent (année stable) : ", chemin)
  }
  
  chemin
}

lire_sim_quotidien <- function(path) {
  readr::read_csv2(
    path,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8"),
    col_types = readr::cols_only(
      LAMBX    = readr::col_character(),
      LAMBY    = readr::col_character(),
      DATE     = readr::col_character(),
      SSWI_10J = readr::col_character()
    )
  )
}

maj_meteo_decadaire <- function(dir_data = "data", nettoyer = TRUE) {
  
  if (!dir.exists(dir_data)) {
    dir.create(dir_data, recursive = TRUE)
  }
  
  url_api <- "https://www.data.gouv.fr/api/1/datasets/donnees-changement-climatique-sim-quotidienne/"
  
  res <- httr::GET(url_api, httr::timeout(120))
  httr::stop_for_status(res)
  
  dataset <- jsonlite::fromJSON(
    httr::content(res, as = "text", encoding = "UTF-8"),
    flatten = TRUE
  )
  
  N <- as.integer(format(Sys.Date(), "%Y"))
  annees_utiles <- c(N - 1, N)
  
  # Le jeu quotidien est lui aussi passé à UN FICHIER PAR ANNÉE
  # (QUOT_SIM2_2026.csv.gz, QUOT_SIM2_2025.csv.gz…) + un QUOT_SIM2_latest.csv.gz
  # (jours récents glissants). On récupère les années utiles + latest, puis on
  # dédoublonne au jour/point de grille plus bas (distinct DATE_JOUR/LAMBX/LAMBY).
  ressources_csv <- dataset$resources |>
    dplyr::filter(stringr::str_detect(url, "\\.csv\\.gz$")) |>
    dplyr::mutate(
      nom        = basename(url),
      date_maj   = as.POSIXct(last_modified, tz = "UTC"),
      annee      = as.integer(stringr::str_match(nom, "(?i)QUOT_SIM2_(\\d{4})")[, 2]),
      est_latest = stringr::str_detect(nom, "(?i)QUOT_SIM2_latest")
    )
  
  ressources_a_telecharger <- ressources_csv |>
    dplyr::filter(annee %in% annees_utiles | est_latest) |>
    dplyr::distinct(nom, .keep_all = TRUE)
  
  if (nrow(ressources_a_telecharger) == 0) {
    stop(
      "Aucun fichier QUOT_SIM2 pour ", paste(annees_utiles, collapse = "/"),
      ".\nFichiers disponibles :\n",
      paste(ressources_csv$nom, collapse = "\n")
    )
  }
  
  message(
    "Fichiers quotidiens retenus : ",
    paste(ressources_a_telecharger$nom, collapse = ", ")
  )
  
  chemins_quot <- purrr::map_chr(
    seq_len(nrow(ressources_a_telecharger)),
    ~ telecharger_sim_quotidien(ressources_a_telecharger[.x, ], dir_data)
  )
  
  message("Lecture des fichiers quotidiens...")
  
  sim_quotidien <- purrr::map_dfr(chemins_quot, lire_sim_quotidien) |>
    dplyr::mutate(
      DATE_JOUR = as.Date(DATE, format = "%Y%m%d"),
      ANNEE = lubridate::year(DATE_JOUR),
      LAMBX = convertir_nombre(LAMBX),
      LAMBY = convertir_nombre(LAMBY),
      SSWI_10J = convertir_nombre(SSWI_10J)
    ) |>
    dplyr::filter(
      ANNEE %in% annees_utiles,
      !is.na(DATE_JOUR),
      !is.na(LAMBX),
      !is.na(LAMBY),
      !is.na(SSWI_10J)
    ) |>
    dplyr::distinct(DATE_JOUR, LAMBX, LAMBY, .keep_all = TRUE)
  
  message("Nombre de lignes quotidiennes conservées : ", nrow(sim_quotidien))
  
  date_du_jour <- Sys.Date()
  
  sim_decade <- sim_quotidien |>
    dplyr::mutate(
      JOUR = lubridate::day(DATE_JOUR),
      DATE = format(DATE_JOUR, "%Y%m"),
      DECADE = dplyr::case_when(
        JOUR <= 10 ~ "1",
        JOUR <= 20 ~ "2",
        TRUE       ~ "3"
      ),
      DATE_FIN_DECADE = dplyr::case_when(
        DECADE == "1" ~ as.Date(
          paste0(format(DATE_JOUR, "%Y-%m"), "-10")
        ),
        DECADE == "2" ~ as.Date(
          paste0(format(DATE_JOUR, "%Y-%m"), "-20")
        ),
        DECADE == "3" ~ lubridate::ceiling_date(
          DATE_JOUR,
          unit = "month"
        ) - lubridate::days(1)
      )
    ) |>
    dplyr::group_by(DATE, DECADE, LAMBX, LAMBY) |>
    dplyr::filter(max(DATE_JOUR, na.rm = TRUE) >= first(DATE_FIN_DECADE)) |>
    dplyr::arrange(DATE_JOUR, .by_group = TRUE) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::rename(SWI = SSWI_10J) |>
    dplyr::select(DATE, DECADE, LAMBX, LAMBY, SWI)
  
  message("Nombre de lignes décadaires conservées : ", nrow(sim_decade))
  message("Création des carrés météo décadaires...")
  
  geom <- purrr::map2(
    sim_decade$LAMBX * 100,
    sim_decade$LAMBY * 100,
    make_square
  )
  
  sf_meteo_carres_decade <- sim_decade |>
    dplyr::select(DATE, DECADE, SWI) |>
    sf::st_as_sf(
      geometry = sf::st_sfc(geom, crs = 27572)
    ) |>
    sf::st_transform(2154)
  
  fichier_sortie <- file.path(dir_data, "sf_meteo_carres_decade.rds")
  
  saveRDS(
    sf_meteo_carres_decade,
    file = fichier_sortie
  )
  
  message("Fichier créé : ", fichier_sortie)
  message(
    "Années disponibles décadaires : ",
    paste(sort(unique(substr(sf_meteo_carres_decade$DATE, 1, 4))), collapse = ", ")
  )
  
  message("Résumé SWI :")
  print(summary(sf_meteo_carres_decade$SWI))
  
  if (isTRUE(nettoyer)) {
    nettoyer_anciens_fichiers(
      dir_data = dir_data,
      fichiers_a_garder = basename(chemins_quot),
      motifs = c("^QUOT_SIM2_.*\\.csv\\.gz$", "^DECAD_SIM2_.*\\.csv\\.gz$")
    )
  }
  
  invisible(fichier_sortie)
}

normaliser_mois_deploiement <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_character_, length(x))
  
  ok <- grepl("^[0-9]{6}$", x)
  out[ok] <- x[ok]
  
  ok <- is.na(out) & grepl("^[0-9]{4}-[0-9]{2}$", x)
  out[ok] <- gsub("-", "", x[ok])
  
  ok <- is.na(out) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)
  out[ok] <- format(as.Date(x[ok]), "%Y%m")
  
  out
}

restreindre_un_pas <- function(x, france) {
  if (!inherits(x, "sf")) stop("Objet Météo-France non sf.", call. = FALSE)
  if (is.na(st_crs(x))) stop("CRS absent sur la grille Météo-France.", call. = FALSE)
  
  fr <- st_transform(france, st_crs(x))
  fr <- st_make_valid(fr)
  fr <- st_union(fr)
  
  # Carrés entiers qui touchent la France : beaucoup moins coûteux qu'un
  # st_intersection(), et suffisant pour l'affichage national/régional.
  keep <- lengths(suppressWarnings(st_intersects(x, fr))) > 0
  x <- x[keep, , drop = FALSE]
  
  # Reprojection une fois pour toutes pour Leaflet.
  st_transform(x, 4326)
}




preparer_donnees_deploiement_meteofrance <- function(dir_data = "data") {
  
  message("========================================")
  message("Préparation automatique pour Shiny")
  message("========================================")
  
  DIR_DATA <- dir_data
  FICHIER_MENSUEL <- file.path(DIR_DATA, "sf_meteo_carres.rds")
  FICHIER_DECADE  <- file.path(DIR_DATA, "sf_meteo_carres_decade.rds")
  FICHIER_FRANCE  <- file.path(DIR_DATA, "france_metropole.rds")
  
  DIR_MENSUEL_OUT <- file.path(DIR_DATA, "mf_mensuel")
  DIR_DECADE_OUT  <- file.path(DIR_DATA, "mf_decade")
  
  dir.create(DIR_MENSUEL_OUT, recursive = TRUE, showWarnings = FALSE)
  dir.create(DIR_DECADE_OUT,  recursive = TRUE, showWarnings = FALSE)
  
  verifier(FICHIER_MENSUEL)
  verifier(FICHIER_DECADE)
  verifier(FICHIER_FRANCE)
  
  france <- readRDS(FICHIER_FRANCE)
  if (!inherits(france, "sf") && !inherits(france, "sfc")) {
    stop("france_metropole.rds doit contenir un objet sf ou sfc.", call. = FALSE)
  }
  if (inherits(france, "sfc")) france <- sf::st_sf(geometry = france)
  france <- sf::st_make_valid(france)
  france <- sf::st_union(france)
  france <- sf::st_sf(geometry = france)
  
  # ---- Mensuel
  message("Lecture du fichier mensuel...")
  mf <- readRDS(FICHIER_MENSUEL)
  if (!inherits(mf, "sf")) stop("sf_meteo_carres.rds n'est pas un objet sf.")
  if (!all(c("DATE", "SSWI1") %in% names(mf))) {
    stop("Le fichier mensuel doit contenir DATE et SSWI1.")
  }
  
  mois <- normaliser_mois_deploiement(mf$DATE)
  if (anyNA(mois)) {
    stop("Certaines valeurs DATE mensuelles n'ont pas pu être normalisées.")
  }
  
  sorties_mensuelles <- character(0)
  
  for (m in sort(unique(mois))) {
    message("Mensuel : ", m)
    
    idx <- mois == m
    geom_col <- attr(mf, "sf_column")
    x <- mf[idx, c("DATE", "SSWI1", geom_col), drop = FALSE]
    
    x$DATE <- m
    x$SSWI1 <- suppressWarnings(as.numeric(x$SSWI1))
    x <- restreindre_un_pas(x, france)
    
    sortie <- file.path(DIR_MENSUEL_OUT, paste0("mf_mensuel_", m, ".rds"))
    saveRDS(x, sortie, compress = "xz")
    sorties_mensuelles <- c(sorties_mensuelles, sortie)
    
    message("  -> ", sortie, " (", nrow(x), " carrés)")
    
    rm(x)
    gc(verbose = FALSE)
  }
  
  # Nettoyage des anciens fichiers mensuels préparés
  existants_m <- list.files(
    DIR_MENSUEL_OUT,
    pattern = "^mf_mensuel_[0-9]{6}\\.rds$",
    full.names = TRUE
  )
  anciens_m <- setdiff(
    normalizePath(existants_m, winslash = "/", mustWork = FALSE),
    normalizePath(sorties_mensuelles, winslash = "/", mustWork = FALSE)
  )
  if (length(anciens_m)) unlink(anciens_m)
  
  rm(mf, mois)
  gc(verbose = FALSE)
  
  # ---- Décadaire
  message("Lecture du fichier décadaire...")
  md <- readRDS(FICHIER_DECADE)
  if (!inherits(md, "sf")) stop("sf_meteo_carres_decade.rds n'est pas un objet sf.")
  if (!all(c("DATE", "DECADE") %in% names(md))) {
    stop("Le fichier décadaire doit contenir DATE et DECADE.")
  }
  
  col_val <- if ("SSWI_10J" %in% names(md)) {
    "SSWI_10J"
  } else if ("SWI" %in% names(md)) {
    "SWI"
  } else if ("SSWI_10J_APP" %in% names(md)) {
    "SSWI_10J_APP"
  } else {
    stop("Aucune colonne SSWI_10J / SWI trouvée dans le fichier décadaire.")
  }
  
  mois_d <- normaliser_mois_deploiement(md$DATE)
  dec_d <- as.character(md$DECADE)
  
  if (anyNA(mois_d)) stop("Certaines DATE décadaires n'ont pas pu être normalisées.")
  if (any(!dec_d %in% c("1", "2", "3"))) stop("DECADE doit valoir 1, 2 ou 3.")
  
  cles <- paste(mois_d, dec_d, sep = "_")
  sorties_decades <- character(0)
  
  for (cle in sort(unique(cles))) {
    
    morceaux <- strsplit(cle, "_", fixed = TRUE)[[1]]
    m <- morceaux[1]
    d <- morceaux[2]
    
    message("Décade : ", m, " / ", d)
    
    idx <- cles == cle
    geom_col <- attr(md, "sf_column")
    x <- md[idx, c("DATE", "DECADE", col_val, geom_col), drop = FALSE]
    
    x$DATE <- m
    x$DECADE <- d
    x$SSWI_10J_APP <- suppressWarnings(as.numeric(x[[col_val]]))
    
    if (col_val != "SSWI_10J_APP") x[[col_val]] <- NULL
    
    x <- restreindre_un_pas(x, france)
    
    sortie <- file.path(
      DIR_DECADE_OUT,
      paste0("mf_decade_", m, "_", d, ".rds")
    )
    
    saveRDS(x, sortie, compress = "xz")
    sorties_decades <- c(sorties_decades, sortie)
    
    message("  -> ", sortie, " (", nrow(x), " carrés)")
    
    rm(x)
    gc(verbose = FALSE)
  }
  
  # Nettoyage des anciens fichiers décadaires préparés
  existants_d <- list.files(
    DIR_DECADE_OUT,
    pattern = "^mf_decade_[0-9]{6}_[123]\\.rds$",
    full.names = TRUE
  )
  anciens_d <- setdiff(
    normalizePath(existants_d, winslash = "/", mustWork = FALSE),
    normalizePath(sorties_decades, winslash = "/", mustWork = FALSE)
  )
  if (length(anciens_d)) unlink(anciens_d)
  
  rm(md, mois_d, dec_d, cles)
  gc(verbose = FALSE)
  
  message("========================================")
  message("Préparation Shiny terminée ✅")
  message("========================================")
  
  invisible(list(
    mensuel = sorties_mensuelles,
    decade = sorties_decades
  ))
}


# =========================================================
# Fonction principale Météo-France
# =========================================================

maj_meteo_france <- function(dir_data = "data", nettoyer = TRUE) {
  
  message("========================================")
  message("Mise à jour Météo-France")
  message("========================================")
  
  fichier_mensuel <- maj_meteo_mensuelle(
    dir_data = dir_data,
    nettoyer = nettoyer
  )
  
  fichier_decade <- maj_meteo_decadaire(
    dir_data = dir_data,
    nettoyer = nettoyer
  )
  
  # Étape automatique : création des petits fichiers optimisés
  # utilisés par l'application Shiny en ligne.
  fichiers_deploiement <- preparer_donnees_deploiement_meteofrance(
    dir_data = dir_data
  )
  
  message("========================================")
  message("Mise à jour Météo-France terminée ✅")
  message("Données Shiny prêtes ✅")
  message("========================================")
  
  invisible(list(
    mensuel = fichier_mensuel,
    decade = fichier_decade,
    deploiement = fichiers_deploiement
  ))
}

# =========================================================
# Exécution automatique uniquement lorsque ce fichier
# est lancé directement avec Rscript.exe
# =========================================================

if (sys.nframe() == 0) {
  maj_meteo_france()
}