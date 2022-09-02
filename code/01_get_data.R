library(nwfscSurvey)
# library(sdmTMB)
library(dplyr)
# library(stringr)

# bring in common names
# UrlText <- "https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=Groundfish%20Slope%20and%20Shelf%20Combination%20Survey,date_dim$year>=2003&variables=common_name,scientific_name"
# DataPull <- try(jsonlite::fromJSON(UrlText))
catch <- nwfscSurvey::PullCatch.fn(SurveyName = "NWFSC.Combo")
# saveRDS(catch, "survey_data/wcbts_catch_2022-08-01.rds")
haul <- nwfscSurvey::PullHaul.fn(SurveyName = "NWFSC.Combo")
saveRDS(haul, "data/wcbts_haul.rds")
# bio <- nwfscSurvey::PullBio.fn(SurveyName = "NWFSC.Combo")
# saveRDS(bio, "survey_data/wcbts_bio_2022-08-01.rds")
# spec_names = group_by(DataPull, common_name) %>%
#  dplyr::summarize(scientific_name = scientific_name[1])
# spec_names$scientific_name = tolower(spec_names$scientific_name)
# saveRDS(spec_names,"nwfsc_lookup.rds")

spec_names <- readRDS("data/nwfsc_species_lookup.rds")

# Reformat names in catch data
names(catch) <- tolower(names(catch))
catch$date <- as.character(catch$date)
catch$trawl_id <- as.numeric(catch$trawl_id)

if ("scientific_name" %in% names(catch)) catch$scientific_name <- tolower(catch$scientific_name)
if ("common_name" %in% names(catch)) catch$common_name <- tolower(catch$common_name)

catch <- dplyr::left_join(catch, spec_names)

# WC names in cope and haltuch 2012
cope_haltuch <- c(
  "aurora rockfish", "big skate", "bigfin eelpout",
  "black eelpout", "brown cat shark", "california slickhead",
  "canary rockfish", "chilipepper", "darkblotched rockfish",
  "deepsea sole", "dover sole", "english sole", "giant grenadier",
  "greenstriped rockfish", "halfbanded rockfish", "lingcod",
  "longnose skate", "longspine thornyhead", "butterfish unident.",
  "pacific flatnose", "pacific grenadier", "pacific hake",
  "pacific sanddab", "petrale sole", "pink seaperch",
  "rex sole", "sablefish", "sandpaper skate", "sharpchin rockfish",
  "shortbelly rockfish", "shortspine thornyhead", "slender sole",
  "pacific spiny dogfish", "splitnose rockfish", "spotted ratfish",
  "stripetail rockfish", "white croaker", "yellowtail rockfish"
)

# these are additional species on the prioritization spreadsheet
fram <- c(
  cope_haltuch, "vermilion and sunset rockfish",
  "black rockfish", "cowcod", "copper rockfish",
  "brown rockfish", "quillback rockfish",
  "redbanded rockfish", "tree rockfish",
  "squarespot rockfish", "starry rockfish",
  "speckled rockfish", "rougheye and blackspotted rockfish",
  "shortraker rockfish", "flathead sole",
  "widow rockfish", "kelp greenling",
  "olive rockfish", "blue rockfish",
  "kelp rockfish", "cabezon",
  "sand sole", "flag rockfish",
  "starry flounder", "rock sole unident.",
  "greenspotted rockfish",
  "honeycomb rockfish",
  "California scorpionfish",
  "blackgill rockfish"
)
fram <- tolower(fram)

# filter catch data to only include these species
catch <- dplyr::filter(catch, common_name %in% fram)

# filter fields for smaller file size
catch <- dplyr::select(
  catch, trawl_id, common_name,
  year, date, depth_m, area_swept_ha,
  cpue_kg_km2,
  scientific_name
)
saveRDS(catch, "data/wcbts_catch.rds")
