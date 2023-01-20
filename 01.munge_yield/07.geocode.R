library(tidyverse)
library(RJSONIO)


# Get latitude/longitude for towns near trial locations -------------------
locations <- read_csv("data/trial_data/metadata/munged_locations.csv", 
                      col_types = c("cnccccccc"))

# Identify unique places without latitude/longitude
noll <- filter(locations, is.na(Latitude) | is.na(Longitude)) %>%
  mutate(County = str_split(County, "; "), 
         Town = str_split(Town, "; ")) %>%
  unnest(c("County", "Town")) %>%
  distinct(State, County, Town) %>%
  mutate(URL_County = str_replace_all(County, " ", "%20"), 
         URL_Town = str_replace_all(Town, " ", "%20"), 
         URL_Town = if_else(State == "NEBRASKA" & Town == "COLUMBIA", 
                            "BLOOMFIELD", URL_Town), 
         URL_Town = if_else(Town %in% c("NORTHWEST AGRICULTURAL LABORATORY", 
                                        "BOX BUTTE EXPERIMENTAL STATION"), 
                            "ALLIANCE", URL_Town), 
         URL_State = if_else(Town == "TORRINGTON" & State == "NEBRAKSA", 
                             "WYOMING", State), 
         URL_State = if_else(Town == "MABEL" & State == "IOWA", "MINNESOTA", URL_State), 
         Latitude = as.numeric(NA), 
         Longitude = as.numeric(NA))


# Query the OpenStreetMap data through the nominatim API ------------------
### Uses stringent matching by town, county, and state
idx <- which(is.na(noll$Latitude))
for (i in seq_along(idx)) {
  cat(i, "/", length(idx), "\r")
  url <- paste("http://nominatim.openstreetmap.org/search?city=", 
               noll$URL_Town[idx[i]], "&county=", noll$URL_County[idx[i]], 
               "&state=", noll$URL_State[idx[i]], "&format=json", 
               "&email=amkusmec@iastate.edu", sep = "")
  
  tries <- 0
  repeat {
    x <- tryCatch(fromJSON(url), error = function(e) e)
    if (length(x) == 0 | tries >= 20) {
      if (length(x) == 0) {
        x <- list(list("lat" = Inf, "lon" = Inf))
      } else {
        x <- list(list("lat" = -Inf, "lon" = -Inf))
      }
      break
    } else if ("error" %in% class(x)) {
      tries <- tries + 1
    } else {
      break
    }
  }
  
  noll$Latitude[idx[i]] <- x[[1]]$lat
  noll$Longitude[idx[i]] <- x[[1]]$lon
}

# Check for errors
noll <- noll %>%
  mutate(across(Latitude:Longitude, as.numeric))
idx <- which(is.infinite(noll$Latitude))


# Save the results --------------------------------------------------------
locations <- locations %>%
  mutate(across(County:Latitude, ~ str_split(.x, "; "))) %>%
  unnest(c("County", "Town", "Planted", "Harvested", "Latitude", "Longitude"))

locA <- locations %>%
  filter(!(State == "IOWA" & Year >= 1955 & Year <= 2004), 
         is.na(Latitude) | is.na(Longitude)) %>%
  left_join(., noll, by = c("State", "County", "Town")) %>%
  select(-Latitude.x, -Longitude.x, -URL_County, -URL_Town) %>%
  rename(Latitude = Latitude.y, Longitude = Longitude.y) %>%
  mutate(across(Latitude:Longitude, as.character))

locB <- locations %>%
  filter(State == "IOWA", Year >= 1955, Year <= 2004) %>%
  left_join(., noll, by = c("State", "County", "Town")) %>%
  select(-Latitude.x, -Longitude.x, -URL_County, -URL_Town) %>%
  rename(Latitude = Latitude.y, Longitude = Longitude.y) %>%
  filter(!is.na(Planted), !is.na(Harvested)) %>%
  group_by(State, Year, Region) %>%
  summarise(across(County:Longitude, ~ paste(.x, collapse = "; ")), 
            .groups = "drop")

locC <- locations %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  mutate(across(Latitude:Longitude, as.character))

locations <- bind_rows(locA, locB, locC) %>%
  arrange(State, Year, Region, County, Town) %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  filter(!is.na(Planted), !is.na(Harvested))

write_csv(locations, "data/trial_data/metadata/locations_gps.csv")
