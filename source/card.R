#--- Script details ------------------------------------------------------------
# Creation date: 12 December 2022
# Client:        client
# Project:       christmas-card-2022
# Description:   script description
# Author:        Nick Twort

library(tidyverse)
library(lubridate)
library(magrittr)
library(here)
library(janitor)
library(sf)
library(gganimate)
library(ggmap)

#--- Import data ---------------------------------------------------------------

dat <- read_delim("data/raw/2938231_christmas_card_2022_expanded_cel_cdl_report/2938231_christmas_card_2022_expanded_cel_cdl_report_cel.tsv.gz")

#--- Process data --------------------------------------------------------------

dat <- dat |> 
    clean_names() |> 
    filter(!is.na(common_evening_lat)) |> 
    st_as_sf(coords = c("common_evening_long", "common_evening_lat"), crs = 4326)

bbox <- st_polygon(x = list(matrix(data = c(150.08302179608586, -33.330815162241194,
                                            150.08302179608586, -34.38762884730629,
                                            151.45025566699192, -34.38762884730629,
                                            151.45025566699192, -33.330815162241194,
                                            150.08302179608586, -33.330815162241194), nrow = 5, ncol = 2, byrow = TRUE)))

grid <- st_make_grid(bbox, square = FALSE, n = 20) |> 
    st_set_crs(4326)

grid <- grid |>
    as_tibble() |>
    st_sf()

cc_data <- grid |> 
    st_join(dat, join = st_contains) |> 
    filter(!is.na(polygon_id)) |> 
    group_by(geometry) |> 
    summarise(n = n(), .groups = "drop")

map_data <- cc_data |>
    st_centroid() |> 
    st_transform(3577) |>
    mutate(geometry = st_buffer(geometry, sqrt(n) * 3000)) |> 
    st_transform(4326) |> 
    mutate(fill = sample(c('lawngreen', 'gold', 'white', 'orchid', 'royalblue', 'yellow', 'orange'), n(), replace = TRUE))

basemap <- get_stamenmap(unname(st_bbox(bbox)), zoom = 10, maptype = "toner-lite", crop = FALSE)

md <- map_data |>
    mutate(rn = sample(1:7, size = n(), replace = TRUE),
           transition_length = sample(3:6, size = n(), replace = TRUE)) |> 
    arrange(rn) |> 
    mutate(in_frame = (rn - 1) * 3 + 1, out_frame = in_frame + transition_length) |> 
    mutate(id = row_number())

mdm <- md |> filter(FALSE)

max_frame <- max(md$out_frame + md$transition_length)

for (i in 1:nrow(md)) {
    tlength <- md$transition_length[i]
    cur_size <- st_area(md$geometry[i])
    n <- md$n[i]
    
    cent <- st_centroid(md$geometry[i])
    
    new_df <- md |> filter(row_number() == -1)
    
    size_zero <- md |> 
        filter(row_number() == i) |> 
        st_centroid() |> 
        st_transform(3577) |>
        st_buffer(0.0001) |> 
        st_transform(4326)
    
    for (j in 1:tlength) {
        
        new_area <- j / tlength
        
        new_cent <- cent |> 
            st_transform(3577) |>
            st_buffer(sqrt(n) * 3000 * new_area) |> 
            st_transform(4326)
        
        new_df <- bind_rows(
            md |> filter(row_number() == i) |> mutate(geometry = new_cent, frame_id = in_frame + j - 1),
            md |> filter(row_number() == i) |> mutate(geometry = new_cent, frame_id = in_frame + tlength * 2 - j - 1),
            new_df
            )
        
    }
    
    new_df <- distinct(new_df)
    
    if (md$in_frame[1] > 1) {
        for (j in 1:(md$in_frame[1] - 1)) {
            new_df <- bind_rows(
                size_zero |> mutate(frame_id = j),
                new_df
            )
        }
    }
    
    mdm <- bind_rows(
        mdm, 
        new_df
    )
    
}


p <- basemap |> 
    ggmap() +
    geom_sf(data = mdm, aes(group = seq_along(id), fill = fill, colour = fill), alpha = 0.6, linewidth = 0, inherit.aes = FALSE) +
    scale_fill_identity(aesthetics = c("colour", "fill")) +
    theme_void() +
    transition_time(time = frame_id)
   
pa <- animate(p, width = 960, height = 960)
 
anim_save("outputs/cc_animation_2022.gif", pa)
