



make_precinct_map <- function(district, show_district_layer = FALSE) {
  
  if(district != "pdx") {
    osm_data <- get(str_c("osm_tiles_d", district))
    d_filter <- district
    title_label <- str_c("District ", d_filter)
    
  } else if(district == "pdx") {
    osm_data <- get(str_c("osm_tiles_pdx"))
    d_filter <- c("1", "2", "3", "4")
    title_label <- ""
  }
  
  portland_dist_precincts_d <- portland_dist_precincts |> 
    filter(DISTRICT %in% d_filter)
  
  portland_council_districts_d <- portland_council_districts |> 
    filter(DISTRICT %in% d_filter)
  
  n_precincts <- portland_dist_precincts_d |> 
    select(id_number) |> 
    distinct() |> 
    nrow()
  
  my_colors <- rainbow(n_precincts)
  
  # 2. Shuffle them randomly
  #set.seed(42) # Keeps the shuffle the same every time you run it
  shuffled_palette <- sample(my_colors)
  
  precinct_map <- portland_dist_precincts_d |> 
    ggplot() + 
    labs(title = "Precinct Map",
         subtitle = title_label) +
    geom_spatraster_rgb(data = osm_data, alpha = 0.7) +
    coord_sf(crs = 4269) +
    geom_sf(aes(fill = factor(PRECINCTID)), alpha = 0.4)  +
    scale_fill_manual(values = shuffled_palette) + 
    theme_minimal() +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size = 20))
  
  if(show_district_layer) {
    precinct_map <- precinct_map + 
      geom_sf(data=portland_council_districts_d, fill=NA, color = "black", linewidth = 1.2) +
      geom_sf_text(data=portland_council_districts_d, aes(label = DISTRICT), size=12, fontface = "bold", size= 5) 
  } else {
    precinct_map <- precinct_map + 
      geom_sf_text(aes(label = PRECINCTID_PRINT), size=2, show.legend = FALSE)  
  }
  
  precinct_map <- precinct_map + 
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.title = element_text(size = 15)
      
  )
  
  
  
  precinct_map
  
}


make_precinct_rank_graph <- function(office_label, candidate, show_labels = TRUE) {
  
  district_number <- str_sub(office_label, start = -1)
  osm_tiles <- get(str_c("osm_tiles_", office_label))
  
  if(office_label == "pdx") {
    ranks_values <- get_ranks("mayor", candidate, nseats = 1) |> 
      mutate(ranks_factor = factor(ranks, 
                                   levels = c(0, 1), 
                                   labels = c("Not Elected", "Elected")))
    portland_dist_precincts_d <- portland_dist_precincts
    my_colors <- c("Not Elected" = "red", "Elected" = "forestgreen")
    breaks_vals = c("Not Elected", "Elected")
    subtitle_string <- str_c("Mayor:  ",candidate)
    
  } else {
    
    ranks_values <- get_ranks(office_label, candidate) |> 
      mutate(ranks_factor = factor(ranks, 
                                   levels = c(0, 1, 2, 3), 
                                   labels = c("Not Elected", "First", "Second", "Third")))
    
    portland_dist_precincts_d <- portland_dist_precincts |> 
      filter(DISTRICT == district_number)
    my_colors <- c("Not Elected" = "red", "First" = "forestgreen", "Second" = "springgreen1", "Third" = "darkseagreen1")
    breaks_vals = c("Not Elected", "First", "Second", "Third")
    subtitle_string <- str_c("District ", district_number,":  ",candidate)
  }
  
  
  
  
  d_layer_1 <- portland_dist_precincts_d |> 
    left_join(ranks_values, by=join_by(PRECINCTID == precinct_1)) |>
    mutate(rank_print = ifelse(precinct_area <= 4e+05,
                               "",
                               ranks))
  
  d_layer_1 |> ggplot() + 
    labs(title = "Simulated Elections by Precinct",
         subtitle = subtitle_string)  +
    geom_spatraster_rgb(data = osm_tiles, alpha = 1.0) +
    #coord_sf(crs = 4269) +
    geom_sf(aes(fill = ranks_factor), alpha = 0.5, color = "white")  +
    coord_sf(datum = NA) +
    
    scale_fill_manual(name = "",
                      breaks = breaks_vals,
                      values = my_colors, 
                      na.translate = FALSE) +
    
    guides(fill = guide_legend(override.aes = list(alpha = 0.5))) + 
    #geom_sf_text(aes(label = PRECINCTID_PRINT), color = "black", size = 2) +
    if (show_labels) geom_sf_text(aes(label = PRECINCTID_PRINT), color = "black", size = 3) else list() +
    
    theme(
      axis.text = element_blank(),
      axis.text.x = element_blank(),   # Hides x-axis text labels
      axis.text.y = element_blank(),   # Hides y-axis text labels
      axis.ticks.x = element_blank(),  # Hides x-axis ticks
      axis.ticks.y = element_blank(),  # Hides y-axis ticks
      axis.title = element_blank(),
      axis.title.x = element_blank(),  # Hides x-axis title
      axis.title.y = element_blank(),   # Hides y-axis title
      panel.grid = element_blank(),    # Removes background grid lines
      plot.title = element_text(size = 15)
      
    )
  
  
}  


percent_by_precinct_graphs <- function(office_label, candidate_vector, show_labels = TRUE) {
  
  
  office_input <- switch(office_label,
                         "d1" = get("council_d1_2024"),
                         "d2" = get("council_d2_2024"),
                         "d3" = get("council_d3_2024"),
                         "d4" = get("council_d4_2024"),
                         "pdx" = get("mayor_2024")
  )
  


  table_1 <- make_ballots_by_precinct_table(office_input, candidate_vector)
  district_number <- str_sub(office_label, start = -1)
  osm_tiles <- get(str_c("osm_tiles_", office_label))
  
  
  if(office_label != "pdx") {
    portland_dist_precincts_d <- portland_dist_precincts |> 
      filter(DISTRICT == district_number)
  } else {
    portland_dist_precincts_d <- portland_dist_precincts
    
  }


  d_layer_1 <- portland_dist_precincts_d |> 
    left_join(table_1, by=join_by(PRECINCTID == precinct_1)) |>
    
    mutate(pct_print = ifelse(precinct_area <= 4e+05,
                              "",
                              any_rank_pct)) |> 
    
    mutate(text_print_color = ifelse(any_rank_pct <= .6 | is.na(any_rank_pct),
                                     "low",
                                     "high"))

  table_2 <- table_1 
  
  
  med_value <- median(table_2$any_rank_pct, na.rm=TRUE)
  min_value <- min(table_2$any_rank_pct, na.rm=TRUE)
  max_value <- max(table_2$any_rank_pct, na.rm=TRUE)
  
  d_layer_1 <- portland_dist_precincts_d |> 
    left_join(table_1, by=join_by(PRECINCTID == precinct_1)) |>
    mutate(pct_print = ifelse(precinct_area <= 4e+05,
                              "",
                              round(any_rank_pct*100,1))) |> 
    mutate(text_print_color = ifelse(any_rank_pct <= med_value | is.na(any_rank_pct),
                                     "low",
                                     "high"))
  

  
  d_layer_1 |> ggplot() + 
    labs(title = "Percent of Ballots with Ranking",
         subtitle = str_c(candidate_vector,": ranked anywhere from 1 to 6"))  +
    geom_spatraster_rgb(data = osm_tiles, alpha = 1.0) +
    coord_sf(crs = 4269) +
    geom_sf(aes(fill = round(any_rank_pct*100,0)), alpha = 0.9, color = "white")  +
    labs(fill = "Percent") +
    coord_sf(datum = NA) +
    scale_fill_viridis_c(option = "mako", limits = c(min(min_value*100 - 1), max_value*100 + 1)) +
    #if (show_labels) geom_sf_text(data=d_layer_1, aes(label = round(any_rank_pct*100, 1), color=text_print_color), size=3) else list() +
    geom_sf_text(aes(label = pct_print, color=text_print_color), size=3, show.legend = FALSE) +
    guides(color = guide_legend(override.aes = list(label = ""))) +
    scale_color_manual(
      values = c(
        "low" = "white", 
        "high" = "black")
      ) +
    
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.title = element_text(size = 15)
    )
  
  
}
