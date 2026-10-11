update_pdx_cvr <- function(office_input, eliminated = "", elected = "") {
  
  if(!test_office_input_df(office_input))  {
    return(str_c("Problem with structure of ", office_input))
  }
  
  office_output <- office_input |> 
    pivot_longer(cols = !c(recordID, precinct), 
                 names_to = "choice_num", values_to = "choice") |> 
    mutate(choice_num = as.numeric(str_sub(choice_num, -1, -1))) |> 
    filter(choice != "undervote" & 
             !str_detect(choice, "\n") & 
             !(choice %in% eliminated) &
             !(choice %in% elected)) |> 
    
    group_by(recordID, precinct, choice) |> 
      slice_min(choice_num, with_ties = FALSE) |> 
    ungroup() |> 
    
    group_by(recordID, precinct) |> 
      mutate(choice_num = rank(choice_num)) |> 
    ungroup() |> 
    
    pivot_wider(id_cols = c("recordID", "precinct"),
                names_from = choice_num,
                values_from = choice)
  
  if (!("6" %in% names(office_output))) {
    office_output$`6` <- NA_character_
  }
  if (!("5" %in% names(office_output))) {
    office_output$`5` <- NA_character_
  }
  if (!("4" %in% names(office_output))) {
    office_output$`4` <- NA_character_
  }
  if (!("3" %in% names(office_output))) {
    office_output$`3` <- NA_character_
  }
  if (!("2" %in% names(office_output))) {
    office_output$`2` <- NA_character_
  }
  if (!("1" %in% names(office_output))) {
    office_output$`1` <- NA_character_
  } 
  
  office_output|> 
    relocate(recordID, precinct, rank_1 = `1`, rank_2 = `2`, rank_3 = `3`,
             rank_4 = `4`, rank_5 = `5`, rank_6 = `6`) 
  
}


generate_all_candidates_table <- function() {
  
  council_d1_2024_o <- council_d1_2024 |> 
    mutate(office = "d1")
  
  council_d2_2024_o <- council_d2_2024 |> 
    mutate(office = "d2")
  
  council_d3_2024_o <- council_d3_2024 |> 
    mutate(office = "d3")
  
  council_d4_2024_o <- council_d4_2024 |> 
    mutate(office = "d4")
  
  mayor_2024_o <- mayor_2024 |> 
    mutate(office = "mayor")
  
  bind_rows(council_d1_2024_o,
            council_d2_2024_o,
            council_d3_2024_o,
            council_d4_2024_o,
            mayor_2024_o) |>
    pivot_longer(cols = !c(recordID, precinct, office), 
                 names_to = "choice_num", values_to = "choice") |> 
    filter(choice != "undervote" & 
             !str_detect(choice, "\n")) |>
    select(choice, office) |> 
    arrange(choice) |> 
    distinct()
}

generate_all_candidates_vector <- function() {
  bind_rows(council_d1_2024,
                            council_d2_2024,
                            council_d3_2024,
                            council_d4_2024,
                            mayor_2024) |>
  pivot_longer(cols = !c(recordID, precinct), 
               names_to = "choice_num", values_to = "choice") |> 
  filter(choice != "undervote" & 
           !str_detect(choice, "\n")) |>
  select(choice) |> 
  arrange(choice) |> 
  distinct() |> 
  pull(choice)
  
}

generate_rank_1_tally <- function(office_input) {
  
  office_input |> 
    select(rank_1) |> 
    filter(rank_1 != "undervote" & 
             !str_detect(rank_1, "\n")) |> 
    count(rank_1, sort = TRUE)
  
}

elec_status <- function(votes = 0, elec_quota) {
  if(votes >= elec_quota) {
    "elected"
  } else {
    "eliminated"
  }
}


eliminated_status_list_gen <- function(dlist_input, 
                                       seats = 3, 
                                       overall_quota_status = oqs,
                                       absolute_quota = FALSE) {

  to_be_elected <- dlist_input$to_be_elected
  
  if(to_be_elected == 0) {
    return(list(cvr = NULL, tally = NULL,
                quota = NULL, to_be_elected = 0, 
                surplus_factor = NULL,
                top_vote_total = NULL, elec_status_semaphore = NULL,
                elected_candidate = NULL, 
                eliminated_candidate = NULL,
                transfer_values = NULL))
  }
  
  cvr <- update_pdx_cvr(dlist_input$cvr, eliminated = dlist_input$eliminated_candidate)
  
  surplus_factor <- dlist_input$surplus_factor
  transfer_values <- dlist_input$transfer_values
  
  cvr <- cvr |> 
    left_join(dlist_input$transfer_value, join_by(recordID, precinct))
  
  tally <- cvr |> 
    group_by(rank_1) |> 
      summarize(n = sum(transfer_value)) |> 
      arrange(desc(n)) |> 
    ungroup()
  
  transfer_values <- cvr |> 
    select(recordID, precinct, transfer_value)
  
  cvr <- cvr |> 
    select(!transfer_value)
  
  if(overall_quota_status == "FIXED") {
    quota <- dlist_input$fixed_quota
  } else {
    quota <- nrow(cvr) %/% (seats + 1) + 1 
  }
  
  fixed_quota <- dlist_input$fixed_quota
  
  if(absolute_quota) quota = ABSOLUTE_QUOTA
  
  

  if(nrow(tally) > 0) {
    top_vote_total <- tally[[1,2]]
  }
  
  if(nrow(tally) <= dlist_input$to_be_elected |
     (elec_status(top_vote_total, quota) == "elected" & nrow(tally) > 0)) {
    
    eliminated_candidate <- "" #tally_initial[[nrow(tally_initial), 1]]
    #tally[[1,2]] <- quota
    elected_candidate <- tally[[1, 1]]
    elec_status_semaphore <- "elected"
    
  } else if(elec_status(top_vote_total, quota) == "eliminated" & nrow(tally) > 0) {
    
    eliminated_candidate <- tally[[nrow(tally), 1]]
    elected_candidate <- ""
    elec_status_semaphore <- "eliminated"
    
  } else if(nrow(tally) == 1 |
            (elec_status(top_vote_total, quota) == "elected" & nrow(tally) > 0)) {
    
    eliminated_candidate <- "" #tally_initial[[nrow(tally_initial), 1]]
    #tally[[1,2]] <- quota
    elected_candidate <- tally[[1, 1]]
    elec_status_semaphore <- "elected"
  }
  
  
  list(cvr = cvr, tally = tally,
       quota = quota, fixed_quota = fixed_quota, to_be_elected = to_be_elected, 
       top_vote_total = top_vote_total, elec_status_semaphore = elec_status_semaphore,
       elected_candidate = elected_candidate, 
       eliminated_candidate = eliminated_candidate,
       transfer_values = transfer_values,
       surplus_factor = surplus_factor)
  
}

# Oct 7 why is the winning vote total rounding?

elected_status_list_gen <- function(dlist_input, 
                                    seats = 3, 
                                    overall_quota_status = oqs,
                                    absolute_quota = FALSE,
                                    truncate_surplus_factor = TRUE) {
  #https://www.portland.gov/code/2/08/030
  to_be_elected <- dlist_input$to_be_elected - 1
  
  #need to return a list that allows us to break the loop
  if(to_be_elected == 0) {
    return(list(cvr = 0, tally = 0,
                quota = 0, to_be_elected = 0, 
                surplus_factor = 0,
                top_vote_total = 0, elec_status_semaphore = 0,
                elected_candidate = 0, 
                eliminated_candidate = 0,
                transfer_values = 0,
                fixed_quota = 0))
  }
  
  if(truncate_surplus_factor) {
   
     new_surplus_factor <- max(
      trunc((
        dlist_input$top_vote_total - dlist_input$quota) /
        dlist_input$top_vote_total * 
          10^4) / 
          10^4,
      0
    )
    } else {
      
      new_surplus_factor <- max(
        (dlist_input$top_vote_total - dlist_input$quota) /
          dlist_input$top_vote_total,
          0
      )
    }
  

  #fix this....second winner not generating right fraction.  See round 29 in district 4
  winner_cvr <- dlist_input$cvr |> 
    filter(rank_1 == dlist_input$elected_candidate) |>
    update_pdx_cvr(elected = dlist_input$elected_candidate) |> 
    left_join(dlist_input$transfer_values, join_by(recordID, precinct)) |> 
    mutate(transfer_value = new_surplus_factor * transfer_value)
  
  if(truncate_surplus_factor) {
    
    winner_cvr <- winner_cvr |> 
      mutate(transfer_value = trunc(transfer_value * 10^4) / 10^4)
  }

  
  non_winner_cvr <- dlist_input$cvr |> 
    filter(rank_1 != dlist_input$elected_candidate)
  
  if(nrow(non_winner_cvr) > 0) {
    non_winner_cvr <- non_winner_cvr |> 
      update_pdx_cvr(elected = dlist_input$elected_candidate)  |> 
      left_join(dlist_input$transfer_values, join_by(recordID, precinct))
  }
  
  if(nrow(non_winner_cvr) > 0 & nrow(winner_cvr) > 0) {
    cvr <- bind_rows(winner_cvr, non_winner_cvr)
  } else if(nrow(non_winner_cvr) > 0 & nrow(winner_cvr) == 0) {
    cvr <- non_winner_cvr 
  } else {
    cvr <- winner_cvr
  }
  
  tally <- cvr |> 
    group_by(rank_1) |> 
    summarize(n = sum(transfer_value)) |>
    arrange(desc(n)) |> 
    ungroup()
  
  if(nrow(tally) > 0) {
    top_vote_total <- tally[[1,2]]
  } else {
    top_vote_total <- 1
  }
  
  transfer_values <- cvr |> 
    select(recordID, precinct, transfer_value)
  
  cvr <- cvr |> 
    select(!transfer_value)
  
  if(overall_quota_status == "FIXED") {
    quota <- dlist_input$fixed_quota
  } else {
    quota <- nrow(cvr) %/% (seats + 1) + 1 
  }
  
  fixed_quota <- dlist_input$fixed_quota
  
  if(absolute_quota) quota = ABSOLUTE_QUOTA
  
  if(elec_status(top_vote_total, quota) == "eliminated" & nrow(tally) > 1) {
    
    eliminated_candidate <- tally[[nrow(tally), 1]]
    elected_candidate <- ""
    elec_status_semaphore <- "eliminated"
    
  } else if(elec_status(top_vote_total, quota) == "elected" | nrow(tally) == 1) {
    
    eliminated_candidate <- ""
    if(nrow(tally) > 0) {
      elected_candidate <- tally[[1, 1]]
    } else {
      elected_candidate <- dlist_input$cvr[["rank_1"]]
    }
    elec_status_semaphore <- "elected"
  }
  
  if(!exists("elec_status_semaphore")) {
    elec_status_semaphore = "elected"
  }
  
  list(cvr = cvr, tally = tally,
       quota = quota, fixed_quota = fixed_quota, to_be_elected = to_be_elected, 
       surplus_factor = new_surplus_factor,
       top_vote_total = top_vote_total, elec_status_semaphore = elec_status_semaphore,
       elected_candidate = elected_candidate, 
       eliminated_candidate = eliminated_candidate,
       transfer_values = transfer_values)
}

gen_status_list <- function(basis_list, ...) {
  
  if(basis_list$to_be_elected == 0) return("all done")
  
  if(basis_list$elec_status_semaphore == "eliminated") {
    new_list <- eliminated_status_list_gen(basis_list, ...)
  }
  
  if(basis_list$elec_status_semaphore == "elected") {
    new_list <- elected_status_list_gen(basis_list, ...)
  }
  return(new_list)
}

candidate_number <- function(office_input) {
  office_input |> 
    pivot_longer(cols = !c(recordID, precinct), 
                names_to = "choice_num", values_to = "choice") |> 
    select(choice) |> 
    distinct() |> 
    na.omit() |> 
  nrow()
}

combine_mayor_and_district <- function(district, mayor_cand = "ALL") {
  district_data <- str_c("council_d", as.character(district), "_2024")
  
  mayor_2024 |> 
    select(recordID, precinct, rank_1) |> 
    inner_join(get(district_data), by=(c("recordID", "precinct"))) |> 
    rename(rank_m = rank_1.x, rank_1 = rank_1.y) |> 
    filter(if (mayor_cand != "ALL") rank_m == mayor_cand else TRUE) |> 
    select(-rank_m)
}

gen_initial_list <- function(SOURCE_DATA_NAME, 
                             seats = 3, 
                             absolute_quota = FALSE) {
  
  cvr_initial <- update_pdx_cvr(SOURCE_DATA_NAME)
  
  transfer_value_initial <- cvr_initial |> 
    select(recordID, precinct) |>
    mutate(transfer_value = 1)
  
  tally_initial <- generate_rank_1_tally(cvr_initial)
  
  quota <- nrow(cvr_initial) %/% (seats + 1) + 1
  if(absolute_quota) quota <- ABSOLUTE_QUOTA

  to_be_elected <- seats

  top_vote_total <- tally_initial[[1,2]]
  
  if(elec_status(top_vote_total, quota) == "eliminated" ) {
    
    eliminated_candidate <- tally_initial[[nrow(tally_initial), 1]]
    elected_candidate <- ""
    elec_status_semaphore <- "eliminated"
    
  } else if(elec_status(top_vote_total, quota) == "elected" ) {
    
    eliminated_candidate <- "" #tally_initial[[nrow(tally_initial), 1]]
    #tally_initial[[1,2]] <- round(quota)
    elected_candidate <- tally_initial[[1, 1]]
    elec_status_semaphore <- "elected"
  }
  
  list(cvr = cvr_initial, tally = tally_initial,
       quota = quota, fixed_quota = quota, to_be_elected = to_be_elected, 
       top_vote_total = top_vote_total, elec_status_semaphore = elec_status_semaphore,
       elected_candidate = elected_candidate, 
       eliminated_candidate = eliminated_candidate,
       transfer_values = transfer_value_initial,
       surplus_factor = 1)
}


elim_write_in <- function(office_input, eliminated = "", elected = "") {
  
  office_output <- office_input |> 
    pivot_longer(cols = !c(recordID, precinct), 
                 names_to = "choice_num", values_to = "choice") |> 
    mutate(choice_num = as.numeric(str_sub(choice_num, -1, -1))) |> 
    #FIX THIS above: will not work if rank positions is > 9
    filter(choice != "undervote" & 
             !str_detect(choice, "\n") & 
             choice != eliminated &
             choice != elected &
             !str_detect(choice, "Write-in-"))|> 
    
    group_by(recordID, precinct, choice) |> 
    slice_min(choice_num, with_ties = FALSE) |> 
    ungroup() |> 
    
    group_by(recordID, precinct) |> 
    mutate(choice_num = rank(choice_num)) |> 
    ungroup() |> 
    
    pivot_wider(id_cols = c("recordID", "precinct"),
                names_from = choice_num,
                values_from = choice)
  
  if (!("6" %in% names(office_output))) {
    office_output$`6` <- NA_character_
  }
  if (!("5" %in% names(office_output))) {
    office_output$`5` <- NA_character_
  }
  if (!("4" %in% names(office_output))) {
    office_output$`4` <- NA_character_
  }
  if (!("3" %in% names(office_output))) {
    office_output$`3` <- NA_character_
  }
  if (!("2" %in% names(office_output))) {
    office_output$`2` <- NA_character_
  }
  if (!("1" %in% names(office_output))) {
    office_output$`1` <- NA_character_
  } 
  
  office_output|> 
    relocate(recordID, precinct, rank_1 = `1`, rank_2 = `2`, rank_3 = `3`,
             rank_4 = `4`, rank_5 = `5`, rank_6 = `6`) 
  
}

candidate_subset <- function(office_input, candidate_vector, collapse = TRUE) {
  
  office_output <- office_input |> 
    pivot_longer(cols = !c(recordID, precinct), 
                 names_to = "choice_num", values_to = "choice") |> 
    mutate(choice_num = as.numeric(str_sub(choice_num, -1, -1))) |> 
    #FIX THIS above: will not work if rank positions is > 9
    filter(choice != "undervote" & 
             !str_detect(choice, "\n")) |> 
    
    filter(choice %in% candidate_vector) |>
    
    group_by(recordID, precinct, choice) |> 
    slice_min(choice_num, with_ties = FALSE) |> 
    ungroup()  
  
  if(collapse) {
    office_output <- office_output |> 
      group_by(recordID, precinct) |> 
      mutate(choice_num = rank(choice_num)) |> 
      ungroup()
  }
  # office_output_sum <- office_output |> 
  #   group_by(choice_num) |> 
  #     count(choice) |> 
  #   ungroup()
  
  
  office_output <- office_output |> 
    pivot_wider(id_cols = c("recordID", "precinct"),
                names_from = choice_num,
                values_from = choice)
  
  if (!("6" %in% names(office_output))) {
    office_output$`6` <- NA_character_
  }
  if (!("5" %in% names(office_output))) {
    office_output$`5` <- NA_character_
  }
  if (!("4" %in% names(office_output))) {
    office_output$`4` <- NA_character_
  }
  if (!("3" %in% names(office_output))) {
    office_output$`3` <- NA_character_
  }
  if (!("2" %in% names(office_output))) {
    office_output$`2` <- NA_character_
  }
  if (!("1" %in% names(office_output))) {
    office_output$`1` <- NA_character_
  } 
  
  office_output|> 
    relocate(recordID, precinct, rank_1 = `1`, rank_2 = `2`, rank_3 = `3`,
             rank_4 = `4`, rank_5 = `5`, rank_6 = `6`) 
  
}

two_way_count <- function(office_input, c1, c2) {
  
  office_output <- office_input |> 
    candidate_subset(c(c1, c2)) |> 
    
    
    count(precinct, rank_1) |> 
    
    pivot_wider(id_cols = precinct, names_from = rank_1, values_from = n) |> 
    mutate(c1_pct = .data[[c1]] / (.data[[c1]] + .data[[c2]])) |> 
    
    left_join(precinct_reference, join_by(precinct == id_number)) |> 
    rename(precinct_name = precinct.y)
}

two_way_count_overall <- function(office_input, c1, c2) {
  
  office_output <- office_input |> 
    candidate_subset(c(c1, c2)) |> 
    count(rank_1) |> 
    
    pivot_wider(names_from = rank_1, values_from = n) |> 
    mutate(c1_pct = .data[[c1]] / (.data[[c1]] + .data[[c2]])) 
  
}

candidate_ranked_pct <- function(office_input, candidate) {
  
  office_output <- office_input |> 
    pivot_longer(cols = !c(recordID, precinct), 
                 names_to = "choice_num", values_to = "choice") |> 
    mutate(choice_num = as.numeric(str_sub(choice_num, -1, -1))) |> 
    #FIX THIS above: will not work if rank positions is > 9
    filter(choice != "undervote" & 
             !str_detect(choice, "\n")) |> 
    
    pivot_wider(id_cols = c("recordID", "precinct"),
                names_from = choice_num,
                values_from = choice)
    
  if (!("6" %in% names(office_output))) {
    office_output$`6` <- NA_character_
  }
  if (!("5" %in% names(office_output))) {
    office_output$`5` <- NA_character_
  }
  if (!("4" %in% names(office_output))) {
    office_output$`4` <- NA_character_
  }
  if (!("3" %in% names(office_output))) {
    office_output$`3` <- NA_character_
  }
  if (!("2" %in% names(office_output))) {
    office_output$`2` <- NA_character_
  }
  if (!("1" %in% names(office_output))) {
    office_output$`1` <- NA_character_
  }
    
  office_output <- office_output |> 
    relocate(recordID, precinct, rank_1 = `1`, rank_2 = `2`, rank_3 = `3`,
             rank_4 = `4`, rank_5 = `5`, rank_6 = `6`) 
  
  total_ballots <- nrow(office_output)
  
  candidate_ballots <- office_output |> 
    filter(rank_1 == candidate | rank_2 == candidate | rank_3 == candidate |
             rank_4 == candidate | rank_5 == candidate | rank_6 == candidate) |> 
    nrow()
  
  candidate_ballots / total_ballots
}


get_ranks <- function(office, candidate_input, nseats = 3) {
  
  if(!(office %in% c("d1", "d2", "d3", "d4", "mayor"))) {
    return("check office input format")
  }
  
  if(office == "d1") {
    dlist_precincts <- dlist_01_precincts
    office_input <- council_d1_2024
  }
  
  if(office == "d2") {
    dlist_precincts <- dlist_02_precincts
    office_input <- council_d2_2024
  }
  
  if(office == "d3") {
    dlist_precincts <- dlist_03_precincts
    office_input <- council_d3_2024
  }
  
  if(office == "d4") {
    dlist_precincts <- dlist_04_precincts
    office_input <- council_d4_2024
  }
  
  if(office == "mayor") {
    dlist_precincts <- dlist_mayor_precincts
    office_input <- mayor_2024
    nseats <- 1
  }
  
  w <- ""
  
  precincts <- get_source_data_precincts(office_input)
  
  for(m in 1:nrow(precincts)) {
    w <- c(w, map_chr(dlist_precincts[[m]], "elected_candidate"))
  }
  
  w <- w[nzchar(w)]
  w_list <- split(w, rep(1:nrow(precincts), each = nseats))
  
  place_1 <- map_chr(w_list, 1)
  if(nseats == 3) {
      place_2 <- map_chr(w_list, 2)
      place_3 <- map_chr(w_list, 3)
  } else {
      place_2 <- rep(NA_character_, nrow(precincts))
      place_3 <- rep(NA_character_, nrow(precincts))
  }
  
  rank_table <- precincts |> 
    mutate(first = place_1, second = place_2, third = place_3)
  
  c1 <- numeric(0)
  candidate <- candidate_input
  
  
  if(nseats == 3) {
    for(m in 1:nrow(precincts)) {
      if(place_1[m] == candidate) {
        c1[m] <- 1
      } else if(place_2[m] == candidate) {
        c1[m] <- 2
      } else if(place_3[m] == candidate) {
        c1[m] <- 3 
      }  else {
        c1[m] <- 0
      }
    }  
    
  } else if(nseats == 1) {
    for(m in 1:nrow(precincts)) {
      if(place_1[m] == candidate) {
        c1[m] <- 1
      } else {
        c1[m] <- 0
      }
    }
  }
  precincts |> mutate(ranks = c1)
}


get_rank_table <- function(office, nseats = 3) {
  
  if(!(office %in% c("d1", "d2", "d3", "d4", "mayor"))) {
    return("check office input format")
  }
  
  if(office == "d1") {
    office_input <- council_d1_2024
    dlist_precincts <- dlist_01_precincts
  }
  
  if(office == "d2") {
    office_input <- council_d2_2024
    dlist_precincts <- dlist_02_precincts
  }
  
  if(office == "d3") {
    office_input <- council_d3_2024
    dlist_precincts <- dlist_03_precincts
  }
  
  if(office == "d4") {
    office_input <- council_d4_2024
    dlist_precincts <- dlist_04_precincts
    
  }
  
  if(office == "mayor") {
    office_input <- mayor_2024
    dlist_precincts <- dlist_mayor_precincts
    nseats <- 1
  }
  
  w <- ""
  
  precincts <- get_source_data_precincts(office_input)
  
  for(m in 1:nrow(precincts)) {
    w <- c(w, map_chr(dlist_precincts[[m]], "elected_candidate"))
  }
  
  w <- w[nzchar(w)]
  w_list <- split(w, rep(1:nrow(precincts), each = nseats))
  
  place_1 <- map_chr(w_list, 1)
  if(nseats == 3) {
    place_2 <- map_chr(w_list, 2)
    place_3 <- map_chr(w_list, 3)
  } else {
    place_2 <- rep(NA_character_, nrow(precincts))
    place_3 <- rep(NA_character_, nrow(precincts))
  }
  
  rank_table <- precincts |> 
    mutate(first = place_1, second = place_2, third = place_3) |> 
    select(n, house_district, precinct = precinct_1, first, second, third) |> 
    relocate(house_district, precinct, n, first, second, third) |> 
    arrange(house_district, precinct)
  
  
  return(rank_table)
}

gen_tally_master <- function(stv_list) {
  stv_list[1:length(stv_list)]  |> 
    map("tally") |> 
    list_rbind(names_to = "round")
}

run_stv_election <- function(office_input, 
                             n_seats = 3,
                             elim_zero_vote = FALSE,
                             overall_quota_status = "FIXED",
                             ABSOLUTE_QUOTA = FALSE,
                             write_in = FALSE,
                             oqs = "FIXED",
                             print = TRUE) {
  
  absolute_quota_val <- 0
  
  if (elim_zero_vote) {
    office_output <- eliminate_zero_vote_rank_1_candidates(office_input)
  } else {
    office_output <- office_input
  }
  
  if (write_in) {
    office_output <- elim_write_in(office_output)
  }
  
  dlist <- vector("list", length = 2)
  i <- 1
  dlist[[1]] <- gen_initial_list(office_output, 
                                 seats = n_seats, 
                                 absolute_quota = ABSOLUTE_QUOTA)
  
  while (TRUE) {  
    
    dlist[[i+1]] <- 
      gen_status_list(dlist[[i]], 
                      seats = n_seats, 
                      overall_quota_status = oqs,
                      absolute_quota = FALSE)
if(print == TRUE) {  
    if(dlist[[i]]$elec_status_semaphore == "eliminated") {
      cat(str_c("round:",i,"  ", "\t",
                "Eliminated: ",dlist[[i]]$eliminated_candidate,"\t",
                " Most votes: ", round(dlist[[i]]$top_vote_total,4),"\t",
                  " Quota: ", dlist[[i]]$quota, "\n"))
    }
 
      if(dlist[[i]]$elec_status_semaphore == "elected") {
        cat(str_c("round:",i,"  ", "\t",
                    "Elected: ",dlist[[i]]$elected_candidate,"\t",
                    " Most votes: ", round(dlist[[i]]$top_vote_total,4),"\t",
                    " Quota: ", dlist[[i]]$quota, "\n"))    }       
      
      }
    
        
    
    i <- i + 1
    
    if(dlist[[i]]$to_be_elected == 0) break
    
  }
  
  dlist <- dlist[-length(dlist)] 
  
}

get_source_data_precincts <- function(office_input) {
  source_data_precincts <- office_input |> 
    count(precinct) |> 
    rename(id_number = precinct) |> 
    left_join(precinct_reference, join_by(id_number)) 
  
}

run_stv_by_precinct <- function(office_input, precinct_number, seats = 3) {
  office_input |> 
    filter(precinct == precinct_number) |> 
    run_stv_election(n_seats = seats, oqs = "VARIABLE")
}

make_pct_table <- function(office_input) {
  
  candidate_vector <- office_input |> 
    update_pdx_cvr() |> 
    pull(rank_1) |> 
    unique()
  
  percentages<- map_dbl(candidate_vector, 
                        \(x) 
                        candidate_ranked_pct(office_input, x))
  tibble(candidate_vector, percentages) |> 
    arrange(desc(percentages))
  
}

ballots_by_precinct_any_rank <- function(office_input, candidate_vector) {
  candidate_subset(office_input, c(candidate_vector), collapse = TRUE) |>
    select(precinct, rank_1) |> 
    group_by(precinct) |> 
    count() |>
    ungroup() |> 
    left_join(precinct_reference, by=join_by(precinct == id_number)) |>
    rename(any_rank = n) |> 
    select(precinct_1, any_rank)
}

#count of first place votes by precinct
ballots_by_precinct_rank_1 <- function(office_input, candidate_vector) {
  office_input |>
    filter(rank_1 %in% candidate_vector) |> 
    select(precinct, rank_1) |> 
    group_by(precinct) |> 
    count() |>
    ungroup() |> 
    left_join(precinct_reference, by=join_by(precinct == id_number)) |> 
    rename(rank_1 = n) |> 
    select(precinct_1, rank_1)
}

ballots_by_precinct_all <- function(office_input) {
  office_input |> 
    group_by(precinct) |> 
    count() |> 
    ungroup() |> 
    left_join(precinct_reference, by=join_by(precinct == id_number)) |> 
    rename(all_ballots = n) |> 
    select(precinct_1, all_ballots)
}

make_ballots_by_precinct_table <- function(office_input, candidate_vector) {
  
  x1 <- ballots_by_precinct_any_rank(office_input, candidate_vector)
  x2 <- ballots_by_precinct_rank_1(office_input, candidate_vector)
  x3 <- ballots_by_precinct_all(office_input)
  x3 |> 
    left_join(x1, join_by(precinct_1)) |> 
    left_join(x2, join_by(precinct_1)) |> 
    mutate_all(~replace(., is.na(.), 0)) |> 
    mutate(rank_1_pct = rank_1 / all_ballots) |> 
    mutate(any_rank_pct = any_rank / all_ballots) |> 
    arrange(precinct_1)
  
}

test_office_input_df <- function(obj) {
  
  if(!is.data.frame(obj)) {
    return(FALSE)
  }
  
  correct_names <- c("recordID", "precinct", "rank_1", "rank_2", "rank_3", "rank_4", "rank_5", "rank_6" )
  all(colnames(obj) %in% correct_names)
  
}


