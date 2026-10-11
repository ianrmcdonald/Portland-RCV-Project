c_g_1a <- candidate_subset(council_d4_2024, c("Olivia Clark", "Mitch Green")) |> 
  filter(rank_1 == "Olivia Clark" & rank_2 == "Mitch Green") |> 
  select(recordID, precinct)


c_g_1b <- candidate_subset(council_d4_2024, c("Eric Zimmerman", "Mitch Green")) |> 
  filter(rank_1 == "Eric Zimmerman" & rank_2 == "Mitch Green") |> 
  select(recordID, precinct)

c_g_1c <- candidate_subset(council_d4_2024, c("Eli Arnold", "Mitch Green")) |> 
  filter(rank_1 == "Eli Arnold" & rank_2 == "Mitch Green") |> 
  select(recordID, precinct)

c_g_1d <- bind_rows(c_g_1, c_g_1b, c_g_1c) |> 
  distinct()

c_g_2 <- inner_join(c_g_1d, council_d4_2024, by=join_by(recordID, precinct)) |> 
  compute_pdx_stv(eliminated="Mitch Green")

c_g_3 <- anti_join(council_d4_2024, c_g_1d, by=join_by(recordID, precinct)) |> 
  compute_pdx_stv()

c_g_4 <- bind_rows(c_g_2, c_g_3)

c_g_5 <- run_stv_election(c_g_4, n_seats = 3, oqs = "FIXED", write_in = TRUE)
