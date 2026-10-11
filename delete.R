tally <- cvr |> 
  filter(rank_1 == "Eli Arnols") |> 
  summarize(n = sum(transfer_value)) |> 
  arrange(desc(n)) |> 
  ungroup()

  tally
  