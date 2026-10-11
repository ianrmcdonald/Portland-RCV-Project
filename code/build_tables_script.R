library(tidyverse)
source("code/new_cvr_functions.R")

start <- Sys.time()      

read_source_cvr <- function(office_input_csv) {
  council_d1_2024 <- read_csv(office_input_csv) |> 
    select(recordID = `Record Id`, 
           precinct = Precinct,
           rank_1 = `Rank 1`,
           rank_2 = `Rank 2`,
           rank_3 = `Rank 3`,
           rank_4 = `Rank 4`,
           rank_5 = `Rank 5`,
           rank_6 = `Rank 6`) |> 
    filter(!(if_all(starts_with("rank_"), ~ .x == "undervote"))) |> 
    mutate(recordID = str_remove_all(recordID, '\"'))
}     

council_d1_2024 <- read_source_cvr("data/pdx_2024_council_d1.csv")    
council_d2_2024 <- read_source_cvr("data/pdx_2024_council_d2.csv")      
council_d3_2024 <- read_source_cvr("data/pdx_2024_council_d3.csv")      
council_d4_2024 <- read_source_cvr("data/pdx_2024_council_d4.csv")      
mayor_2024 <- read_source_cvr("data/pdx_2024_mayor.csv")      


      #script to generate the all_candidates table
      
all_candidates <- generate_all_candidates_table()
all_candidates_vector <- generate_all_candidates_vector()


#script to generate the precinct reference table

wa_cl_house_districts <- read_csv("data/wa_cl_house_districts.csv")

precinct_reference <- read_csv("data/Precinct_ID_Reference.csv") |> 
  filter(precinct != "NULL") |> 
  mutate(id_number = as.integer(parse_number(id_number))) |> 
  mutate(precinct = str_trim(precinct)) |> 
  mutate(house_district = str_sub(precinct, 1, 2)) |> 
  mutate(county = case_when(
    str_sub(precinct, 1, 5) == "CLACK" ~ "C",
    str_sub(precinct, 1, 4) == "WASH" ~ "W", 
    TRUE ~ "M")) |> 
  mutate(precinct_2 = ifelse(county == "M", precinct, str_sub(precinct, start = -3))) |> 
  mutate(precinct_1 = str_c(county, precinct_2)) |>
  left_join(wa_cl_house_districts, by=join_by(precinct_1 == precinct)) |> 
  mutate(house_district = ifelse(is.na(house_district_delete), house_district, house_district_delete)) |> 
  select(!c(county, precinct_2, house_district_delete))


#generate dlists

dlist_test_1 <- run_stv_election(council_d1_2024, write_in = TRUE)
dlist_test_2 <- run_stv_election(council_d2_2024, write_in = TRUE)
dlist_test_3 <- run_stv_election(council_d3_2024, write_in = TRUE)
dlist_test_4 <- run_stv_election(council_d4_2024, write_in = TRUE)
dlist_test_m <- run_stv_election(mayor_2024, n_seats = 1, 
                                 overall_quota_status = "VARIABLE", 
                                 write_in = TRUE, 
                                 oqs = "VARIABLE")


#simulated elections by precinct   
precincts <- get_source_data_precincts(council_d1_2024)
dlist_01_precincts <- precincts$id_number |> 
  map(\(x) run_stv_by_precinct(office_input = council_d1_2024, precinct_number = x), .progress=TRUE) |> 
  setNames(precincts$precinct_1)

precincts <- get_source_data_precincts(council_d2_2024)
dlist_02_precincts <- precincts$id_number |> 
  map(\(x) run_stv_by_precinct(office_input = council_d2_2024, precinct_number = x), .progress=TRUE) |> 
  setNames(precincts$precinct_1)

precincts <- get_source_data_precincts(council_d3_2024)
dlist_03_precincts <- precincts$id_number |> 
  map(\(x) run_stv_by_precinct(office_input = council_d3_2024, precinct_number = x), .progress=TRUE) |> 
  setNames(precincts$precinct_1)

precincts <- get_source_data_precincts(council_d4_2024)
dlist_04_precincts <- precincts$id_number |> 
  map(\(x) run_stv_by_precinct(office_input = council_d4_2024, precinct_number = x), .progress=TRUE) |> 
  setNames(precincts$precinct_1)

precincts <- get_source_data_precincts(mayor_2024)
dlist_mayor_precincts <- precincts$id_number |> 
  map(\(x) run_stv_by_precinct(office_input = mayor_2024, precinct_number = x, seats = 1), .progress=TRUE) |> 
  setNames(precincts$precinct_1)


end <- Sys.time()      

start
end


