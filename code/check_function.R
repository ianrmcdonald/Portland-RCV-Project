

test_office_input <- function(obj) {
  
  if(!is.data.frame(obj)) {
    return(FALSE)
  }
  
  correct_names <- c("recordID", "precinct", "rank_1", "rank_2", "rank_3", "rank_4", "rank_5", "rank_6" )
  all(colnames(obj) %in% correct_names)
  
}

