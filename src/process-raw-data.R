# ----- STEP 0: Configuration ----- #
library("dplyr")
library("readxl")
library("stringr")
library("purrr")

# ----- STEP 1: Read and reshape the raw data ----- #

data_raw <- read_excel("input/Water taste test (Responses).xlsx")
key <- read_excel("input/key.xlsx")

# Rename columns from Google Forms to be more concise
data_renamed <- data_raw |> 
  rename_with(~"respondent", matches("^Your Name$")) |>
  rename_with(~"timestamp", matches("^Timestamp$")) |>
  rename_with(~str_replace(.x, "^Water (\\d+)$", "encode_water\\1"), matches("^Water \\d+$")) |>
  rename_with(~str_replace(.x, "^Water (\\d+): Rating.*", "rating_water\\1"), matches("^Water \\d+: Rating")) |>
  rename_with(~str_replace(.x, "^Water (\\d+): Descriptives.*", "desc_water\\1"), matches("^Water \\d+: Descriptives"))

data_renamed |> glimpse()

# Count the number of waters in the survey by counting encode columns
w <- data_renamed |> 
  select(starts_with("encode")) |> 
  ncol()


# Reshape the data into the desired long format
data_long <- map_dfr(1:w, function(i) {
  data_renamed |> 
    transmute(
      water  = .data[[paste0("encode_water", i)]],
      rating = .data[[paste0("rating_water", i)]],
      words  = .data[[paste0("desc_water", i)]],
      respondent = respondent
    )
}) |>
  arrange(water, respondent) |>
  left_join(key)
  
data_processed <- data_long

# ----- STEP 2: Water results ----- #
save(data_processed, file = "throughput/processed-data.RData")
