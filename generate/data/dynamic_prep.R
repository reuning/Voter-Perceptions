rm(list = ls())
### Builds data for stan models.
library(tidyverse)
setwd(here::here("generate", "data"))

data <- read_csv("cleaned_data/full_data.csv")
data <- filter(data, year > 2009)

candidates <- unique(data$UUID)
# Democratic Party is id04566
# Republican Party is id04569
candidates <- candidates[!candidates %in% c("id04566", "id04569")]
candidates <- c(c("id04566", "id04569"), candidates)

data$candidate_id <- match(data$UUID, candidates)


voters_drop <- data |>
  mutate(bridge = ifelse(type == "Rep", "No", "Yes")) |> ## Identifies bridges
  group_by(voter_id, year, bridge) |>
  summarize(n = n()) |>
  ungroup() |> ## Counts number of bridges (and non-bridges)
  complete(nesting(voter_id, year), bridge, fill = list("n" = 0)) |> ## Creaes 0s for any missing combinations
  filter(bridge == "Yes") |>
  arrange(n) |>
  filter(n == 0) |> ## finds voters that rated 0 bridges
  mutate("year_voter" = paste0(year, voter_id)) |>
  pull("year_voter")

voter_id_data <- data |>
  group_by(year, voter_id) |>
  summarize(voter_ratings = n()) |>
  ungroup() |>
  filter(voter_ratings > 1) |> ## need more than 1 rating to be useful
  filter(!paste0(year, voter_id) %in% voters_drop) |>
  mutate(unique_voter_id = row_number())

real_voters <- voter_id_data |>
  mutate("year_voter" = paste0(year, voter_id)) |>
  pull("year_voter")

data <- data |>
  filter(paste0(year, voter_id) %in% real_voters)


candidates_remove <- data |>
  group_by(candidate_id) |>
  summarize(n = n()) |>
  filter(n < 5) |>
  pull(candidate_id)


candidate_data <- data |>
  filter(!paste0(year, voter_id) %in% voters_drop) |>
  # select(year, candidate_id, candidate) |>
  group_by(year, candidate_id) |>
  reframe(
    candidate_ratings = n(),
    candidate_mean = mean(rating),
    candidate_sd = sd(rating)
  ) |>
  filter(!candidate_id %in% candidates_remove) |> # need at least 5 ratings overa ll years
  #filter(candidate_ratings>5) |> # needs five ratings in a given year
  arrange(candidate_id, year) |>
  mutate("first_instance" = 1 * !duplicated(candidate_id)) |>
  group_by(candidate_id) |>
  complete(year = full_seq(year, 1), fill = list(first_instance = 0))

candidate_data$theta_id <- 1:nrow(candidate_data)
candidate_data$past_id <- 0

candidate_data <- candidate_data |>
  mutate(
    "rating_type" = case_match(
      year,
      2006:2008 ~ "Continuous",
      2009:2024 ~ "Ordered"
    )
  )

write_out <- data |>
  distinct(candidate_id, candidate) |>
  distinct(candidate_id, .keep_all = T) |>
  right_join(candidate_data)

write_csv(write_out, "../model/candidate_data.csv")


# bridge_id <- read_csv("CCES Data/cces_dynamic_data.csv")

# candidates <- unique(bridge_id$UUID)
# # Democratic Party is id04566
# # Republican Party is id04569
# candidates <- candidates[!candidates %in% c("id04566", "id04569")]
# candidates <- c(c("id04566", "id04569"), candidates)

# bridge_id$candidate_id <- match(bridge_id$UUID, candidates)
# bridge_id$candidate <- NULL
bridge_id <- data |> distinct(candidate_id, year, type, status)
write_out <- left_join(
  write_out,
  bridge_id,
  by = join_by("candidate_id" == "candidate_id", "year" == "year")
)

bridge_df <- write_out |> filter(type == "Common")

bridge_df <- bridge_df |>
  pivot_wider(
    names_from = candidate,
    id_cols = year,
    values_from = candidate_ratings
  ) |>
  arrange(year) |>
  relocate(
    year,
    `Democratic Party`,
    `Republican Party`,
    #`George Bush`,
    #`John McCain`,
    #`Barack Obama`,
    `Tea Party Movement`,
    `Supreme Court`,
    `House of Representatives`,
    Senate,
    `Mitt Romney`,
    `Jeb Bush`,
    `Ted Cruz`,
    `Rand Paul`,
    `Hillary Clinton`,
    `Donald Trump`,
    `Merrick Garland`,
    `Joe Biden`
  ) |>
  rename("Year" = "year")

write_csv(bridge_df, "supplementary_data/bridges.csv", na = "")


write_out |>
  mutate(
    status = case_when(
      is.na(status) & type == "Rep" ~ "Former",
      .default = status
    )
  ) |>
  filter(type != "Common") |>
  group_by(type, year, status) |>
  summarize(n = n(), votes = sum(candidate_ratings)) |>
  pivot_wider(
    names_from = c("type", "status"),
    values_from = c("n", "votes")
  ) |>
  write_csv("supplementary_data/all_ratings.csv")


all_data <- left_join(data, candidate_data) |>
  left_join(voter_id_data) |>
  drop_na(unique_voter_id, theta_id) |>
  select(
    rating,
    candidate_id,
    unique_voter_id,
    theta_id,
    first_instance,
    rating_type
  )


all_data <- all_data |>
  mutate(
    rating = case_match(
      rating_type,
      "Continuous" ~
        as.numeric(cut(
          rating,
          breaks = seq(0, 101, by = 100 / 7),
          include.lowest = T,
          ordered = T
        )),
      "Ordered" ~ rating
    )
  )


dynamic_data <- list(
  voter_N = max(all_data$unique_voter_id),
  scores_N = nrow(all_data),
  candidates_N = max(all_data$theta_id),

  rating = all_data$rating,
  candidate_id = all_data$theta_id,
  voter_id = all_data$unique_voter_id,

  first_instance = candidate_data$first_instance
)

all(1:dynamic_data$voter_N %in% dynamic_data$voter_id) # should be true
all(1:dynamic_data$candidates_N %in% dynamic_data$candidate_id) # no longer true as Ithere are gaps
all(dynamic_data$rating < 8) # should be true.


cmdstanr::write_stan_json(dynamic_data, "../model/model_data.json")
