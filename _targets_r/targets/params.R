list(
  tar_target(pollsters, polls |> 
               filter(Election == 2023) |> 
               distinct(Pollster)),
  tar_target(parties_in_parliament, 
             tibble(Party = c('ACT', 'Green', 'Labour', 'National', 'Te Pāti Māori'))),
  tar_target(parties,  polls |> 
               filter(Election == 2023, VotingIntention > 0.025) |>
               filter(n() > 3, .by=Party) |> 
               distinct(Party) |> 
               union(parties_in_parliament) |> 
               arrange(Party)),
  tar_target(pollsters2020, polls |> 
               filter(Election == 2020,
                      Pollster != 'YouGov') |> # Only one YouGov poll 
               distinct(Pollster)),
  tar_target(parties_in_parliament2020, 
             tibble(Party = c('ACT', 'Green', 'Labour', 'National', 'NZ First'))),
  tar_target(parties2020,  polls |> 
               filter(Election == 2020, VotingIntention > 0.025) |>
               filter(n() > 3, .by=Party) |> 
               distinct(Party) |> 
               union(parties_in_parliament2020) |> 
               arrange(Party))
)
