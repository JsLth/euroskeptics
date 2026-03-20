library(tidyverse)
library(haven)
library(rnaturalearth)
library(giscoR)
library(countrycode)
library(sf)

# eurobaro <- haven::read_dta("data/ZA8841.dta")
eurobaro <- read_dta("data/ZA8841.dta")
eurobaro <- eurobaro |>
  select(
    country = isocntry,
    ep_attitude = qa1,
    ep_role = qa2,
    ep_interest = qa3,
    eu_attitude = d78,
    eu_membership = qa4,
    eu_importance = qa5,
    eu_advantages = qa6,
    eu_future = sd22,
    eu_effects = qa7,
    eu_covid = qa8_1,
    eu_climate = qa8_2,
    eu_econ = qa8_3,
    eu_russia = qa8_4,
    eu_migration = qa8_5,
    eu_brexit = qa8_6,
    eu_world = qa9,
    vote_importance = qa16a,
    vote_regular = qa17,
    vote_democracy = qa21_1,
    vote_future = qa21_2,
    vote_others = qa21_3,
    vote_crises = qa21_4,
    
    age = d11r1,
    gender = d10,
    occupation = d15b,
    place_type = d25,
    class = d63,
    starts_with("d9a")
  ) |>
  as_factor() |>
  
  mutate(gender = fct_recode(
    gender,
    Mann = "Man",
    Frau = "Woman",
    "Non-binär" = "None of the above/ Non binary/ do not recognize yourself in above categories/Prefer not to say"
  )) |>
  
  mutate(
    ep_role = fct_relevel(ep_role, "More important", "No change / As it is now (SPONTANEOUS)", "Less important"),
    eu_membership = fct_relevel(eu_membership, "A good thing", "Neither a good thing nor a bad thing", "A bad thing"),
    eu_world = fct_relevel(eu_world, "More important", "Stayed the same", "Less important")
  ) |>
  
  mutate(
    occupation = fct_collapse(
      occupation,
      "Führungskraft" = c(
        "Professional (lawyer, etc.)",
        "Business proprietors, etc.",
        "Employed professional (employed doctor, etc.)",
        "General management, etc.",
        "Middle management, etc."
      ),
      "Bürokraft" = c(
        "Owner of a shop, craftsmen, etc." ,
        "Employed position, at desk",
        "Employed position, travelling",
        "Employed position, service job",
        "Supervisor"
      ),
      "Arbeiter" = c(
        "Farmer",
        "Fisherman",
        "Skilled manual worker",
        "Unskilled manual worker, etc."
      ),
      "Nicht erwerbstätig" = c(
        "Never did any paid work"
      )
    )
  ) |>
  
  mutate(across(
    starts_with("d9a"),
    ~if_else(
      .x == "Not mentioned",
      NA,
      as.numeric(str_extract(cur_column(), "\\d+$"))
    )
  )) %>%
  mutate(
    education = do.call(coalesce, select(., num_range("d9a_", 13:1))),
    education = factor(fct_collapse(
      as.character(education),
      "Grundbildung" = as.character(1:3),
      "Berufsbildung" = c("4", "5", "10"),
      "Hochschulbildung" = c("6", "7", "8", "9", "11"),
      other_level = NA
    )
  )) |>
  select(-num_range("d9a_", 13:1)) |>
  
  mutate(across(where(is.factor), ~{
    fct_relabel(.x, ~gsub(" (SPONT.)", "", .x, fixed = TRUE))
  })) |>
  
  mutate(across(where(is.factor), ~{
    bad_levels <- levels(.x)[grepl(
      "^(Don't know|Other|None|Refusal|DK|Inap\\.|It depends|You were too young)",
      levels(.x)
    )]
    
    fct_match(.x, bad_levels) |>
      if_else(NA, .x) |>
      fct_drop()
  })) |>
  
  mutate(
    place_type = fct_recode(
      place_type,
      "Dorf" = "Rural area or village",
      "Kleinstadt" = "Small or middle sized town",
      "Großstadt" = "Large town"
    ),
    
    class = fct_recode(
      class,
      "Arbeiterklasse" = "The working class of society",
      "Untere Mittelschicht" = "The lower middle class of society",
      "Mittelschicht" = "The middle class of society",
      "Obere Mittelschicht" = "The upper middle class of society",
      "Oberschicht" = "The higher class of society"
    )
  ) |>
  mutate(country = substr(country, 1, 2))

saveRDS(eurobaro, "data/eurobaro.rds")


iso3 <- countrycode(substr(unique(eurobaro$country), 1, 2), "iso2c", destination = "iso3c")
group_polys <- gisco_get_nuts(country = iso3, nuts_level = "0", epsg = 3035, resolution = 60) |>
  mutate(geo = countrycode(geo, "eurostat", "iso2c")) |>
  select(country = geo) |>
  right_join(eurobaro, by = "country") |>
  group_by(country, age, gender, education, occupation, place_type, class) |>
  summarise(across(-geometry, ~{
    new <- levels(.x)[median(as.numeric(.x), na.rm = TRUE)]
    factor(new, levels = levels(eurobaro[[cur_column()]]))
  })) |>
  ungroup() |>
  st_drop_geometry()
global_polys <- ne_countries(scale = 50) |>
  st_transform(3035) |>
  select(country = iso_a2_eh)

saveRDS(group_polys, "data/eurobaro_spatial.rds")
saveRDS(global_polys, "data/global_polys.rds")
