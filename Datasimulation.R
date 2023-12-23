# Pakker som kreves for kodeutførelsen
library(readxl) # 'readxl' brukes til å lese Excel-filer. Denne pakken tillater enkel import av data fra Excel-format.
library(dplyr)  # 'dplyr' er en del av tidyverse-pakken og brukes for data manipulering og analyse. 
# Den tilbyr brukervennlige funksjoner for å filtrere, sortere, oppsummere og manipulere datasett.
library(writexl)  # Legger til openxlsx-pakken for Excel-eksport

# Laster inn data
data <- read_excel("C:/Users/moon/DataViTrenger.xlsx") # Lagrer og endrer filbanen til der våres Excel-fil er lagret.
# 'read_excel' funksjonen fra 'readxl' pakken brukes her for å lese inn data fra en Excel-fil.

# Behandling av data for å forberede for analyse
# Deler 'Resultat'-kolonnen for å få separate kolonner for antall mål scoret av hjemme- og bortelag.
# Her brukes regulære uttrykk (regex) i 'sub' funksjonen for å splitte tekststrengen.
data$Hjemme_Mål <- as.integer(sub(" - .*", "", data$Resultat)) # Fjerner alt etter " - " og konverterer til heltall.
data$Borte_Mål <- as.integer(sub(".* - ", "", data$Resultat)) # Fjerner alt før " - " og konverterer til heltall.

# Beregner gjennomsnittlige mål
# Grupperer dataene etter 'Hjemme' (hjemmelag) og beregner gjennomsnittlig antall mål scoret og innrømmet.
home_stats <- data %>%
  group_by(Hjemme) %>%
  summarise(Hjemme_Mål_Scoret = mean(Hjemme_Mål, na.rm = TRUE), # 'na.rm = TRUE' sikrer at NA-verdier ignoreres i beregningen.
            Hjemme_Mål_Innrømmet = mean(Borte_Mål, na.rm = TRUE))

# Gjentar prosessen for bortelagene.
away_stats <- data %>%
  group_by(Borte) %>%
  summarise(Borte_Mål_Scoret = mean(Borte_Mål, na.rm = TRUE),
            Borte_Mål_Innrømmet = mean(Hjemme_Mål, na.rm = TRUE))

# Kombinerer statistikk for hjemme og bortelag til en enkelt data.frame.
# 'full_join' brukes for å slå sammen de to settene med statistikk, noe som gir en fullstendig oversikt per lag.
avg_stats <- full_join(home_stats, away_stats, by = c("Hjemme" = "Borte"))

# Funksjon for å simulere en fotballkamp
simulate_match <- function(home_team, away_team, stats) {
  # Henter statistikk for de angitte lagene.
  home_stats <- stats[stats$Hjemme == home_team, ]
  away_stats <- stats[stats$Hjemme == away_team, ]
  
  # Sjekker at nødvendig statistikk er tilgjengelig og gyldig for begge lagene.
  if (nrow(home_stats) == 0 || nrow(away_stats) == 0 || is.na(home_stats$Hjemme_Mål_Scoret) || is.na(away_stats$Borte_Mål_Innrømmet)) {
    stop("No valid stats found for one or both teams: ", home_team, " vs ", away_team)
  }
  
  # Beregner forventet antall mål (lambda) for hjemme- og bortelaget basert på tidligere statistikk.
  lambda_home <- home_stats$Hjemme_Mål_Scoret * away_stats$Borte_Mål_Innrømmet
  lambda_away <- away_stats$Borte_Mål_Scoret * home_stats$Hjemme_Mål_Innrømmet
  
  # Sjekker at de beregnede lambda-verdiene er gyldige for Poisson-distribusjonen.
  if (lambda_home <= 0 || lambda_away <= 0) {
    stop("Invalid lambda values for match: ", home_team, " vs ", away_team)
  }
  
  # Simulerer antall mål scoret av hvert lag ved hjelp av Poisson-distribusjonen.
  goals_home <- rpois(1, lambda_home)
  goals_away <- rpois(1, lambda_away)
  
  return(c(goals_home, goals_away))
}

# Forbereder kampplan for den kommende sesongen
teams <- unique(c(data$Hjemme, data$Borte)) # Lager en unik liste av alle lagene.
fixture <- expand.grid(home = teams, away = teams) # Lager en fullstendig kampplan med alle mulige hjemme- og borte-kombinasjoner.
fixture <- subset(fixture, home != away)  # Fjerner kamper hvor et lag ville spille mot seg selv.

# Funksjon for å tildele poeng basert på kampresultatene
assign_points <- function(result, home_team, away_team, points_table) {
  # Tildeler poeng basert på resultatet: 3 poeng for seier, 1 for uavgjort, og 0 for tap.
  if (result[1] > result[2]) {
    points_table[points_table$team == home_team, "points"] <- points_table[points_table$team == home_team, "points"] + 3
  } else if (result[1] < result[2]) {
    points_table[points_table$team == away_team, "points"] <- points_table[points_table$team == away_team, "points"] + 3
  } else {
    points_table[points_table$team %in% c(home_team, away_team), "points"] <- points_table[points_table$team %in% c(home_team, away_team), "points"] + 1
  }
  return(points_table)
}

# Funksjon for å kjøre en enkelt sesong og returnere poengtabellen
run_simulation <- function(teams, fixture, avg_stats) {
  # Initialiserer en poengtavle for alle lagene.
  points_table <- data.frame(team = teams, points = rep(0, length(teams)))
  
  # Gjennomfører en simulering for hver kamp i kampplanen.
  for (i in 1:nrow(fixture)) {
    result <- simulate_match(fixture[i, "home"], fixture[i, "away"], avg_stats)
    points_table <- assign_points(result, fixture[i, "home"], fixture[i, "away"], points_table)
  }
  
  return(points_table)
}

# Funksjon for å kjøre flere sesongsimuleringer og beregne en gjennomsnittlig liga-tabell
run_multiple_simulations <- function(num_simulations, teams, fixture, avg_stats) {
  # Oppretter en liste for å lagre resultatene fra hver simulering.
  all_simulations <- list()
  
  # Gjennomfører det angitte antallet simuleringer.
  for (s in 1:num_simulations) {
    simulation_result <- run_simulation(teams, fixture, avg_stats)
    all_simulations[[s]] <- simulation_result
  }
  
  # Kombinerer resultatene fra alle simuleringene til en enkelt data.frame.
  combined_simulations <- do.call(rbind, all_simulations)
  
  # Beregner gjennomsnittlig poeng for hvert lag basert på alle simuleringene.
  avg_points <- aggregate(points ~ team, combined_simulations, mean)
  
  # Runder av poengene til nærmeste heltall for realistisk poengtelling.
  avg_points$points <- round(avg_points$points)
  
  # Sorterer lagene basert på gjennomsnittspoeng og rangerer dem.
  avg_league_table <- avg_points %>%
    arrange(desc(points)) %>%
    mutate(rank = row_number())
  
  return(avg_league_table)
}

# Angir antall simuleringer som skal kjøres.
num_simulations <- 200

# Kjører de angitte simuleringene og viser den gjennomsnittlige liga-tabellen.
avg_league_table <- run_multiple_simulations(num_simulations, teams, fixture, avg_stats)
print(avg_league_table)

# Skriver ut tabellen til Excel datafil.
write_xlsx(avg_league_table, "C:/Users/moon/LigaTabell.xlsx")


