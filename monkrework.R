
roll20 <- 1:20

attacks <- function(ab, base_apr, ubab = TRUE) {
  first_attack_ab <- ab
  attack_decrement <- if (ubab) -3 else -5
  last_attack_ab <- first_attack_ab + attack_decrement * (base_apr - 1)
  seq(from = first_attack_ab, to = last_attack_ab, by = attack_decrement)
}

attacks_df <- function(ab, base_apr, ubab = FALSE, haste = TRUE, flurry = FALSE) {
  df <- data.frame("1" = roll20)
  
  for (i in 1:base_apr) {
    df[, i] <- attacks(ab, base_apr, ubab)[i] + roll20
  }
  
  if (haste) {
    df$haste <- attacks(ab, base_apr, ubab)[1] + roll20
  }
  
  if (flurry) {
    df$flurry <- attacks(ab, base_apr, ubab)[1] + roll20
    df <- df - 2
  }
  
  return(df)
}

hits <- function(ab, base_apr, ubab, haste, flurry, enemy_ac) {
  hits_matrix <- attacks_df(ab, base_apr, ubab, haste, flurry) - enemy_ac
  hits_matrix[1, ] <- -1
  hits_matrix[20, ] <- 1
  hits <- length(hits_matrix[hits_matrix >= 0])
  return(hits)
}

crits <- function(ab, base_apr, ubab, haste, flurry, enemy_ac, crit_range) {
  hits(ab, base_apr, ubab, haste, flurry, enemy_ac) * crit_range
}

damage <- function(ab, base_apr, ubab, haste, flurry, enemy_ac, crit_range, crit_threat, dmg_per_hit, sneak_per_hit) {
  crits_val <- crits(ab, base_apr, ubab, haste, flurry, enemy_ac, crit_range)
  hits_val <- hits(ab, base_apr, ubab, haste, flurry, enemy_ac) - crits_val
  hits_damage <- hits_val * (dmg_per_hit + sneak_per_hit)
  crits_damage <- dmg_per_hit * crits_val * crit_threat
  total_damage <- (hits_damage + crits_damage) / 20
  return(total_damage)
}

damage_to_ac_range <- function(ab, base_apr, ubab, haste, flurry, crit_range, crit_threat, dmg_per_hit, sneak_per_hit) {
  ac_ranges <- list(
    "vs AC 40 to 50" = 40:50,
    "vs AC 50 to 60" = 50:60,
    "vs AC 60 to 70" = 60:70,
    "vs AC 70 to 80" = 70:80
  )
  
  mean_damages <- sapply(ac_ranges, function(ac_range) {
    sapply(ac_range, function(enemy_ac) {
      damage(ab, base_apr, ubab, haste, flurry, enemy_ac, crit_range, crit_threat, dmg_per_hit, sneak_per_hit)
    }) |> mean()
  })
  
  return(as.data.frame(t(mean_damages)))
}

library(googlesheets4)
library(dplyr)

gs4_auth()

sheet_url <- "https://docs.google.com/spreadsheets/d/1zmUBmbZUbCmm5DpKoAuexaD1uU5RUFVDxE1-vwREUnY"

builds <- read_sheet(sheet_url, sheet = "Builds")

# Compute damage for each build
results <- lapply(1:nrow(builds), function(i) {
  row <- builds[i, ]
  damage_df <- damage_to_ac_range(
    ab = row$ab,
    base_apr = row$base_apr,
    ubab = as.logical(row$ubab),
    haste = as.logical(row$haste),
    flurry = as.logical(row$flurry),
    crit_range = row$crit_range,
    crit_threat = row$crit_threat,
    dmg_per_hit = row$dmg_per_hit,
    sneak_per_hit = row$sneak_per_hit
  )
  return(as.numeric(damage_df[1, ]))
})

damage_matrix <- do.call(rbind, results)
colnames(damage_matrix) <- c("vs_AC_40_50", "vs_AC_50_60", "vs_AC_60_70", "vs_AC_70_80")

# Write only the result columns back to the same sheet
range_write(
  ss = sheet_url,
  data = as.data.frame(damage_matrix),
  sheet = "Builds",
  range = "K2",
  col_names = FALSE
)