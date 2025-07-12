
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


monk_30_wis_flurry <- damage_to_ac_range(
  ab = 46, base_apr = 5, ubab = TRUE, haste = TRUE, flurry = TRUE,
  crit_range = 0.15, crit_threat = 2, dmg_per_hit = 39.5, sneak_per_hit = 0
)

monk_30_wis <- damage_to_ac_range(
  ab = 46, base_apr = 5, ubab = TRUE, haste = TRUE, flurry = FALSE,
  crit_range = 0.15, crit_threat = 2, dmg_per_hit = 39.5, sneak_per_hit = 0
)

monk_30_str_no_2hand <- damage_to_ac_range(
  ab = 46, base_apr = 5, ubab = TRUE, haste = TRUE, flurry = TRUE,
  crit_range = 0.15, crit_threat = 2, dmg_per_hit = 51, sneak_per_hit = 0
)

monk_30_str <- damage_to_ac_range(
  ab = 48, base_apr = 5, ubab = TRUE, haste = TRUE, flurry = FALSE,
  crit_range = 0.15, crit_threat = 2, dmg_per_hit = 58, sneak_per_hit = 0
)

monk_25_sd5_flurry <- damage_to_ac_range(
  ab = 45, base_apr = 5, ubab = TRUE, haste = TRUE, flurry = TRUE,
  crit_range = 0.15, crit_threat = 2, dmg_per_hit = 37.5, sneak_per_hit = 0
)

monk_25_sd5 <- damage_to_ac_range(
  ab = 45, base_apr = 5, ubab = TRUE, haste = TRUE, flurry = FALSE,
  crit_range = 0.15, crit_threat = 2, dmg_per_hit = 37.5, sneak_per_hit = 0
)

monk_20_sd5_poe5_flurry <- damage_to_ac_range(
  ab = 45, base_apr = 5, ubab = TRUE, haste = TRUE, flurry = TRUE,
  crit_range = 0.15, crit_threat = 2, dmg_per_hit = 43, sneak_per_hit = 0
)

monk_20_sd5_poe5 <- damage_to_ac_range(
  ab = 45, base_apr = 5, ubab = TRUE, haste = TRUE, flurry = FALSE,
  crit_range = 0.15, crit_threat = 2, dmg_per_hit = 43, sneak_per_hit = 0
)

monk_25_cav5_flurry <- damage_to_ac_range(
  ab = 50, base_apr = 6, ubab = TRUE, haste = TRUE, flurry = TRUE,
  crit_range = 0.15, crit_threat = 2, dmg_per_hit = 58, sneak_per_hit = 0
)

monk_25_cav5 <- damage_to_ac_range(
  ab = 50, base_apr = 6, ubab = TRUE, haste = TRUE, flurry = FALSE,
  crit_range = 0.15, crit_threat = 2, dmg_per_hit = 58, sneak_per_hit = 0
)

monk_25_fighter5_flurry <- damage_to_ac_range(
  ab = 48, base_apr = 6, ubab = TRUE, haste = TRUE, flurry = TRUE,
  crit_range = 0.15, crit_threat = 2, dmg_per_hit = 62, sneak_per_hit = 0
)

monk_25_fighter5 <- damage_to_ac_range(
  ab = 48, base_apr = 6, ubab = TRUE, haste = TRUE, flurry = FALSE,
  crit_range = 0.15, crit_threat = 2, dmg_per_hit = 62, sneak_per_hit = 0
)

monk_19_fighter4_wm7 <- damage_to_ac_range(
  ab = 49, base_apr = 4, ubab = FALSE, haste = TRUE, flurry = FALSE,
  crit_range = 0.4, crit_threat = 3, dmg_per_hit = 62, sneak_per_hit = 0
)

monk_18_cav5_wm7 <- damage_to_ac_range(
  ab = 51, base_apr = 4, ubab = FALSE, haste = TRUE, flurry = FALSE,
  crit_range = 0.4, crit_threat = 2, dmg_per_hit = 62, sneak_per_hit = 0
)

monk_19_fighter4_wm7_staff <- damage_to_ac_range(
  ab = 49, base_apr = 6, ubab = TRUE, haste = TRUE, flurry = TRUE,
  crit_range = 0.25, crit_threat = 3, dmg_per_hit = 60, sneak_per_hit = 0
)

monk_18_cav5_wm7_staff <- damage_to_ac_range(
  ab = 51, base_apr = 6, ubab = TRUE, haste = TRUE, flurry = TRUE,
  crit_range = 0.25, crit_threat = 3, dmg_per_hit = 58, sneak_per_hit = 0
)
