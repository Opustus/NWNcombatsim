roll20 = 1:20

attacks = function(ab, base_apr, ubab = T)
{
  first_attack_ab = ab 
  
  attack_decrement = -5
  
  if (ubab == T)
    attack_decrement = -3
  
  last_attack_ab = first_attack_ab + attack_decrement * (base_apr-1)
  seq(from = first_attack_ab, to = last_attack_ab, by = attack_decrement)
}

attacks_df = function(ab, base_apr, ubab = F, haste = T, flurry = F)
{
  dataframe <- data.frame("1" = c(roll20))
  
  for (i in 1:base_apr)
  {
    dataframe[,i] = c(attacks(ab, base_apr, ubab)[i]+roll20)
  }
  
  if (haste == T)
    dataframe$haste = c(attacks(ab, base_apr, ubab)[1]+roll20)
  
  if (flurry == T)
  {
    dataframe$flurry = c(attacks(ab, base_apr, ubab)[1]+roll20)
    dataframe = dataframe - 2
  }

  
  return(dataframe)
}

attacks(50, 5, ubab = T)
attacks_df(50, 5, ubab = T, haste = T, flurry = T)

hits = function(ab, base_apr, ubab, haste, flurry, enemy_ac)
{
  hits_matrix = attacks_df(ab, base_apr, ubab, haste, flurry) - enemy_ac
  hits_matrix[1,] <- -1
  hits_matrix[20,] <- 1
  hits = length(hits_matrix[hits_matrix >= 0])
  return(hits)
}

hits(50, 5, ubab = T, haste = T, flurry = T, 66)

crits = function(ab, base_apr, ubab, haste, flurry, enemy_ac, crit_range)
{
  hits(ab, base_apr, ubab, haste, flurry, enemy_ac) * crit_range
}

crits(50, 5, ubab = T, haste = T, flurry = T, 66, 0.15)


damage = function(ab, base_apr, ubab, haste, flurry, enemy_ac, crit_range, crit_threat, dmg_per_hit, sneak_per_hit)
{
  crits = crits(ab, base_apr, ubab, haste, flurry, enemy_ac, crit_range)
  hits = hits(ab, base_apr, ubab, haste, flurry, enemy_ac)
  hits = hits - crits
  hits_damage = hits * (dmg_per_hit + sneak_per_hit)
  crits_damage = dmg_per_hit * crits * crit_threat
  damage = (hits_damage + crits_damage) / 20
  return(damage)
}


damage_to_ac_range = function(ab, base_apr, ubab, haste, flurry, crit_range, crit_threat, dmg_per_hit, sneak_per_hit)
{
  ac_range40to50 = c()
  ac_range50to60 = c()
  ac_range60to70 = c()
  ac_range70to80 = c()
  for (enemy_ac in 40:50)
  {
    out <- damage(ab, base_apr, ubab, haste, flurry, enemy_ac, crit_range, crit_threat, dmg_per_hit, sneak_per_hit)
    ac_range40to50 <- c(ac_range40to50, out)
  }
  for (enemy_ac in 50:60)
  {
    out <- damage(ab, base_apr, ubab, haste, flurry, enemy_ac, crit_range, crit_threat, dmg_per_hit, sneak_per_hit)
    ac_range50to60 <- c(ac_range50to60, out)
  }
  for (enemy_ac in 60:70)
  {
    out <- damage(ab, base_apr, ubab, haste, flurry, enemy_ac, crit_range, crit_threat, dmg_per_hit, sneak_per_hit)
    ac_range60to70 <- c(ac_range60to70, out)
  }
  for (enemy_ac in 70:80)
  {
    out <- damage(ab, base_apr, ubab, haste, flurry, enemy_ac, crit_range, crit_threat, dmg_per_hit, sneak_per_hit)
    ac_range70to80 <- c(ac_range70to80, out)
  }
  
  mean_damage_by_ac_range <- data.frame(mean(ac_range40to50), mean(ac_range50to60), mean(ac_range60to70), mean(ac_range70to80))
  names(mean_damage_by_ac_range) <- c("vs AC 40 to 50", "vs AC 50 to 60", "vs AC 60 to 70", "vs AC 70 to 80")
  
  return (mean_damage_by_ac_range)
}
Monk30WisFlurry = damage_to_ac_range(ab = 46,
                                     base_apr = 5, 
                                     ubab = T,
                                     haste = T, 
                                     flurry = T, 
                                     crit_range = 0.15, 
                                     crit_threat = 2, 
                                     dmg_per_hit = 39.5, 
                                     sneak_per_hit = 0)

Monk30Wis = damage_to_ac_range(ab = 46,
                               base_apr = 5, 
                               ubab = T,
                               haste = T, 
                               flurry = F, 
                               crit_range = 0.15, 
                               crit_threat = 2, 
                               dmg_per_hit = 39.5, 
                               sneak_per_hit = 0)

Monk30StrNo2Hand = damage_to_ac_range(ab = 46,
                                     base_apr = 5, 
                                     ubab = T,
                                     haste = T, 
                                     flurry = T, 
                                     crit_range = 0.15, 
                                     crit_threat = 2, 
                                     dmg_per_hit = 51, 
                                     sneak_per_hit = 0)


Monk30Str = damage_to_ac_range(ab = 48,
                               base_apr = 5, 
                               ubab = T,
                               haste = T, 
                               flurry = F, 
                               crit_range = 0.15, 
                               crit_threat = 2, 
                               dmg_per_hit = 58, 
                               sneak_per_hit = 0)

Monk25SD5Flurry = damage_to_ac_range(ab = 45,
                                      base_apr = 5, 
                                      ubab = T,
                                      haste = T, 
                                      flurry = T, 
                                      crit_range = 0.15, 
                                      crit_threat = 2, 
                                      dmg_per_hit = 37.5, 
                                      sneak_per_hit = 0)

Monk25SD5 = damage_to_ac_range(ab = 45,
                              base_apr = 5, 
                              ubab = T,
                              haste = T, 
                              flurry = F, 
                              crit_range = 0.15, 
                              crit_threat = 2, 
                              dmg_per_hit = 37.5, 
                              sneak_per_hit = 0)


Monk20SD5POE5Flurry = damage_to_ac_range(ab = 45,
                                         base_apr = 5, 
                                         ubab = T,
                                         haste = T, 
                                         flurry = T, 
                                         crit_range = 0.15, 
                                         crit_threat = 2, 
                                         dmg_per_hit = 43, 
                                         sneak_per_hit = 0)

Monk20SD5POE5 = damage_to_ac_range(ab = 45,
                                   base_apr = 5, 
                                   ubab = T,
                                   haste = T, 
                                   flurry = F, 
                                   crit_range = 0.15, 
                                   crit_threat = 2, 
                                   dmg_per_hit = 43, 
                                   sneak_per_hit = 0)

Monk25Cav5Flurry = damage_to_ac_range(ab = 50,
                                     base_apr = 6, 
                                     ubab = T,
                                     haste = T, 
                                     flurry = T, 
                                     crit_range = 0.15, 
                                     crit_threat = 2, 
                                     dmg_per_hit = 58, 
                                     sneak_per_hit = 0)

Monk25Cav5 = damage_to_ac_range(ab = 50,
                               base_apr = 6, 
                               ubab = T,
                               haste = T, 
                               flurry = F, 
                               crit_range = 0.15, 
                               crit_threat = 2, 
                               dmg_per_hit = 58, 
                               sneak_per_hit = 0)

Monk25Fighter5Flurry = damage_to_ac_range(ab = 48,
                                     base_apr = 6, 
                                     ubab = T,
                                     haste = T, 
                                     flurry = T, 
                                     crit_range = 0.15, 
                                     crit_threat = 2, 
                                     dmg_per_hit = 62, 
                                     sneak_per_hit = 0)

Monk25Fighter5 = damage_to_ac_range(ab = 48,
                               base_apr = 6, 
                               ubab = T,
                               haste = T, 
                               flurry = F, 
                               crit_range = 0.15, 
                               crit_threat = 2, 
                               dmg_per_hit = 62, 
                               sneak_per_hit = 0)

Monk19Fighter4WM7 = damage_to_ac_range(ab = 49,
                                    base_apr = 4, 
                                    ubab = F,
                                    haste = T, 
                                    flurry = F, 
                                    crit_range = 0.4, 
                                    crit_threat = 3, 
                                    dmg_per_hit = 62, 
                                    sneak_per_hit = 0)

Monk18Cav5WM7 = damage_to_ac_range(ab = 51,
                                    base_apr = 4, 
                                    ubab = F,
                                    haste = T, 
                                    flurry = F, 
                                    crit_range = 0.4, 
                                    crit_threat = 2, 
                                    dmg_per_hit = 62, 
                                    sneak_per_hit = 0)

Monk19Fighter4WM7STAFF = damage_to_ac_range(ab = 49,
                                       base_apr = 6, 
                                       ubab = T,
                                       haste = T, 
                                       flurry = T, 
                                       crit_range = 0.25, 
                                       crit_threat = 3, 
                                       dmg_per_hit = 60, 
                                       sneak_per_hit = 0)

Monk18Cav5WM7STAFF = damage_to_ac_range(ab = 51,
                                   base_apr = 6, 
                                   ubab = T,
                                   haste = T, 
                                   flurry = T, 
                                   crit_range = 0.25, 
                                   crit_threat = 3, 
                                   dmg_per_hit = 58, 
                                   sneak_per_hit = 0)

Monk18Cav5WM7STAFF = damage_to_ac_range(ab = 49,
                                        base_apr = 6, 
                                        ubab = T,
                                        haste = T, 
                                        flurry = T, 
                                        crit_range = 0.25, 
                                        crit_threat = 3, 
                                        dmg_per_hit = 51, 
                                        sneak_per_hit = 0)
