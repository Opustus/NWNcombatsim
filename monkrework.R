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


damage = function(ab, base_apr, ubab, haste, flurry, enemy_ac, crit_range, crit_threat, damage, sneak_attack)
{
  crits = crits(ab, base_apr, ubab, haste, flurry, enemy_ac, crit_range)
  hits = hits(ab, base_apr, ubab, haste, flurry, enemy_ac)
  hits = hits - crits
  hits_damage = hits * (damage + sneak)
  crits_damage = crits * damage * crits * crit_threat
  damage = (hits_damage + crits_damage) / 20
  return(damage)
}

damage(50, 5, ubab = T, haste = T, flurry = T, 66, 0.15, )