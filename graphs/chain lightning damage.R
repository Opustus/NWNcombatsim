## Old chain lightning


old_chain = function(targets)
{
  if (targets > 2)
  {
    dmg = 120 + 60*6 + (targets-2)*7*30+30
  }
  if (targets == 2)
  {
    dmg = 120 + 60*6 + 30
  }
  if (targets == 1)
  {
    dmg = 120
  }
  return(dmg)
}
old_chain(10)


targets <- 0
x <- 0
dmg <- c()

new_chain = repeat{
  targets <- targets+1
  x = 120 * targets
  dmg[x] <- x
  print(dmg)
  
  if (targets > 10) break
}

  
  
  

new_chain = function(targets)
{
  if (targets == 1)
  {
    dmg = 120
  }
  else
  {
    dmg = 120 * 6
  }
  return(dmg)
}
