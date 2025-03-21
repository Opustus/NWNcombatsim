roll20 = 1:20

attacks = function(ab, baseApr, ubab = F)
{
	firstAttackAb = ab 
	attackDecrement = -5
	
	if (ubab == T)
	{
		attackDecrement = -3
	}

	lastAttackAb = firstAttackAb - attackDecrement * (baseApr-1)
	seq(from = firstAttackAb, to = lastAttackAb, by = attackDecrement)
}

attacks_dataframe = function(ab, baseApr, ubab = F, haste = T, flurry = F)
{
	dataframe <- data.frame()
	
	for (i in 1:baseApr)
	{
		dataframe <- cbind(attacks(ab, baseApr, ubab)[i]+roll20)
	}
  
	if (flurry == T || haste == T)
	{
    		mat = cbind(c1,c1,c2,c3,c4)
	}
	
  	if (haste == T && rapid == T)
  	{
    		mat = cbind(c1,c1,c1,c2,c3,c4)
  	}
	
  	return(mat)
}



