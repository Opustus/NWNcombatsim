roll20 = 1:20

attackSequence = function(ab, baseApr, ubab = F)
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



