level=c(1:30)

damage_old_min=1*level/2
damage_old_min[26:30] <- damage_old_min[26:30]*1.5
damage_old_min[6:30] <-  damage_old_min[6:30]*2
damage_old_max=6*level/2
damage_old_max[26:30] <- damage_old_max[26:30]*1.5
damage_old_max[6:30] <- damage_old_max[6:30]*2
damage_old_both=cbind(damage_old_min,damage_old_max)
damage_old_sd=apply(damage_old_both, 1, sd)
damage_old_average=(damage_old_min+damage_old_max)/2

cha_mod=c(4,	4,	6,	6,	6,	6,	6,	7,	11,	11,	11,	11,	11,	11,	11,	12,	12,	12,	12,	12,	12,	12,	13,	13,	13,	13,	13,	14,	14,	14)
damage_new_min=1*level+cha_mod
damage_new_min[21:30] <- damage_new_min[21:30]*1.25
damage_new_min[6:30] <-  damage_new_min[6:30]*2
damage_new_max=2*level+cha_mod
damage_new_max[21:30] <- damage_new_max[21:30]*1.25
damage_new_max[6:30] <-  damage_new_max[6:30]*2
damage_new_both=cbind(damage_new_min,damage_new_max)
damage_new_sd=apply(damage_new_both, 1, sd)
damage_new_average=(damage_new_min+damage_new_max)/2


plot(level,damage_new_average,type="l",col="red",ylim=c(0,200), ylab="dmg average / round")
lines(damage_old_average, col="blue")
arrows(level,damage_new_average-damage_new_sd/2,level,damage_new_average+damage_new_sd/2, code=3, length=0.02, angle = 90, col="red")
arrows(level,damage_old_average-damage_old_sd/2,level,damage_old_average+damage_old_sd/2, code=3, length=0.02, angle = 90, col="blue")
legend(2, 160, legend=c("old damage", "new damage"), lty=1, col=c("blue", "red")) 

plot(level,damage_old_average,type="l",col="blue",ylim=c(0,200), ylab="dmg average / round")
arrows(level,damage_old_average-damage_old_sd/2,level,damage_old_average+damage_old_sd/2, code=3, length=0.02, angle = 90, col="blue")
legend(2, 160, legend="old damage", lty=1, col="blue")


plot(level,damage_new_average,type="l",col="red",ylim=c(0,200), ylab="dmg average / round")
arrows(level,damage_new_average-damage_new_sd/2,level,damage_new_average+damage_new_sd/2, code=3, length=0.02, angle = 90, col="red")
legend(2, 160, legend="new damage", lty=1, col="red")
