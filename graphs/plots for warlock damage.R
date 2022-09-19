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


## Perilous flames and AoE stacking

AoE1=13.5+13.5
AoE2=AoE1+14+27*1.1
AoE3=AoE2+27*1.1+27*1.1
AoE4=AoE3+27*1.1+27*1.1
AoE5=AoE4+27*1.1+27*1.1
AoE6=AoE5+27*1.1+27*1.1
AoE7=AoE6+27*1.1+27*1.1
AoE8=AoE7+27*1.1
AoE9=AoE8
AoE10=AoE9

AoE=c(AoE1, AoE2, AoE3, AoE4, AoE5, AoE6, AoE7, AoE8, AoE9, AoE10)

## AoE stacking and Eldritch

eldritch_i = function(i){
  eldritch_i=damage_new_average[i]
  dmg <- AoE1 + 14 + eldritch_i
  return(dmg)
  }

eldritch_16thru30=eldritch_i(16:30)


e16=c(AoE1, rep(eldritch_16thru30[1],9))
e17=c(AoE1, rep(eldritch_16thru30[2],9))
e18=c(AoE1, rep(eldritch_16thru30[3],9))
e19=c(AoE1, rep(eldritch_16thru30[4],9))
e20=c(AoE1, rep(eldritch_16thru30[5],9))
e21=c(AoE1, rep(eldritch_16thru30[6],9))
e22=c(AoE1, rep(eldritch_16thru30[7],9))
e23=c(AoE1, rep(eldritch_16thru30[8],9))
e24=c(AoE1, rep(eldritch_16thru30[9],9))
e25=c(AoE1, rep(eldritch_16thru30[10],9))
e26=c(AoE1, rep(eldritch_16thru30[11],9))
e27=c(AoE1, rep(eldritch_16thru30[12],9))
e28=c(AoE1, rep(eldritch_16thru30[13],9))
e29=c(AoE1, rep(eldritch_16thru30[14],9))
e30=c(AoE1, rep(eldritch_16thru30[15],9))

rounds=1:10

?matplot
damage_rounds=data.frame(AoE, e16, e17, e18, e19, e20, e21, e22, e23, e24, e25, e26, e27, e28, e29, e30)
matplot(rounds, damage_rounds, type="l", ylab="damage/round", xaxt="n")
axis(1, rounds)
legend(1, 380, legend=c("AoE stacking lv 16","long AoE+blast lv 16 thru 30"), lty=1, col=c("black","red"), cex=0.7)
       