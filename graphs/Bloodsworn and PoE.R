HP=400

damage_poe <- function(n) {
  sapply(
    n,
    function(x) sum(seq(20, by=5, length = x))
  )
}

damage_bs <- function(n) {
   n*sum(seq(from=HP/2/10,to=HP/20))
}

sum(seq(from=HP/2/10,to=HP/20))

rounds = 1:10
damage_bs_10 = damage_bs(1:10)
damage_poe_10 = damage_poe(1:10)

plot(rounds,damage_bs_10, type="b", col="red", ylim=c(0,500), ylab="damage", xaxt = "n")
lines(rounds,damage_poe_10, type="b", col="green")
legend(2, 450, legend=c("PoE", "Bloodsworn"), col=c("green", "red"), lty=1, cex=0.8)
axis(1, at = rounds)



