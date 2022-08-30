HP=400

damage_poe <- function(n) {
  sapply(
    n,
    function(x) sum(seq(20, by=5, length = x))
  )
}

damage_poe2 <- function(n) {
  sapply(
    n,
    function(x) sum(seq(0, by=5, length = x))
  )
}
damage_poe2(1:10)

damage_poe3 <- function(n) {
  sapply(
    n,
    function(x) sum(seq(20, by=2, length = x))
  )
}
damage_poe3(1:10)

rounds = 1:15
plot(rounds,damage_poe(rounds), type="b", col="red", ylim=c(0,700), ylab="damage", xaxt = "n")
axis(1, at = rounds)
lines(rounds,damage_poe2(rounds), type="b", col="green")
lines(rounds,damage_poe3(rounds), type="b", col="blue")
legend(2, 600, legend=c("20+5", "20+2", "0+5"), col=c("red","blue","green"), lty=1, cex=0.8)



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



