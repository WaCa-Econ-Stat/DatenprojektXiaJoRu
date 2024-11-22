x <- matrix(1:9,ncol=3)
apply(x, 1, function(z)  z[2]) #1. Zeile 2. Element

Bino <- function(Alpha, k){
  a <- Alpha
  Produkt = 1
  for(j in 1:k){
    Produkt = prod(Produkt,
                   (a+1-j) / j
                   )
  }
  return(Produkt)
}
Bino(6,2)
