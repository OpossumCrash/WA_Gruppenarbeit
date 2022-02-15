#a)Eine Funktion, die verschiedene geeignete deskriptive Statistiken
#  für metrische Variablen berechnet und ausgibt.

deskr_metr <- function(x){
  n = length(x)
  quant <- quantile(x, c(0.25, 0.5, 0.75))
  q_abst <- quant[3] - quant[1]
  names(q_abst) <- NULL
  mqa <- sum((x - mean(x))^2) / n
  
  
  return(list("Mittelwert" = mean(x),
              "Quantile" = quant,
              "Extrempunkte" = range(x),
              "Quartilabstand" = q_abst,
              "MQA" = mqa,
              "Variationskoeffizient" = (sqrt(mqa) / mean(x))
              ))
}

#b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken für
#kategoriale Variablen berechnet und ausgibt


#c) Eine Funktion, die geeignete deskriptive bivariate Statistiken für den
#Zusammenhang zwischen zwei kategorialen Variablen berechnet ausgibt


#d) Eine Funktion, die geeignete deskriptive bivariate Statistiken für den
#Zusammengang zwischen einer metrischen und einer dichotomen Variablen 
#berechnet und ausgibt


#e) Eine Funktion, die eine mindestens ordinal skalierte Variable quantilbasiert
#kategorisiert (z.B. in „niedrig“, „mittel“, „hoch“)
quant_kat_ord <- function(x){
  categories <- numeric(length(x))
  q <- quantile(x, c(0.25, 0.75))
  categories[x < q[1]] <- "niedrig"
  categories[x > q[2]] <- "hoch"
  categories[categories == "0"] <- "mittel"

  return(categories)
}

quant_kat_ord(x)


#f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier 
#kategorialen Variablen erstellt

