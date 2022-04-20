install.packages("qrmtools")
library(qrmtools)
library(copula)
Policz_SCR <-function(Sigma,W)
{
  SCR<-sqrt(W%*%Sigma%*%t(W))
  return(SCR)
}

# Definiuje rozklady strat
set.seed(0)
#L_Market <- qnorm(0.995, mean=0,sd=116)
L_Market <- rnorm(1000000, mean=0,sd=116)
set.seed(0)
L_Credit <- 2.175e05*(rbeta(1000000, 0.58, 1954))
set.seed(0)
L_Life <- rnorm(10000000, mean=0,sd=392)
set.seed(0)
L_Health <- rnorm(1000000, mean=0,sd=248)
set.seed(0)
L_Non_Life <- 200*(rlnorm(100000000, meanlog = 0, sdlog = 1))

set.seed(0)
#SCR dla poszczegolnych ryzyk
SCR_Market <- quantile(L_Market,0.995) - mean(L_Market)
SCR_Market
SCR_Life <- quantile(L_Life,0.995) - mean(L_Life)
SCR_Life
mean(L_Non_Life)
SCR_Non_Life <- quantile(L_Non_Life,0.995) - mean(L_Non_Life)
SCR_Non_Life
SCR_Health <- quantile(L_Health,0.995) - mean(L_Health)
SCR_Health
SCR_Credit <- quantile(L_Credit,0.995) - mean(L_Credit)
SCR_Credit
# Standardowa forlula
W <- as.matrix(cbind(SCR_Market,SCR_Credit,SCR_Life,SCR_Health,SCR_Non_Life))
W
sum(W)
#W<-as.matrix(cbind(299.6, 1010.5, 2299.8, 640.7, 400.1))
Sigma <- matrix(c(1,0.25,0.25,0.25,0.25,
                  0.25,1,0.25,0.25,0.5,
                  0.25,0.25,1,0.25,0,
                  0.25,0.25,0.25,1,0,
                  0.25,0.5,0,0,1),nrow = 5)
Sigma1 <- matrix(c(1,0,0,0,0,
                   0,1,0,0,0,
                   0,0,1,0,0,
                   0,0,0,1,0,
                   0,0,0,0,1
                   ),nrow = 5)

Sigma2 <- matrix(c(1,1,1,1,1,
                   1,1,1,1,1,
                   1,1,1,1,1,
                   1,1,1,1,1,
                   1,1,1,1,1
),nrow = 5)

Formula_Standardowa1 = Policz_SCR(Sigma1,W)
Formula_Standardowa1

Formula_Standardowa = Policz_SCR(Sigma,W)
Formula_Standardowa

Formula_Standardowa2 = Policz_SCR(Sigma2,W)
Formula_Standardowa2

sum(W)
1-(Formula_Standardowa/sum(W))
1-(Formula_Standardowa1/sum(W))
1-(Formula_Standardowa2/sum(W))




# Dobor wartosci do kopuli

#Funkcja liczy wartosc korelacji Pearsona dla zadaniej  kopuli tStudenta
# o parametr - parametr kopuli tStudenta
#ryzyko_1 - rozklad dla pierwszej zmiennej ryzyka
#ryzyko_2 - rozklad dla drugiej zmiennej ryzyka
# v-stopniach swobody.
Wspolczynnik_tStudent <- function(parametr,ryzyko_1,ryzyko_2,stopnie_swobody)
{
  set.seed(0)
  t_Copula_df2<- tCopula(param=parametr,dim = 2, 
                         df = stopnie_swobody,dispstr = "un")
  set.seed(0)
  Kopula <- rCopula(200000, t_Copula_df2)
  Rozklady1 <-cbind(qnorm(Kopula[,1],mean = 0, sd = 116),
                    2.175e05*qbeta(Kopula[,1], 0.58, 1954),
                    qnorm(Kopula[,1],mean = 0, sd = 392 ),
                    qnorm(Kopula[,1],mean = 0, sd = 248 ),
                    200*qlnorm(Kopula[,1], meanlog = 0, sdlog = 1))
  Rozklady2 <-cbind(qnorm(Kopula[,2],mean = 0, sd = 116),
                    2.175e05*qbeta(Kopula[,2], 0.58, 1954),
                    qnorm(Kopula[,2],mean = 0, sd = 392 ),
                    qnorm(Kopula[,2],mean = 0, sd = 248 ),
                    200*qlnorm(Kopula[,2], meanlog = 0, sdlog = 1))
  
  return(cor(Rozklady1[,ryzyko_1],Rozklady2[,ryzyko_2]))
  
}

Wspolczynnik_tStudent(0.267,3,4,2)
#Funkcja liczy wartosc korelacji Pearsona dla kopuli Gaussa
#parametr - parametr kopuli
#ryzyko_1 - rozklad dla pierwszej zmiennej ryzyka
#ryzyko_2 - rozklad dla drugiej zmiennej ryzyka
Wspolczynnik_Normal <- function(parametr,ryzyko_1,ryzyko_2)
{
  set.seed(0)
  normCopula <- ellipCopula(family = "normal", dim = 2, dispstr = "un",param = parametr)
  set.seed(0)
  Kopula <- rCopula(1000, normCopula)
  set.seed(0)
  Rozklady1 <-cbind(qnorm(Kopula[,1],mean = 0, sd = 116),
                    2.175e05*qbeta(Kopula[,1], 0.58, 1954),
                    qnorm(Kopula[,1],mean = 0, sd = 392 ),
                    qnorm(Kopula[,1],mean = 0, sd = 248 ),
                    200*qlnorm(Kopula[,1], meanlog = 0, sdlog = 1))
  Rozklady2 <-cbind(qnorm(Kopula[,2],mean = 0, sd = 116),
                    2.175e05*qbeta(Kopula[,2], 0.58, 1954),
                    qnorm(Kopula[,2],mean = 0, sd = 392 ),
                    qnorm(Kopula[,2],mean = 0, sd = 248 ),
                    200*qlnorm(Kopula[,2], meanlog = 0, sdlog = 1))
  
  return(cor(Rozklady1[,ryzyko_1],Rozklady2[,ryzyko_2]))
  
}
#Dla kopuli tStudenta
# W petli wybieram dla jakich stopni swobody chce obliczyc korelacje, nastepnie
#  wybieram dwa ryzyka do badania. SUgeruje sie macierza korelacji, biore odpowiednio
# dwa moduly ryzyka, dla ktorych odpowiada wartosc pod przekatna macierzy
# W ostatniej petli for(parametr in parametry) sprawdzam dla jakiego paratru w kopuli
#korelacja jest rowna korelacji danej w dyrektywie
for(st in c(2,5)){
  cat("Obliczenia dla",st,"stopni swobody","\n")
  for(i in 1:5){
    for(j in 2:5){
      if(i<j){
        if(i==2 & j==5){
          korelacja=0.5
          parametry = seq(0.4,0.8,0.001)
        } else if((i==3 & j==5)||(i==4 & j==5)){
          korelacja=0
          parametry = seq(-0.35,0.2,0.001)
        } else{
          korelacja=0.25
          parametry = seq(0.2,0.4,0.001)
        }
        for(parametr in parametry){
          set.seed(0)
          if(round(Wspolczynnik_tStudent(parametr,i,j,st),3)==korelacja){
            cat("W kolumnie o numerze: ",i, " i wierszu o numerze:",j, "wpisz:",parametr,"\n")
            break
          }
          
        }
      }
    }
  }
}

#Dla kopuli Normalnej
for(i in 1:5){
  for(j in 2:5){
    if(i<j){
      if(i==2 & j==5){
        korelacja=0.5
        parametry = seq(0.5,0.8,0.001)
      } else if((i==3 & j==5)||(i==4 & j==5)){
        korelacja=0
        parametry = seq(-0.35,0.2,0.001)
      } else{
        korelacja=0.25
        parametry = seq(0.2,0.4,0.001)
      }
      for(parametr in parametry){
        if(round(Wspolczynnik_Normal(parametr,i,j),3)==korelacja){
          cat("W kolumnie o numerze: ",i, " i wierszu o numerze:",j, "wpisz:",parametr,"\n")
          break
        }
        
      }
      
    }
  }
}
# Normalna kopula

c(0.284,.243,.243,.304,.291,.291,.586,.243,0,0)
set.seed(0)
normCopula <- normalCopula(c(0.297,0.25,0.25,0.329,0.296,0.296,0.606,0.25,0.001,0.001),
                           dim = 5,dispstr = "un")
set.seed(0)
losowanie_normCopula<- rCopula(1000000, normCopula)
set.seed
rozklady_normCopula <-  cbind(qnorm(losowanie_normCopula[,1],mean = 0, sd = 116 ), 
                              2.175e05*(rbeta(losowanie_normCopula[,2], 0.58, 1954)),
                              qnorm(losowanie_normCopula[,3],mean = 0, sd = 392 ),
                              qnorm(losowanie_normCopula[,4],mean = 0, sd = 248 ),
                              200*qlnorm(losowanie_normCopula[,5], meanlog = 0, sdlog = 1)
                              )

Dystrybuanta_normCopula <- (rozklady_normCopula[,1] + rozklady_normCopula[,2]
                            + rozklady_normCopula[,3]+ rozklady_normCopula[,4]
                            + rozklady_normCopula[,5])
SCR_normCopula <- quantile(Dystrybuanta_normCopula, 0.995) - mean(Dystrybuanta_normCopula)
SCR_normCopula
1-(SCR_normCopula/sum(W))

?tCopula
#Student copula df 2
set.seed(0)
t_Copula_df2 <- tCopula(c(0.304,0.266,0.266,0.334,0.309,0.309,0.491,0.266,-0.005,-0.005) ,dim = 5, 
                        df = 2,dispstr = "un")
set.seed(0)

losowanie_t_Copula_df2 <- rCopula(1000000, t_Copula_df2)
set.seed(0)

rozklady_t_Copula_df2 <- cbind(qnorm(losowanie_t_Copula_df2[,1],mean = 0, sd = 116 ), 
                               2.175e05*(rbeta(losowanie_t_Copula_df2[,2], 0.58, 1954)),
                               qnorm(losowanie_t_Copula_df2[,3],mean = 0, sd = 392 ),
                               qnorm(losowanie_t_Copula_df2[,4],mean = 0, sd = 248 ),
                               200*qlnorm(losowanie_t_Copula_df2[,5], meanlog = 0, sdlog = 1))
Dystrybuanta_t_Copula_df2 <- (rozklady_t_Copula_df2[,1] + rozklady_t_Copula_df2[,2]
                              + rozklady_t_Copula_df2[,3]+ rozklady_t_Copula_df2[,4]
                              + rozklady_t_Copula_df2[,5])
SCR_t_Copula_df2 <- quantile(Dystrybuanta_t_Copula_df2, 0.995) - mean(Dystrybuanta_t_Copula_df2)
SCR_t_Copula_df2
1-(SCR_t_Copula_df2/sum(W))

#Student copula df 5
set.seed(0)
t_Copula_df5<- tCopula(c(0.297,0.253,0.253,0.323,0.294,0.294,0.543,0.253,0.001,0.001) ,dim = 5, 
                        df = 5,dispstr = "un")
set.seed(0)
losowanie_t_Copula_df5 <- rCopula(1000000, t_Copula_df5)
set.seed(0)
rozklady_t_Copula_df5 <- cbind(qnorm(losowanie_t_Copula_df5[,1],mean = 0, sd = 116 ), 
                               2.175e05*(rbeta(losowanie_t_Copula_df5[,2], 0.58, 1954)),
                               qnorm(losowanie_t_Copula_df5[,3],mean = 0, sd = 392 ),
                               qnorm(losowanie_t_Copula_df5[,4],mean = 0, sd = 248 ),
                               200*qlnorm(losowanie_t_Copula_df5[,5], meanlog = 0, sdlog = 1))
Dystrybuanta_t_Copula_df5 <- (rozklady_t_Copula_df5[,1] + rozklady_t_Copula_df5[,2]
                              + rozklady_t_Copula_df5[,3]+ rozklady_t_Copula_df5[,4]
                              + rozklady_t_Copula_df5[,5])
SCR_t_Copula_df5 <- quantile(Dystrybuanta_t_Copula_df5, 0.995) - mean(Dystrybuanta_t_Copula_df5)
SCR_t_Copula_df5
1-(SCR_t_Copula_df5/sum(W))

 #Algorytm ARA

qF <- list(qMarket = function(p) qnorm(p,mean = 0, sd = 116),
           qCredit = function(p) 2.175e05*(qbeta(p, 0.58, 1954)),
           qLife = function(p) qnorm(p,mean = 0, sd = 392),
           qHealth = function(p) qnorm(p, mean=0,sd=248),
           qNonLife = function(p) 200*(qlnorm(p, meanlog = 0, sdlog = 1)))

set.seed(0) 
?ARA
wVaRwarst <- ARA(0.995, qF = qF)
ARA_Var_warst <- wVaRwarst$bounds[2]
wVaRbest <- ARA(0.995, qF = qF, method = "best.VaR")
ARA_Var_best <- wVaRbest$bounds[1]
ARA_Var_best
1-ARA_Var_best/sum(W)
ARA_Var_warst
1-ARA_Var_warst/sum(W)



