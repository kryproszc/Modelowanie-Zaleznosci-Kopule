################ VINE COPULA
library ('VineCopula')
library (TSP) 
set.seed(0)
n<-1000
y_1 <- rnorm(n, mean=0,sd=116)
y_2 <- 2.175e05*(rbeta(n, 0.58, 1954))
y_3 <- rnorm(n, mean=0,sd=392)
y_4 <- rnorm(n, mean=0,sd=248)
y_5 <- 200*(rlnorm(n, meanlog = 0, sdlog = 1))
dane<-data.frame(y_1, y_2, y_3, y_4, y_5)
colnames(dane)<-c('Market','Couterp.','Life','Health','Non Life')
dane

#Przygotowanie dane pseudo-kopulowe
dane_kopulowe<-pobs (dane)
dane_kop<-data.frame(dane_kopulowe)
copula<-as.copuladata (dane_kop) 

#rodziny kopul brane w symulacjach
familyset=c(1,2,3,4,5,6,7,8,9,10)

#Dopasowanie kopuli C-vine

C_vine<- RVineStructureSelect(data =copula ,
                              type = 1,
                              familyset  = familyset, 
                              treecrit ='tau',
                              selectioncrit = 'logLik',
                              rotations = TRUE, method = 'mle' ) 

C_vine
summary(C_vine)
par(mfrow=c(1,1))
plot(C_vine, edge.labels = "family-tau")

#Symulowanie obserwacji z kopuli C-vine i obliczanie SCR

familyC <- C_vine$family
parC<-C_vine$par
parC2<-C_vine$par2
MatrixC<-C_vine$Matrix

RVMC = RVineMatrix(Matrix=MatrixC,family=familyC,par=parC,
                   par2=parC2, names=c('Market','Couterp.','Life','Health','Non Life'))
RVMC$type

Data_C_vine = as.copuladata(RVineSim(10000,RVMC))


rozklady_C_vine <- cbind(qnorm(Data_C_vine[,1],mean = 0, sd = 116 ), 
                               2.175e05*(qbeta(Data_C_vine[,2], 0.58, 1954)),
                               qnorm(Data_C_vine[,3],mean = 0, sd = 392 ),
                               qnorm(Data_C_vine[,4],mean = 0, sd = 248 ),
                               200*qlnorm(Data_C_vine[,5], meanlog = 0, sdlog = 1))
Dystrybuanta_C_vine <- (rozklady_C_vine[,1] + rozklady_C_vine[,2]
                              + rozklady_C_vine[,3]+ rozklady_C_vine[,4]
                              + rozklady_C_vine[,5])
SCR_c_vine <- quantile(Dystrybuanta_C_vine, 0.995) - mean(Dystrybuanta_C_vine)
SCR_c_vine

#Dopasowanie kopuli D-vine

d <-dim(copula ) [ 2 ] 
M<-1-abs(TauMatrix( copula ) )
hamilton <-TSP::insert_dummy(TSP(M) , label = 'cut' )
sol <- solve_TSP(hamilton, 
                 method='repetitive_nn' ) 
order <- cut_tour( sol , 'cut' )
DVM<- D2RVine(order ,
              family = rep(0 , d*(d-1)/2) ,
              par = rep(0 , d*(d-1)/2)) 
D_vine<- RVineCopSelect ( data =copula , family =familyset,
                          Matrix = DVM$Matrix , selectioncrit = 'logLik',
                          method = 'mle' , rotations = TRUE)

familyD <- D_vine$family
parD<-D_vine$par
parD2<-D_vine$par2
MatrixD<-D_vine$Matrix

RVMD = RVineMatrix(Matrix=MatrixD,family=familyD,par=parD,par2=parD2,
                   names=c('Market','Couterp.','Life','Health','Non Life'))
RVMD$type

Data_D_vine = as.copuladata(RVineSim(10000,RVMD))
rozklady_D_vine <- cbind(qnorm(Data_D_vine[,1],mean = 0, sd = 116 ), 
                         2.175e05*(rbeta(Data_D_vine[,2], 0.58, 1954)),
                         qnorm(Data_D_vine[,3],mean = 0, sd = 392 ),
                         qnorm(Data_D_vine[,4],mean = 0, sd = 248 ),
                         200*qlnorm(Data_D_vine[,5], meanlog = 0, sdlog = 1))
Dystrybuanta_D_vine <- (rozklady_D_vine[,1] + rozklady_D_vine[,2]
                        + rozklady_D_vine[,3]+ rozklady_D_vine[,4]
                        + rozklady_D_vine[,5])
SCR_D_vine <- quantile(Dystrybuanta_D_vine, 0.995) - mean(Dystrybuanta_D_vine)
SCR_D_vine
summary(D_vine)

#Dopasowanie kopuli R-vine

R_vine<- RVineStructureSelect(data =copula ,
                              type = 0,
                              familyset  = familyset, 
                              treecrit ='tau',
                              selectioncrit = 'logLik',
                              rotations = TRUE, method = 'mle' ) 

R_vine
summary(R_vine)
plot(R_vine, edge.labels = "family-tau")

#Symulowanie z kopuli R-vine i obliczanie SCR

familyR <- R_vine$family
parR<-R_vine$par
parR2<-R_vine$par2
MatrixR<-R_vine$Matrix
RVMR = RVineMatrix(Matrix=MatrixR,family=familyR,par=parR,
                   par2=parR2, names=c('Market','Couterp.','Life','Health','Non Life'))
RVMR$type
Data_R_vine = as.copuladata(RVineSim(10000,RVMR))
set.seed(0)
rozklady_R_vine <- cbind(qnorm(Data_R_vine[,1],mean = 0, sd = 116 ), 
                         2.175e05*(rbeta(Data_R_vine[,2], 0.58, 1954)),
                         qnorm(Data_R_vine[,3],mean = 0, sd = 392 ),
                         qnorm(Data_R_vine[,4],mean = 0, sd = 248 ),
                         200*qlnorm(Data_R_vine[,5], meanlog = 0, sdlog = 1))
Dystrybuanta_R_vine <- (rozklady_R_vine[,1] + rozklady_R_vine[,2]
                        + rozklady_R_vine[,3]+ rozklady_R_vine[,4]
                        + rozklady_R_vine[,5])
SCR_R_vine <- quantile(Dystrybuanta_R_vine, 0.995) - mean(Dystrybuanta_R_vine)

#Wyniki badań
plot(C_vine, tree="ALL",edge.labels = "family-tau",type=1)



D_vine
plot(D_vine,tree="ALL", edge.labels = "family-tau",type=1)

summary(C_vine)
summary(D_vine)


C_cine_dywersyfikacja<-(1-SCR_c_vine/4650)
D_cine_dywersyfikacja<-(1-SCR_D_vine/4650)

SCR_c_vine
C_cine_dywersyfikacja
SCR_D_vine
D_cine_dywersyfikacja



