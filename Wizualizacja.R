library(latex2exp)
library(tinytex)
library(ggplot2)
library(copula)
a<-TeX('$C_{\\theta}^{Joe}(u_{1}, u_{2})=1-\\left(\\left(1-u_{1}\\right)^{\\theta}+
               \\left(1-u_{2}\\right)^{\\theta}-
               \\left(1-u_{1}\\right)^{\\theta}\\left(1-
               u_{2}\\right)^{\\theta}\\right)^{\\frac{1}{\\theta}}$$
               dla $$\\theta >1.$
                 ')
ggplot(data.frame(rCopula(1000,joeCopula(3.47))),
       xlab='u1',ylab='u2')

plot(rCopula(1000,joeCopula(3.47)),
     xlab='u1',ylab='u2',main =withMathJax(helpText('Kopula Joe:$C_{\\theta}^{Joe}(u_{1}, u_{2})=')),cex.main=0.5)
                                                
                                                
                                                


plot(x, xlim=c(0, 4), ylim=c(0, 10), 
     xlab='x', ylab=TeX(r'($\alpha  x^\alpha$, where $\alpha \in 1 \ldots 5$)'), 
     type='n', main=TeX(r'(Using $\LaTeX$ for plotting in base graphics!)', bold=TRUE, italic=TRUE))

plot(rCopula(1000,joeCopula(3.47)),
     xlab='u1',ylab='u2',main =TeX(('$C_{\\theta}^{Joe}(u_{1}, u_{2})=1-\\left(\\left(1-u_{1}\\right)^{\\theta}+\\left(1-u_{2}\\right)^{\\theta}-\\left(1-u_{1}\\right)^{\\theta}\\left(1-u_{2}\\right)^{\\theta}\\right)^{\\frac{1}{\\theta}}$')),cex.main=1)

plot(rCopula(1000,claytonCopula(2.85)),
     xlab='u1',ylab='u2',main =TeX(('$C_{\\theta}^{Clayton}(u_{1}, u_{2})=\\left(u_{1}^{-\\theta}+u_{2}^{-\\theta}-1\\right)^{-\\frac{1}{\\theta}}$')),cex.main=1)


  
set.seed(0)
plot(rCopula(1000,BB7Copula( c(2,1.2))),
     xlab='u1',ylab='u2',main =TeX('$C_{\\theta, \\delta}^{Joe-Clayton}\\left(u_{1}, u_{2}\\right)=1-\\left[1-\\left(\\left(1-u_{1}^{\\theta}\\right)^{-\\delta}+\\left(1-u_{2}\\right)^{-\\delta}-1\\right)^{-\\frac{1}{\\delta}}\\right]^{\\frac{1}{\\theta}}$'),cex.main=1)

#Claytona obrocone

set.seed(0)
cop <- BiCop(family = 13, par = 2.47)
simdata <- BiCopSim(1000, cop)
cor(simdata)
plot(simdata,xlab='u1',ylab='u2',main = TeX('Kopula Claytona obrócona o $180^{0}.$'),cex.main=1)

set.seed(0)
cop90 <- BiCop(family = 23, par = -2.85)
C90 <- BiCopSim(1000, cop90)
cor(simdata)
plot(C90,xlab='u1',ylab='u2',main = TeX('Kopula Claytona obrócona o $90^{0}.$'),cex.main=1)

######


year <- c("Macierz korelacji", "Algorytm ARA")
course <- c(0.43, 0.92)
course1 <- c(0, -0.27)
penroll <- c(0.3, 0.25)

year[1]
# Creating Data Frame
perf <- data.frame(year, course, penroll)

# Plotting Multiple Charts and changing 
# secondary axis to percentage
library(ggplot2)


ggp <- ggplot(data=perf)+ 
  scale_y_continuous(breaks=c(-0.3,0,0.35,0.41,0.44,0.95))+
  geom_bar(aes(x=year, y=course),stat="identity", fill="steelblue")+
  geom_text(aes(x=year,y=course,label=course), position=position_dodge(width=0.6), 
            vjust=1,color='white')+
  geom_bar(aes(x=year, y=course1),stat="identity", fill="steelblue")+
  geom_text(aes(x=year[1],y=course1[1],label=course1[1]), position=position_dodge(width=0.6), 
            vjust=-0.23,color='white')+
  geom_text(aes(x=year[2],y=course1[2],label=course1[2]), position=position_dodge(width=0.6), 
            vjust=-0.23,color='white')+
  geom_hline(aes(yintercept=0.44,colour="D-vine"),size=1,show.legend = TRUE)+
  geom_hline(aes(yintercept=0.41,colour="C-vine"),size=1,show.legend = TRUE)+
  geom_hline(aes(yintercept=0.35,colour="FS"),size=1,show.legend = TRUE)+
  scale_color_manual(name = "Struktura zależności",
                     values = c("D-vine" = "orange","C-vine"='green',"FS"="red"))+
  ggtitle("")+
  
  
  labs( x="",y="Wartość efektu dywersyfikacji")+
  theme_minimal()
ggp
