# Script por Madeleine Santiago 10.05.2021
# Script en R para el Proyecto Parte 1. del Modulo 6. Análisis Multivariado del
# Dip en Estadistica Aplicada ITAM 2020-2021
# ----------
rm(list = ls())
library(tidyverse)
library(wesanderson)
library(reshape)
library(corrplot)
require(reshape2)
library(ggplot2)
library("ggplot2")
library("ggsci")
library(GGally)
library(factoextra)


happy <- read.csv("Proyecto/Happiness_Index.csv",header=TRUE,
                   row.names=1,stringsAsFactors = TRUE)
View(happy)
dim(happy)
summary.h<-summary(happy)
view(summary.h)

cuant_happy<-data.matrix(happy[,2:7])
dim(cuant_happy)
happy_cuant<-happy[,2:7]


#centro de gravedad
sapply(happy_cuant,mean)
#medianas de cada variable
sapply(happy_cuant,median)
#covarianzas
sd(cuant_happy[,1])
cov(cuant_happy)
var(cuant_happy[,2])
cor(happy_cuant)

#Estandarizar datos
happy_STD<-scale(happy_cuant,center=TRUE,scale=TRUE)
cov(happy_STD)
#los paises con valores negativos es que estan debajo de la media
#los paises con valores positivos estan sobre la media
#Boxplot
meltData <- melt(happy_STD) #Melt data
meltData
#plot Boxplot
p <- ggplot(meltData, aes(factor(variable), value)) +
        geom_boxplot(aes(fill=variable)) +
        scale_fill_grey(start = 0.75, end = 0.25) +
        ggtitle('Boxplot Variables Estandarizadas')+theme_light()+
        theme(axis.text.x = element_text(angle=90),
              legend.position = "none")
p

#Boxplot by Region
levels(happy$Regional.indicator)
unique(happy[c("Regional.indicator")])
Regions=c('South Asia','Central and Eastern Europe',
          'Middle East and North Africa',
          'Latin America and Caribbean',
          'Commonwealth of Independent States',
          'North America and ANZ','Western Europe',
          'Sub-Saharan Africa','Southeast Asia','East Asia'
          
          )
happy$Regional.indicator<-factor(happy$Regional.indicator,
                                 levels=Regions)
pal <- wes_palette("Zissou1", 10, type = "continuous")

#*************
boxplot.sep<-function(Indicator,Variable,String.Var,dataframe){
        p<-ggplot(data = dataframe, aes(x=Indicator, 
                                         y=Variable))
        p+ geom_boxplot(aes(fill=Indicator))+
                theme(axis.text.x = element_text(angle=90),
                      legend.position = "none")+
                #scale_fill_brewer(palette="jco")+
                scale_fill_jco()+
                #scale_fill_grey(start = 0.75, end = 0.25) +
                ggtitle(String.Var)
}
happy_STD<-as.data.frame(apply(happy_STD, 2, unlist))
happy_STD$Regional.indicator<-happy$Regional.indicator
colnames(happy_STD)
class(happy_STD)
happy_STD$Regional.indicator<-factor(happy_STD$Regional.indicator,
                                 levels=Regions)
happy_STD$Regional.indicator
boxplot.sep(happy_STD$Regional.indicator,
            happy_STD$Healthy.life.expectancy.at.birth,
            'Healthy.life.expectancy.at.birth',happy_STD)
boxplot.sep(happy$Regional.indicator,happy$Healthy.life.expectancy.at.birth,
            'Healthy.life.expectancy.at.birth',happy)

boxplot.sep(happy_STD$Regional.indicator,
            happy_STD$Perceptions.of.corruption,
            'Perceptions.of.corruption',happy_STD)
boxplot.sep(happy$Regional.indicator,happy$Perceptions.of.corruption,
            'Perceptions.of.corruption',happy)

#*********
#Gráfica de Disperion Ej1
par(mfrow=c(1,1))
plot(happy_STD[,1],happy_STD[,3],xlab="Log GDP",ylab="Life Expectancy",
     col=happy$Regional.indicator)
legend(x='bottomright',inset=0,title = "Regions",
       legend=Regions,col=c(1:10),
       fill = c(1:10),cex = 0.25)   
#Grafica de dispersion General
pairs(happy_cuant) #Forma 1
ggpairs(happy, columns=2:8) + 
        ggtitle("Happiness Index") #Forma 2. GGplot

#Forma 3. Colores - Grafica Correlación
M<-cor(happy_cuant)
head(round(M,2))
#dev.off()
#plot.new
#png(height=1200, width=1500, pointsize=15, file="corrplot.png")
corrplot(M, method="color",addCoef.col="grey", order = "AOE")
#dev.off()

#*********** ACP ************
happy_cuant<-happy[,2:9]
view(happy_cuant)
resultACP<-prcomp(happy_cuant, center = TRUE, scale = TRUE)
summary(resultACP)
resultACP$rotation
resultACP$x
#Scree Plot 1. With screeplot
plot.new
png(height=1200, width=1500, pointsize=15, file="./Proyecto/ScreePlot.png")
screeplot(resultACP,type="lines",main="Componentes Principales")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
dev.off()
#Plot componentes ACP$x
plot(resultACP$x)

#Screeplot 2. with GGplot
var_explained = resultACP$sdev^2 #/ sum(resultACP$sdev^2)
var_explained
r <- 1:8
qplot(r, var_explained) +  
        geom_line() + 
        xlab("PCx") + 
        ylab("Variance Explained") +
        ggtitle("Scree Plot") + 
        geom_hline(yintercept=1, linetype="dashed", color = "indianred")+
        ylim(0, 5) +theme_light()

#********* EIG WITH VAR *******
plot.new()
png(height=1200, width=1500, pointsize=15,
    file="./Proyecto/fviz_eig.png")
fviz_eig(resultACP)
dev.off()
fviz_pca_ind(resultACP,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#******** EIG WITH VAR 2*********
cumpro <- cumsum(resultACP$sdev^2 / sum(resultACP$sdev^2))
df.var<-data.frame(cumpro)
p<-ggplot(df.var, aes(x=rownames(df.var),y=`cumpro`)) +
        geom_point()+ ggtitle('Cumulative Variance Plot')+
        geom_hline(yintercept=0.7772, linetype="dashed", color = "dodgerblue4")+
        geom_vline(xintercept=3, linetype="dashed", color = "dodgerblue4")
p+labs(x = "PCx",y='Amount of Explained Variance')+theme_light()

#*********** PLOT 2D *************
while (!is.null(dev.list()))  dev.off() 

plot(resultACP$x[,1],resultACP$x[,2], 
     xlab="PC1 (48.2%)", ylab = "PC2 (19%)", main = "PC1 / PC2 - plot")
summary(resultACP)
library("factoextra")
# Plot Individuos por Region, CP1 vs CP2
p<-fviz_pca_ind(resultACP, geom.ind = "point",
            pointshape = 21,pointsize = 2, 
            fill.ind = happy$Regional.indicator, 
            col.ind = "black", 
            palette = "jco", 
            addEllipses = FALSE,
            label = "var",
            col.var = "black",
            repel = TRUE,
            legend.title = "Regional.Ind") +
        ggtitle("2D PCA-plot from Happiness Index Dataset") +
        theme(plot.title = element_text(hjust = 0.5))
p

numCol=1
dim(resultACP$x)
Region<-happy$Regional.indicator
Countries<-happy$Country.name
CP1<-resultACP$x[,numCol]/resultACP$sdev[numcol]
CP2<-resultACP$x[,numCol+1]/resultACP$sdev[numcol+1]
CP3<-resultACP$x[,numCol+2]/resultACP$sdev[numcol+2]
ACP<-data.frame(Countries,Region,CP1,CP2,CP3)

# Correlation Data (ACP)
corr1<-resultACP$rotation[,1]*resultACP$sdev[1]
corr2<-resultACP$rotation[,2]*resultACP$sdev[2]
corr3<-resultACP$rotation[,3]*resultACP$sdev[3]
corr.df<-data.frame(corr1,corr2,corr3)
view(corr.df)

#Subset - Componentes principales por Región 3D
#Latin America and Caribbean, South Asia
#North America and ANZ','Western Europe'
sub_df<-subset(ACP, ACP$Region == "Western Europe")
sub_df

p<-ggplot(sub_df, aes(x=sub_df$CP1, y=sub_df$CP2,
                      size = sub_df$CP3,label=sub_df$Countries)) +
        geom_text()+ 
        geom_hline(yintercept=0, linetype="dashed", color = "red")+
        geom_vline(xintercept=0, linetype="dashed", color = "red")

p + labs(title = "ACP Analysis", 
         subtitle = "Western Europe")+
        coord_cartesian(xlim =c(-1, 2.5), ylim = c(-1.5, 2))
p+xlim(-1, 2.5) + ylim(-1.5, 2)

fviz_pca_var(resultACP,
             col.var=corr3,
             #col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

ordered.data <- ACP[order(-CP1,-CP2,-C93),]
ordered.data$Countries[1:10]
ordered.data$Countries[116:126]

ACP$Distance=sqrt(CP1**2+CP2**2+CP3**2)
ACP$Distance
ordered.data <- ACP[order(-ACP$Distance),]
ordered.data$Countries[1:10]
ordered.data$Countries[116:126]

