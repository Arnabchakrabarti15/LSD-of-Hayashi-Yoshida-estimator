cor1 = read.csv("CORR_Day1.csv", header= FALSE)
e1 = eigen(cor1)$values

cor2 = read.csv("CORR_Day2.csv", header= FALSE)
e2 = eigen(cor2)$values

cor3 = read.csv("CORR_Day3.csv", header= FALSE)
e3 = eigen(cor3)$values

cor4 = read.csv("CORR_Day4.csv", header= FALSE)
e4 = eigen(cor4)$values

par(mfrow= c(2,2))
plot(ecdf(e1),verticals=TRUE,do.points=FALSE,col.hor="red",col.vert="red",xlab="",
     lwd=2,main="CDF of eigenvalues on 22.12.2020")
plot(ecdf(e2),verticals=TRUE,do.points=FALSE,col.hor="red",col.vert="red",xlab="",
     lwd=2,main="CDF of eigenvalues on 23.12.2020")
plot(ecdf(e3),verticals=TRUE,do.points=FALSE,col.hor="red",col.vert="red",xlab="",
     lwd=2,main="CDF of eigenvalues on 24.12.2020")
plot(ecdf(e4),verticals=TRUE,do.points=FALSE,col.hor="red",col.vert="red",xlab="",
     lwd=2,main="CDF of eigenvalues on 25.12.2020")

# Scree plot
library(psych)
# Then use that correlation matrix to create the scree plot
scree(cor1, factors = FALSE)

scree(cor2, factors = FALSE)

scree(cor3, factors = FALSE)

scree(cor4, factors = FALSE)


# eigenvectors 
e11 = eigen(cor1)$vectors[,1]
barplot(abs(e11),  ylab=expression('u'[1]*' (i)'),
        main="First day", cex.lab=1)

e12 = eigen(cor1)$vectors[,2]
barplot(-1*e12,  ylab=expression('u'[2]*' (i)'),
        main="First day", cex.lab=1)

e13 = eigen(cor1)$vectors[,3]
barplot(-1*e13,  ylab=expression('u'[3]*' (i)'),
        main="First day", cex.lab=1)

e14 = eigen(cor1)$vectors[,4]
barplot(-1*e14,  ylab=expression('u'[4]*' (i)'),
        main="First day", cex.lab=1)

e15 = eigen(cor1)$vectors[,5]
barplot(-1*e15,  ylab=expression('u'[5]*' (i)'),
        main="First day", cex.lab=1)

e16 = eigen(cor1)$vectors[,6]
barplot(-1*e16,  ylab=expression('u'[6]*' (i)'),
        main="First day", cex.lab=1)

e17 = eigen(cor1)$vectors[,7]
barplot(-1*e17,  ylab=expression('u'[7]*' (i)'),
        main="First day", cex.lab=1)

e18 = eigen(cor1)$vectors[,8]
barplot(-1*e18,  ylab=expression('u'[8]*' (i)'),
        main="First day", cex.lab=1)

e19 = eigen(cor1)$vectors[,9]
barplot(-1*e19,  ylab=expression('u'[9]*' (i)'),
        main="First day", cex.lab=1)

e110 = eigen(cor1)$vectors[,10]
barplot(-1*e110,  ylab=expression('u'[10]*' (i)'),
        main="First day", cex.lab=1)

# day 2

# eigenvectors 
e11 = eigen(cor2)$vectors[,1]
barplot(abs(e11),  ylab=expression('u'[1]*' (i)'),
        main="Second day", cex.lab=1)

e12 = eigen(cor2)$vectors[,2]
barplot(-1*e12,  ylab=expression('u'[2]*' (i)'),
        main="Second day", cex.lab=1)

e13 = eigen(cor2)$vectors[,3]
barplot(-1*e13,  ylab=expression('u'[3]*' (i)'),
        main="Second day", cex.lab=1)

e14 = eigen(cor2)$vectors[,4]
barplot(-1*e14,  ylab=expression('u'[4]*' (i)'),
        main="Second day", cex.lab=1)

e15 = eigen(cor2)$vectors[,5]
barplot(-1*e15,  ylab=expression('u'[5]*' (i)'),
        main="Second day", cex.lab=1)

e16 = eigen(cor2)$vectors[,6]
barplot(-1*e16,  ylab=expression('u'[6]*' (i)'),
        main="Second day", cex.lab=1)

e17 = eigen(cor2)$vectors[,7]
barplot(-1*e17,  ylab=expression('u'[7]*' (i)'),
        main="Second day", cex.lab=1)

e18 = eigen(cor2)$vectors[,8]
barplot(-1*e18,  ylab=expression('u'[8]*' (i)'),
        main="Second day", cex.lab=1)

e19 = eigen(cor2)$vectors[,9]
barplot(-1*e19,  ylab=expression('u'[9]*' (i)'),
        main="Second day", cex.lab=1)

e110 = eigen(cor2)$vectors[,10]
barplot(-1*e110,  ylab=expression('u'[10]*' (i)'),
        main="Second day", cex.lab=1)

# day 3

# eigenvectors 
e11 = eigen(cor3)$vectors[,1]
barplot(abs(e11),  ylab=expression('u'[1]*' (i)'),
        main="Third day", cex.lab=1)

e12 = eigen(cor3)$vectors[,2]
barplot(-1*e12,  ylab=expression('u'[2]*' (i)'),
        main="Third day", cex.lab=1)

e13 = eigen(cor3)$vectors[,3]
barplot(-1*e13,  ylab=expression('u'[3]*' (i)'),
        main="Third day", cex.lab=1)

e14 = eigen(cor3)$vectors[,4]
barplot(-1*e14,  ylab=expression('u'[4]*' (i)'),
        main="Third day", cex.lab=1)

e15 = eigen(cor3)$vectors[,5]
barplot(-1*e15,  ylab=expression('u'[5]*' (i)'),
        main="Third day", cex.lab=1)

e16 = eigen(cor3)$vectors[,6]
barplot(-1*e16,  ylab=expression('u'[6]*' (i)'),
        main="Third day", cex.lab=1)

e17 = eigen(cor3)$vectors[,7]
barplot(-1*e17,  ylab=expression('u'[7]*' (i)'),
        main="Third day", cex.lab=1)

e18 = eigen(cor3)$vectors[,8]
barplot(-1*e18,  ylab=expression('u'[8]*' (i)'),
        main="Third day", cex.lab=1)

e19 = eigen(cor3)$vectors[,9]
barplot(-1*e19,  ylab=expression('u'[9]*' (i)'),
        main="Third day", cex.lab=1)

e110 = eigen(cor3)$vectors[,10]
barplot(-1*e110,  ylab=expression('u'[10]*' (i)'),
        main="Third day", cex.lab=1)

# day 4

# eigenvectors 
e11 = eigen(cor4)$vectors[,1]
barplot(abs(e11),  ylab=expression('u'[1]*' (i)'),
        main="Fourth day", cex.lab=1)

e12 = eigen(cor4)$vectors[,2]
barplot(-1*e12,  ylab=expression('u'[2]*' (i)'),
        main="Fourth day", cex.lab=1)

e13 = eigen(cor4)$vectors[,3]
barplot(-1*e13,  ylab=expression('u'[3]*' (i)'),
        main="Fourth day", cex.lab=1)

e14 = eigen(cor4)$vectors[,4]
barplot(-1*e14,  ylab=expression('u'[4]*' (i)'),
        main="Fourth day", cex.lab=1)

e15 = eigen(cor4)$vectors[,5]
barplot(-1*e15,  ylab=expression('u'[5]*' (i)'),
        main="Fourth day", cex.lab=1)

e16 = eigen(cor4)$vectors[,6]
barplot(-1*e16,  ylab=expression('u'[6]*' (i)'),
        main="Fourth day", cex.lab=1)

e17 = eigen(cor4)$vectors[,7]
barplot(-1*e17,  ylab=expression('u'[7]*' (i)'),
        main="Fourth day", cex.lab=1)

e18 = eigen(cor4)$vectors[,8]
barplot(-1*e18,  ylab=expression('u'[8]*' (i)'),
        main="Fourth day", cex.lab=1)

e19 = eigen(cor4)$vectors[,9]
barplot(-1*e19,  ylab=expression('u'[9]*' (i)'),
        main="Fourth day", cex.lab=1)

e110 = eigen(cor4)$vectors[,10]
barplot(-1*e110,  ylab=expression('u'[10]*' (i)'),
        main="Fourth day", cex.lab=1)