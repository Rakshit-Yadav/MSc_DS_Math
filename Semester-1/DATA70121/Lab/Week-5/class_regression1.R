##### DATA70121: Statistics and Machine Learning 1
##### Department of Social Statistics, University of Manchester
##### Lecture 5: Regression 1
##### Lecturer: Diego Perez Ruiz (diego.perezruiz@manchester.ac.uk)
##### Teacher Assistants: Wei Zhuang (wei.zhuang-2@postgrad.manchester.ac.uk) 
##### Teacher Assistants: Shing Yan Kwong <shingyan.kwong@postgrad.manchester.ac.uk>


#babies data-----
babies.data <- read.table("babies.data.txt", header = TRUE) # make sure you are in the correct directory
head(babies.data)
attach(babies.data)
dim(babies.data)

fit0 <- lm(birth.weight ~ 1)
fit1 <- lm(birth.weight ~ mom.weight)
summary(fit1)

plot(mom.weight,birth.weight,  type = 'p', col = 'black', pch=16, 
      main = "birth weight predicted by mom's weight", 
     xlab = "mom's weight", ylab = "birth weight")
abline(fit1, col = 'red')


#correlation
cor(babies.data, birth.weight)

fit2 <- lm(birth.weight ~ gestation)
summary(fit2)

plot(gestation, birth.weight)
abline(fit2, lty = 2, col = "red")

identify(gestation, birth.weight)

trim.data <- babies.data[c(-239, -820), ]
detach(babies.data)
attach(trim.data)
dim(trim.data)

fit3 <- lm(birth.weight ~ gestation + mom.smokes)
plot(predict(fit3), residuals(fit3))

par(mfrow = c(2,2))
plot(fit3)
dev.off()

#spam filter-----
load("email.Rdata") # make sure you are in the correct directory
attach(email)
dim(email)
head(email)

#fit glm
model1 = glm(spam ~  to_multiple, data = email, family = binomial)
summary(model1)
exp(coef(model1))

#fit complex model
model2 = glm(spam ~ ., data = email, family = binomial)
summary(model2)

#plot glm
set.seed(1)
jitter = rnorm(nrow(email), sd = 0.01)
plot(model2$fitted.values, spam + jitter, xlim = 0:1, ylim = c(-0.5, 1.5), axes =
       FALSE,
     xlab = "Predicted probability", ylab = "", col = adjustcolor("blue", 0.2), pch = 16)
axis(1)
axis(2, at = c(0, 1), labels = c("0 (not spam)", "1 (spam)"))
plot(factor(spam, labels = c("not spam", "spam")), model2$fitted.values)


