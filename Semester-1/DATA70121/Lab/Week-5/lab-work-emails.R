load("email.Rdata")
attach(email)
dim(email)
head(email)
View(email)

# fitting logistic regression model (model1) between "spam" and "to_multiple"
model1 <- glm(formula = spam ~ to_multiple, family = binomial , data = email)
summary(model1)

model2 <- glm(formula = spam ~ ., family = binomial, data = email)
summary(model2)
