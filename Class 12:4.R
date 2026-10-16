# Class 12/4

mydata = read.csv('StockMarketA_9.25.csv')
source('functions.R')

n = length(mydata$Date)
x = seq(1,n,1)
y = mydata$Price
plot(x,y,pch=16,col='darkblue',cex=0.5)
lines(x,y)

M = mymlr(x,y)  
m = M$bhat[2]
b = M$bhat[1]

abline(b,m,lwd=2,col='purple')
mean (M$res)


# linearity: mean residuals shouldnt be close to 0 -- non-linear
# homoscedasticity: residuals should be roughly same distance from line 
# normality: not met, not clustered around the line 
# independence of y based on x (stock markets will never be met here (time related))
# For stocks & MLR: CAN you use? Yes | SHOULD you use? No

#window of time 
# (y1 + y2 + y3 + y4) / 4
# 1:4 w=3
# w = n-1
#window small bad (overfitting), window large bad 


yhat_2 = mytsma(y,w=2) # FOUR LOOP for function 
lines(x, yhat_2$approx, col='red', lwd=2) # overfitting

yhat_50 = mytsma(y, w=50)
lines(x, yhat_50$approx, col='pink3', lwd=2) #underfitting

yhat_10 = mytsma(y, w=10)  # w is window size (only optomizing perameter)
lines(x, yhat_10$approx, col='green', lwd=2) #maybe too overfitting so make 'w' bigger 

Z = scale(x) # trying to fix multicolinearity 
m5 = mymlr(cbind(x, x^2, x^3, x^4, x^5), y)
lines(x, M5$Yhat, col='turquoise', lwd=2)

m7 = mymlr(cbind(x, x^2, x^3, x^4, x^5, x^6, x^7), y)
lines(x, M7$Yhat, col='dodgerblue', lwd=2)

m11 = mymlr(cbind(x, x^2, x^3, x^4, x^5, x^6, x^7, x^8, x^9, x^10,x^11), y)
lines(x, m11$Yhat, col='orange', lwd=2) # p is degree of polynomial (only optomizing perameter)

# polynomial regression -- bad for extrapulating bc goes down & up to -inf and +inf (good for short-term)
# aggegration of models so you can average both the poly and tsma to get the best approx (bc they are similar in short-term)

M_w10exp = mytsma(x, w=10, type='exp', a=0.85) #higher alpha kills off things faster 
lines(x, M_w10exp$approx, col='red', lwd=2) # weights the most recent things more, so should be better fit
# arema flatelines --> garch is good for time series 







