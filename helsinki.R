pdata <- read.csv("helsinki-2020-09-05.csv",
    encoding = "UTF-8",
    sep = "\t",
    strip.white = TRUE)

pdata <- transform(pdata, price=as.numeric(as.character(price)))
pdata <- transform(pdata, price.1000=price/1000)
pdata <- transform(pdata, area=as.numeric(as.character(gsub(",", ".", area))))
pdata <- transform(pdata, unitprice=as.numeric(as.character(unitprice)))
pdata <- transform(pdata, year=as.numeric(as.character(year)))
pdata <- transform(pdata, lot=replace(lot, lot == "vuokra ", "vuokra"))
pdata <- transform(pdata, lot=replace(lot, lot == " ", ""))
pdata <- transform(pdata, location=replace(location, location == "harju", "Harju"))

# poistetaan asunnot joiden sijaintia ei tunneta
pdata <- pdata[which(pdata$location != ""),]

fit.0 <- lm(price.1000 ~ factor(location) +
    area + 
    factor(housetype),
    data = pdata)

b <- coef(fit.0)

# 55-neliöinen kerrostalo Harjussa
b[["(Intercept)"]] + b[["factor(location)Harju"]] + 55*b[["area"]]

# on likimain saman hintainen, kuin 100-neliöinen omakotitalo Pakilassa
b[["(Intercept)"]] + b[["factor(location)Pakila"]] + 100*b[["area"]] + b[["factor(housetype)ok"]]