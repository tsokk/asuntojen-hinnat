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
pdata <- transform(pdata, location=replace(location, location == "Kallio, harju", "Harju"))
pdata <- transform(pdata, location=replace(location, location == "Pikku-huopalahti", "Pikku-Huopalahti"))
pdata <- transform(pdata, location=replace(location, location == "Pikku huopalahti", "Pikku-Huopalahti"))
pdata <- transform(pdata, location=replace(location, location == "Vanha-vuosaari", "Vuosaari"))

# poistetaan asunnot joiden sijaintia ei tunneta
pdata <- pdata[which(pdata$location != ""),]
pdata <- pdata[which(pdata$location != "Helsinki"),]

fit.0 <- lm(price.1000 ~ factor(location) +
    area + 
    factor(housetype),
    data = pdata)

b <- coef(fit.0)

# 55-neliöinen kerrostalo Harjussa
b[["(Intercept)"]] + b[["factor(location)Harju"]] + 55*b[["area"]]

# on likimain saman hintainen, kuin 100-neliöinen omakotitalo Pakilassa
b[["(Intercept)"]] + b[["factor(location)Pakila"]] + 100*b[["area"]] + b[["factor(housetype)ok"]]

# sama käyttäen predict-funtiota
x.harju = data.frame(location = "Harju", area = 55, housetype = "kt")
predict(fit.0, x.harju)
x.pakila = data.frame(location = "Pakila", area = 100, housetype = "ok")
predict(fit.0, x.pakila)

# estimaatit ja niiden 95 % luottamusvälit
predict(fit.0, x.harju, interval = "prediction", level = 0.95)
predict(fit.0, x.pakila, interval="prediction", level = 0.95)

# asuntokauppojen lukumäärän ennustaminen
location.counts <- aggregate(pdata$price.1000,
    list(Sijainti = pdata$location, Talotyyppi = pdata$housetype),
    function(x) length(x))

location.price.median <- aggregate(pdata$price.1000,
    list(Sijainti = pdata$location, Talotyyppi = pdata$housetype),
    median)

location.area.median <- aggregate(pdata$area,
    list(Sijainti = pdata$location, Talotyyppi = pdata$housetype),
    median)

cdata <- location.counts
cdata$y <- location.price.median$x
cdata$z <- location.area.median$x

names(cdata) <- c("Sijainti", "Talotyyppi", "Lkm", "Hinta", "Koko")

require(VGAM)
fit.1 <- vglm(Lkm ~ factor(Sijainti) +
    factor(Talotyyppi) + Hinta + Koko,
    pospoisson, data = cdata)
summary(fit.1)
x.kontula <- data.frame(Sijainti = "Kontula",
    Talotyyppi = "kt", Koko = 100, "Hinta" = 200)
predict(fit.1, x.kontula)
x.kulosaari <- data.frame(Sijainti = "Kulosaari",
    Talotyyppi = "kt", Koko = 100, "Hinta" = 400)
predict(fit.1, x.kulosaari)
