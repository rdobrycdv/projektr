library("readxl")
library("ggplot2")
library("tidyr")
library("dplyr")
library("gridExtra")
library("xlsx")
library("zoo")
library("plotly")
library("corrplot")


dane <- readxl::read_xlsx("excel.xlsx", sheet=2)
dane<-dane[,-9] #usuniecie kolumny atrybutow ktora ma same "NA"
dane$Wartosc <- as.numeric(dane$Wartosc) #zmiana koluny wartosci na numeryczna
dane <- na.omit(dane) #usuniecie wierszy w ktorych nie ma zadnej wartosci
dane$Rok <- as.numeric(dane$Rok)
regresja <- lm(dane$Wartosc~dane$Rok) #czy data ma wplyw na wartosci
print(regresja)              #regrejsa wynosi 0,7209

rok6 <- filter(dane, Rok == 2006)
rok7 <- filter(dane, Rok == 2007)
rok8 <- filter(dane, Rok == 2008)
rok9 <- filter(dane, Rok == 2009)
rok10 <- filter(dane, Rok == 2010)
rok11 <- filter(dane, Rok == 2011)
rok12 <- filter(dane, Rok == 2012)
rok13 <- filter(dane, Rok == 2013)
rok14 <- filter(dane, Rok == 2014)
rok15 <- filter(dane, Rok == 2015)
rok16 <- filter(dane, Rok == 2016)
rok17 <- filter(dane, Rok == 2017)
rok18 <- filter(dane, Rok == 2018)
rok19 <- filter(dane, Rok == 2019)

mea06 <- mean(rok6$Wartosc)
mea07 <- mean(rok7$Wartosc)
mea08 <- mean(rok8$Wartosc)
mea09 <- mean(rok9$Wartosc)
mea10 <- mean(rok10$Wartosc)
mea11 <- mean(rok11$Wartosc)
mea12 <- mean(rok12$Wartosc)
mea13 <- mean(rok13$Wartosc)
mea14 <- mean(rok14$Wartosc)
mea15 <- mean(rok15$Wartosc)
mea16 <- mean(rok16$Wartosc)
mea17 <- mean(rok17$Wartosc)
mea18 <- mean(rok18$Wartosc)
mea19 <- mean(rok19$Wartosc)

#srednia cena calego koszyka dla kazdego roku
srednia <-c(mea06, mea07,mea08,mea09,mea10,mea11,mea12,mea13,mea14,mea15,mea16,mea17,mea18,mea19)
daty<- c(2006:2019)
plot(daty,srednia)

plot(rok6$Wartosc,rok7$Wartosc, rok8$Wartosc,rok9$Wartosc,rok10$Wartosc,rok11$Wartosc,rok12$Wartosc,rok13$Wartosc,rok14$Wartosc,rok15$Wartosc,rok16$Wartosc,rok17$Wartosc,rok18$Wartosc,rok19$Wartosc)
sredniacena<- data.frame("Rok" = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), "srednia" = c(mea06, mea07,mea08,mea09,mea10,mea11,mea12,mea13,mea14,mea15,mea16,mea17,mea18,mea19))

png("callosc.png")
plot(sredniacena, type="b", main="Zmiana średnich cen koszyka w latach 2006-2019")
dev.off()
#kolejno wydzielanie wojewodzt przez zmienianie parametru nazwa oraz nazwy tablic
x<-dane %>%
  filter(Nazwa == "ZACHODNIOPOMORSKIE") %>%
  select(Rok, Wartosc)

y06 <- filter(x, Rok == 2006)
y07 <- filter(x, Rok == 2007)
y08 <- filter(x, Rok == 2008)
y09 <- filter(x, Rok == 2009)
y10 <- filter(x, Rok == 2010)
y11 <- filter(x, Rok == 2011)
y12 <- filter(x, Rok == 2012)
y13 <- filter(x, Rok == 2013)
y14 <- filter(x, Rok == 2014)
y15 <- filter(x, Rok == 2015)
y16 <- filter(x, Rok == 2016)
y17 <- filter(x, Rok == 2017)
y18 <- filter(x, Rok == 2018)
y19 <- filter(x, Rok == 2019)

z06 <- mean(y06$Wartosc)
z07 <- mean(y07$Wartosc)
z08 <- mean(y08$Wartosc)
z09 <- mean(y09$Wartosc)
z10 <- mean(y10$Wartosc)
z11 <- mean(y11$Wartosc)
z12 <- mean(y12$Wartosc)
z13 <- mean(y13$Wartosc)
z14 <- mean(y14$Wartosc)
z15 <- mean(y15$Wartosc)
z16 <- mean(y16$Wartosc)
z17 <- mean(y17$Wartosc)
z18 <- mean(y18$Wartosc)
z19 <- mean(y19$Wartosc)

ZACHODNIOPOMORSKIE<- data.frame("Rok" = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), "srednia" = c(z06,z07,z08,z09,z10,z11,z12,z13,z14,z15,z16,z17,z18,z19))

png("DS.png")
plot(DOLNOSLASKIE,type="b", main="Dolnośląskie")
dev.off()

png("KP.png")
plot(KUJPOM,type="b", main="Kujawsko-Pomorksie")
dev.off()

png("lubel.png")
plot(LUBELSKIE,type="b", main="Lubelskie")
dev.off()

png("lubu.png")
plot(LUBUSKIE,type="b", main="Lubuskie")
dev.off()

png("lodz.png")
plot(LODZKIE,type="b", main="Łódzkie")
dev.off()

png("malop.png")
plot(MALOPOLSKIE,type="b", main="Małopolskie")
dev.off()

png("mazow.png")
plot(MAZOWIECKIE,type="b", main="Mazowieckie")
dev.off()

png("opol.png")
plot(OPOLSKIE,type="b", main="Opolskie")
dev.off()

png("podk.png")
plot(PODKARPACKIE,type="b", main="Podkarpackie")
dev.off()

png("podl.png")
plot(PODLASKIE,type="b", main="Podlaskie")
dev.off()

png("pom.png")
plot(POMORSKIE,type="b", main="Pomorskie")
dev.off()

png("slas.png")
plot(SLASKIE,type="b", main="Śląskie")
dev.off()

png("swie.png")
plot(SWIETOKRZYSKIE,type="b", main="Świętokrzyskie")
dev.off()

png("wielko.png")
plot(WIELKOPOLSKIE,type="b", main="Wielkopolskie")
dev.off()

png("zacho.png")
plot(ZACHODNIOPOMORSKIE,type="b", main="Zachodniopomorskie")
dev.off()
#laczenie wyniikow dla bardziej przejzystej przentacji dancyh
skujpom <- KUJPOM[,-1]
slubuskie <- LUBUSKIE[,-1]
slubelskie <-LUBELSKIE[,-1]
slodzkie <-LODZKIE[,-1]
smalopolskie <-MALOPOLSKIE[,-1]
smazowieckie <-MAZOWIECKIE[,-1]
sopolskie <-OPOLSKIE[,-1]
spodkarpackie <-PODKARPACKIE[,-1]
spodlaskie <-PODLASKIE[,-1]
spomorskie<-POMORSKIE[,-1]
sslaskie<-SLASKIE[,-1]
sswietokrzyskie<-SWIETOKRZYSKIE[,-1]
swielkopolskie<-WIELKOPOLSKIE[,-1]
szachpomorskie<-ZACHODNIOPOMORSKIE[,-1]
sdolnoslaskie<-DOLNOSLASKIE[,-1]
lataa<-DOLNOSLASKIE[,-2]
tabelawojewodztw <- cbind(lataa, sdolnoslaskie , skujpom, slubuskie, slubelskie,slodzkie,sopolskie, smalopolskie,smazowieckie,sopolskie,spodkarpackie,spodlaskie,spomorskie,sslaskie,sswietokrzyskie,swielkopolskie,szachpomorskie)
#znalezenie maksimow i maksimow dla danych lat
tabelawojewodztw <- format(tabelawojewodztw, digits=3)
print(tabelawojewodztw)
write.xlsx(tabelawojewodztw) 
#png("wojewodztwakr.png", height=300, width=1322)
#p<-tableGrob(tabelawojewodztw)
#grid.arrange(p)
#dev.off()

options(repr.plot.width=16, repr.plot.height=10)
ts.plot(tabelawojewodztw, col=palette, xlab='Czas', ylab='Cena[zł]',main='Cena jaj za szt w latach 2006-2019')
legend("topleft", colnames(tabelawojewodztw), col=palette, lty=1, cex=.6)


plot_ly(x=lataa)%>%
  add_lines(y=sslaskie, color=I("red"),name="Śląsk")%>%
  add_lines(y=sswietokrzyskie, color=I("green"),name="Świętokrzyskie")%>%
  add_lines(y=skujpom, color=I("blue"),name="Kuj-Pom")%>%
  add_lines(y=slubuskie, color=I("yellow"),name="Lubuskie")%>%
  add_lines(y=slubelskie, color=I("orange"),name="Lubelskie")%>%
  add_lines(y=slodzkie, color=I("black"),name="Łódzkie")%>%
  add_lines(y=smalopolskie, color=I("darkorchid"),name="Małopolska")%>%
  add_lines(y=smazowieckie, color=I("deeppink"),name="Mazowsze")%>%
  add_lines(y=sopolskie, color=I("chocolate1"),name="Opolskie")%>%
  add_lines(y=spodkarpackie, color=I("skyblue"),name="Podkarpacie")%>%
  add_lines(y=spodlaskie, color=I("moccasin"),name="Podlasie")%>%
  add_lines(y=spomorskie, color=I("turquoise1"),name="Pomorze")%>%
  add_lines(y=swielkopolskie, color=I("palevioletred"),name="Wielkopolska")%>% 
  add_lines(y=szachodniopomorskie, color=I("navy"),name="ZPom")%>%
  add_lines(y=sdolnoslaskie, color=I("tan"),name="Dolnyslask")


skujpom <- format(skujpom, digits=3)
slubuskie <- format(slubuskie, digits=3)
slubelskie <-format(slubelskie, digits=3)
slodzkie <-format(slodzkie, digits=3)
smalopolskie <-format(smalopolskie, digits=3)
smazowieckie <-format(smazowieckie, digits=3)
sopolskie <-format(sopolskie, digits=3)
spodkarpackie <-format(spodkarpackie, digits=3)
spodlaskie <-format(spodlaskie, digits=3)
spomorskie<-format(spomorskie, digits=3)
sslaskie<-format(sslaskie, digits=3)
sswietokrzyskie<-format(sswietokrzyskie, digits=3)
swielkopolskie<-format(swielkopolskie, digits=3)
szachpomorskie<-format(szachodniopomorskie, digits=3)
sdolnoslaskie<-format(sdolnoslaskie, digits=3)

p <- plot_ly(
  type = 'table',
  columnwidth = c(300, 300,300,300,300,300,300,300,300,300,300,300,300,300,300),
  header = list(
    values = c("rok","Kujpom","Lubuskie","Lubelskie","Łodzkie","Małopolskie","Mazowieckie","Opolskie","Podkarpackie"),
    line = list(color = '#506784'),
    fill = list(color = 'grey'),
    align = c('left','center'),
    font = list(color = 'white', size = 12)
  ),
  cells = list(
    values = rbind(
      c(2006:2019),
      c(skujpom),
      c(slubuskie),
      c(slubelskie),
      c(slodzkie),
      c(smalopolskie),
      c(smazowieckie),
      c(sopolskie),
      c(spodkarpackie)),
    line = list(color = '#506784'),
    fill = list(color = c('#25FEFD', 'white')),
    align = c('left', 'center'),
    font = list(color = c('#506784'), size = 12)
  ))
p

r <- plot_ly(
  type = 'table',
  columnwidth = c(300, 300,300,300,300,300,300,300,300,300,300,300,300,300,300),
  header = list(
    values = c("rok","Podlaskie","Pomorskie","Świętokrzyskie","Wielkopolskie","Zachodniopomorskie","Dolnoslaskie","Śląskie"),
    line = list(color = '#506784'),
    fill = list(color = 'grey'),
    align = c('left','center'),
    font = list(color = 'white', size = 12)
  ),
  cells = list(
    values = rbind(
      c(2006:2019),
      c(spodlaskie),
      c(spomorskie),
      c(sswietokrzyskie),
      c(swielkopolskie),
      c(szachpomorskie),
      c(sdolnoslaskie),
      c(sslaskie)),
    line = list(color = '#506784'),
    fill = list(color = c('#25FEFD', 'white')),
    align = c('left', 'center'),
    font = list(color = c('#506784'), size = 12)
  ))
r


placamin <- c(899.10,936,1126,1276,1317,1386,1500,1600,1680,1750,1850,2000,2100,2250)
plot(placamin)

korelacja <- plot_ly(x=srednia)%>%
  add_lines(y=placamin, color=I("red"),name="Śląsk")
  add_lines(y=placamin, color=("red"),name="Śląsk")
korelacja
korelacjatab <- cbind(lataa, srednia, placamin)
korlecjadotab<-cor(korelacjatab, method = "pearson", use = "complete.obs") #korelacja jest bardzo wysokoa okolo 0,98
corrplot(korlecjadotab, method="number")
cor.test(srednia,placamin,method="pearson")


