#  
library(dplyr)
library(ggplot2)
library(sf)
library(dplyr)
library(tigris)
library(RColorBrewer) # Per colori più chiari
library(viridis)
library(showtext)
library(tseries)
library(forecast)

#Import dataset
setwd("~/Downloads/AMAZON BUONO 1")
amazon_sales <- list.files(pattern = "\\.csv$")

Amazon_sales <- amazon_sales %>%
  lapply(read.csv) %>%  
  bind_rows()

rm(amazon_sales)


# Cambiare nome della colonna Order.Date in Date
colnames(Amazon_sales)[colnames(Amazon_sales) == "Order.Date"] <- "Date"

# Rimuove righe che sono completamente vuote o contenenti solo spazi
Amazon_sales <- Amazon_sales[!apply(Amazon_sales, 1, function(x) all(trimws(x) == "")), ]

# Converti la colonna 'Date' in formato Date
Amazon_sales$Date <- as.Date(Amazon_sales$Date, format = "%m/%d/%y %H:%M")

# Converti la colonna Quantity.Ordered in numerico, se necessario
Amazon_sales$Quantity.Ordered <- as.numeric(Amazon_sales$Quantity.Ordered)

# Supponendo che la tua tabella si chiami 'dati'
Totals <- Amazon_sales %>%
  group_by(Date) %>%
  summarise(Quantity = sum(Quantity.Ordered, na.rm = TRUE)) 

# Rimuove tutte le righe con almeno un NA
Totals <- na.omit(Totals)

# Esempio: rimuovere la riga 366
Totals <- Totals[-366, ]

# Let's check the initial trend
ggplot(Totals,aes(x=Date))+
  geom_line(aes(y=Quantity),color="red")+
  theme_minimal()+
  coord_cartesian(ylim = c(0, 1000))

#Let'do a time series
ts_amazon=as.ts(Totals$Quantity)

# Test of Dickey-Fuller aumentato (ADF)
test <- adf.test(ts_amazon)
print(test)
#How to interpretate risultate:
# 1.	Ipotesi nulla (H₀): La serie temporale non è stazionaria (ha una radice unitaria).
# 2.	Ipotesi alternativa (H₁): La serie temporale è stazionaria.
# 3.	Se il valore p-value del test è inferiore a un livello di significatività (es. 0.05), puoi rifiutare H₀ e concludere che la serie è stazionaria.
# Since the p-valueis greater than 0,05 , the time series is not stationary, so we have to make differentiation:

# Calcolo della prima differenziazione (d = 1)
ts_amazon_diff <- diff(ts_amazon, differences = 1, lag=1)

# Visualizzazione della serie differenziata
plot(ts_amazon_diff, main = "First differentiated series (d = 1)", 
     ylab = "Differentiation", xlab = "Time")

# Test di Dickey-Fuller aumentato (ADF) con lag = 1
adf_result <- adf.test(ts_amazon_diff, k = 1)  # k specifica il lag nel test ADF

# Stampa del risultato del test
print(adf_result)
#dato che il p-value è 0,01 < 0,05, la serie è stazionaria

#un grafico della funzione di autocorrelazione (ACF) per la serie temporale differenziata ts_amazon_diff
#Da mettere in appendice: ggAcf(ts_amazon_diff,lag.max = 365)+theme_bw()+ylab(label="Quantity")+labs(title="ACF - Quantity")
ggAcf(ts_amazon_diff,lag.max = 20)+theme_bw()+ylab(label="Quantity")+labs(title="ACF - Quantity")

#The PACF plot helps identify the direct correlation at each lag.
#Da mettere in appendice: ggPacf(ts_amazon_diff,lag.max = 365)+theme_bw()+ylab(label="Quantity")+labs(title="PACF - Quantity")
ggPacf(ts_amazon_diff,lag.max = 20)+theme_bw()+ylab(label="Quantity")+labs(title="PACF - Quantity")

#Vedi bene teoria su come trovare p,q per ARIMA

#Creo modelli arima
ARIMA_0_1_1 = Arima(ts_amazon, order = c(0, 1, 1))
ARIMA_0_1_2 = Arima(ts_amazon, order = c(0, 1, 2))
ARIMA_1_1_0 = Arima(ts_amazon, order = c(1, 1, 0))
ARIMA_2_1_0 = Arima(ts_amazon, order = c(2, 1, 0))
ARIMA_0_1_0 = Arima(ts_amazon, order = c(0, 1, 0))
ARIMA_1_1_1 = Arima(ts_amazon, order = c(1, 1, 1))
ARIMA_1_1_2 = Arima(ts_amazon, order = c(1, 1, 2))
ARIMA_2_1_1 = Arima(ts_amazon, order = c(2, 1, 1))


#Creo AIC e BIC
models <- list(ARIMA_0_1_1, ARIMA_0_1_2, ARIMA_1_1_0, ARIMA_2_1_0,ARIMA_0_1_0,ARIMA_1_1_1,ARIMA_1_1_2,ARIMA_2_1_1)
results <- sapply(models, function(model) {
  c(AIC = AIC(model), BIC = BIC(model))
})

results_df <- as.data.frame(t(results))
print(results_df)
#la combinazione migliore è la prima ARIMA_0_1_1

#Selezionare il miglior modello, summary arima (printa fitted values)
# Scegli il modello migliore
best_model <- ARIMA_0_1_1  
summary(best_model)

# Analisi dei residui
checkresiduals(best_model)  # Verifica l'autocorrelazione e la normalità dei residui
#Dato che il p-value è >0,05, i residui non mostrano autocorrelazione significativa e si comportano come rumore bianco
#Questo è un segnale positivo nella modellazione delle serie temporali. Significa che il modello ARIMA ha catturato bene i pattern presenti nella serie temporale (trend, stagionalità, dipendenze) e che non ci sono informazioni significative rimaste nei residui.
#Il modello è riuscito a spiegare tutta la struttura sistematica della serie temporale, lasciando solo componenti casuali nei residui.

# Forecast
forecasted <- forecast(best_model, h = 10)
autoplot(forecasted) + ggtitle("Forecast of quantity sold")

#Auto-arima per controllare che il modello fatto da noi sia corretto
# Adattare il modello ARIMA automaticamente (inserire la time series non differenziata, autoarima la differenzia automaticamente se non stazionaria)
model_autoarima <- auto.arima(ts_amazon)

# Riepilogo del modello
summary(model_autoarima)


      ### GRAFICO PER VEDERE VENDITE NEI PAESI ###

# Estrai lo stato dalla colonna "Purchase.Address"
Amazon_sales <- Amazon_sales %>%
  mutate(stato = sub(".*, ([A-Za-z]{2}) [0-9]+$", "\\1", Purchase.Address))

#Leva i valori nulli N/A
Amazon_sales <- na.omit(Amazon_sales)

# Raggruppa per stato e calcola la quantità totale venduta
tabella_per_stato <- Amazon_sales %>%
  group_by(stato) %>%
  summarise(quantità_totale = sum(Quantity.Ordered))

# Carica la mappa degli stati USA (senza filtrare)
usa_map <- states(cb = TRUE)

# Unisci i dati delle quantità con la geometria degli stati
mappa_con_dati <- usa_map %>%
  left_join(tabella_per_stato, by = c("STUSPS" = "stato"))

# Mantieni la geometria originale e calcola i centroidi separatamente
mappa_con_dati <- mappa_con_dati %>%
  mutate(centroids = st_centroid(geometry))  # Crea una nuova colonna con i centroidi

# Estrai le coordinate dei centroidi
centroid_coords <- st_coordinates(mappa_con_dati$centroids)

# Aggiungi le coordinate X e Y come colonne al dataframe
mappa_con_dati <- mappa_con_dati %>%
  bind_cols(
    data.frame(
      centroid_x = centroid_coords[, 1],  # Coordinata X
      centroid_y = centroid_coords[, 2]   # Coordinata Y
    )
  )

ggplot(mappa_con_dati) +
  geom_sf(aes(fill = quantità_totale), color = "black", lwd = 0.3) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, na.value = "lightgrey") +
  theme_minimal() +
  labs(title = "Quantity sold for each State",
       fill = "Total Quantity") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 7)
  ) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  geom_text(
    aes(x = centroid_x, y = centroid_y, label = round(quantità_totale, 1)),
    color = "red", size = 3, check_overlap = TRUE
  ) +
  coord_sf(
    xlim = c(-125, -60),  # Limiti di longitudine (da ovest a est degli Stati Uniti)
    ylim = c(25, 50)      # Limiti di latitudine (da sud a nord degli Stati Uniti)
  )+
  guides(
    fill = guide_colorbar(
      title.position = "top",      # Posiziona il titolo sopra la legenda
      title.hjust = 0.5,           # Centra il titolo della legenda
      barwidth = 20,               # Allunga la barra della legenda orizzontalmente
      barheight = 1               # Mantieni la barra della legenda sottile
    )
  )



      ### GRAFICO PER VEDERE PRODOTTO CHE HA PORTATO A MAGGIOR RICAVO ###

# Creo tabella nuova che filtra quella originale levando valori N/A
Amazon_sales_clean <- Amazon_sales[!(is.na(Amazon_sales$Price.Each) | Amazon_sales$Price.Each == 0 | Amazon_sales$Price.Each == "" |
                                       is.na(Amazon_sales$Quantity.Ordered) | Amazon_sales$Quantity.Ordered == 0 | Amazon_sales$Quantity.Ordered == ""), ]
#Elimina colonne che non servono
Amazon_sales_clean<-Amazon_sales_clean%>%select(-c( Order.ID, Date, Purchase.Address))

# Converti in numerico
Amazon_sales_clean$Quantity.Ordered <- as.numeric(Amazon_sales_clean$Quantity.Ordered)
Amazon_sales_clean$Price.Each <- as.numeric(Amazon_sales_clean$Price.Each)

# Crea colonna Revenue
Amazon_sales_clean$Revenue <- Amazon_sales_clean$Quantity.Ordered * Amazon_sales_clean$Price.Each
Amazon_sales_clean <- na.omit(Amazon_sales_clean)

print(Amazon_sales_clean)

#Raggruppa tutti i prodotti e trova per ogni porodotto il ricavo
Amazon_sales1<- Amazon_sales_clean %>%
  group_by(Product) %>%
  summarise(Total_Revenue = sum(Revenue, na.rm = TRUE))

print(Amazon_sales)
Amazon_sales1<- as.data.frame(Amazon_sales1)

# Rimuovi le righe con NA
Amazon_sales1 <- na.omit(Amazon_sales1)
# Conversione delle colonne
Amazon_sales1$Product <- as.factor(Amazon_sales1$Product)
Amazon_sales1$Total_Revenue <- as.numeric(Amazon_sales1$Total_Revenue)

# Supponiamo che Amazon_sales1 sia il tuo data frame con le colonne 'Product' e 'Total_Revenue'
ggplot(Amazon_sales1, aes(x = Total_Revenue, y = reorder(Product, Total_Revenue))) +
  geom_bar(stat = "identity", fill = "lightgreen") +  # Stat = identity perché usiamo un valore numerico
  geom_text(aes(label = scales::comma(Total_Revenue)), 
            hjust = 0.5,  # Centra orizzontalmente
            vjust = 0.5,  # Centra verticalmente dentro la barra
            color = "black") +  # Colore del testo
  labs(title = "Revenue per Product", x = "Total Revenue", y = "Product") +
  scale_x_continuous(labels = scales::comma_format()) +  # Rimuove la notazione scientifica
  theme_minimal()


         ### GRAFICO PREVISIONE PER OGNI STATO ###

Amazon_sales <- Amazon_sales %>%
  mutate(stato = sub(".*, ([A-Za-z]{2}) [0-9]+$", "\\1", Purchase.Address))

# Leva i valori nulli N/A
Amazon_sales <- na.omit(Amazon_sales)

# Raggruppiamo i dati per stato e data per calcolare le quantità totali per ogni giorno e paese
Amazon_sales_clean <- Amazon_sales %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y %H:%M")) %>%
  group_by(Date, stato) %>%
  summarise(Quantity = sum(Quantity.Ordered, na.rm = TRUE)) %>%
  ungroup()

# Rimuoviamo le righe con NA
Amazon_sales_clean <- na.omit(Amazon_sales_clean)
# Filtra solo le righe dove lo stato è "CA"
dati_CA <- Amazon_sales_clean %>%
  filter(stato == "CA")

# Rimuovere la colonna "stato"
dati_CA <- dati_CA %>%
  select(-stato)
dati_CA <- dati_CA[-366, ]
ts_amazon_CA=as.ts(dati_CA$Quantity)

# Adatta il modello ARIMA automatico
arima_model_CA<- auto.arima(ts_amazon_CA)


# Previsione per i prossimi 14 giorni
forecasted_values_CA <- forecast(arima_model_CA, h = 10)

# Filtra solo le righe dove lo stato è "WA"
dati_WA <- Amazon_sales_clean %>%
  filter(stato == "WA")
dati_WA <- dati_WA[-366, ]

# Rimuovere la colonna "stato"
dati_WA <- dati_WA %>% 
  select(-stato)

ts_amazon_WA = as.ts(dati_WA$Quantity)

# Adatta il modello ARIMA automatico
arima_model_WA <- auto.arima(ts_amazon_WA)

# Previsione per i prossimi 14 giorni
forecasted_values_WA <- forecast(arima_model_WA, h = 10)


# Filtra solo le righe dove lo stato è "GA"
dati_GA <- Amazon_sales_clean %>%
  filter(stato == "GA")
dati_GA <- dati_GA[-366, ]

# Rimuovere la colonna "stato"
dati_GA <- dati_GA %>% 
  select(-stato)

ts_amazon_GA = as.ts(dati_GA$Quantity)

# Adatta il modello ARIMA automatico
arima_model_GA <- auto.arima(ts_amazon_GA)

# Previsione per i prossimi 14 giorni
forecasted_values_GA <- forecast(arima_model_GA, h = 10)


# Filtra solo le righe dove lo stato è "MA"
dati_MA <- Amazon_sales_clean %>%
  filter(stato == "MA")
dati_MA <- dati_MA[-366, ]

# Rimuovere la colonna "stato"
dati_MA <- dati_MA %>% 
  select(-stato)

ts_amazon_MA = as.ts(dati_MA$Quantity)

# Adatta il modello ARIMA automatico
arima_model_MA <- auto.arima(ts_amazon_MA)

# Previsione per i prossimi 14 giorni
forecasted_values_MA <- forecast(arima_model_MA, h = 10)


# Filtra solo le righe dove lo stato è "ME"
dati_ME <- Amazon_sales_clean %>%
  filter(stato == "ME")
dati_ME <- dati_ME[-366, ]

# Rimuovere la colonna "stato"
dati_ME <- dati_ME %>% 
  select(-stato)

ts_amazon_ME = as.ts(dati_ME$Quantity)

# Adatta il modello ARIMA automatico
arima_model_ME <- auto.arima(ts_amazon_ME)

# Previsione per i prossimi 14 giorni
forecasted_values_ME <- forecast(arima_model_ME, h = 10)


# Filtra solo le righe dove lo stato è "NY"
dati_NY <- Amazon_sales_clean %>%
  filter(stato == "NY")
dati_NY <- dati_NY[-366, ]

# Rimuovere la colonna "stato"
dati_NY <- dati_NY %>% 
  select(-stato)

ts_amazon_NY = as.ts(dati_NY$Quantity)

# Adatta il modello ARIMA automatico
arima_model_NY <- auto.arima(ts_amazon_NY)

# Previsione per i prossimi 14 giorni
forecasted_values_NY <- forecast(arima_model_NY, h = 10)


# Filtra solo le righe dove lo stato è "OR"
dati_OR <- Amazon_sales_clean %>%
  filter(stato == "OR")
dati_OR <- dati_OR[-366, ]

# Rimuovere la colonna "stato"
dati_OR <- dati_OR %>% 
  select(-stato)

ts_amazon_OR = as.ts(dati_OR$Quantity)

# Adatta il modello ARIMA automatico
arima_model_OR <- auto.arima(ts_amazon_OR)

# Previsione per i prossimi 14 giorni
forecasted_values_OR <- forecast(arima_model_OR, h = 10)


# Filtra solo le righe dove lo stato è "TX"
dati_TX <- Amazon_sales_clean %>%
  filter(stato == "TX")
dati_TX <- dati_TX[-366, ]

# Rimuovere la colonna "stato"
dati_TX <- dati_TX %>% 
  select(-stato)

ts_amazon_TX = as.ts(dati_TX$Quantity)

# Adatta il modello ARIMA automatico
arima_model_TX <- auto.arima(ts_amazon_TX)

# Previsione per i prossimi 14 giorni
forecasted_values_TX <- forecast(arima_model_TX, h = 10)

plot(forecasted_values_TX, main = "Previsioni ARIMA per la Serie Temporale", xlab = "Tempo", ylab = "Quantità")

# Supponiamo che tu abbia una lista dei dati storici per ogni stato
# Ad esempio, `dati_TX`, `dati_NY`, ecc. sono le serie storiche per ogni stato

# Primo grafico: serie storica di TX
plot(
  ts_amazon_TX,  # Serie storica per TX
  main = "FORECAST FOR EACH STATE",
  xlab = "Time",
  ylab = "Quantity",
  col = "blue",  # Colore per TX
  type = "l",    # Tipo di grafico a linee
  xlim = c(200, max(time(ts_amazon_TX) + 10)),  # Impostiamo l'asse temporale
  ylim = c(0, 500)  # Impostiamo i limiti per l'asse Y (da 0 a 400)
)

# Aggiungi la previsione di TX
lines(forecasted_values_TX$mean, col = "blue", lwd = 2, lty = 2)  # Previsione di TX (linea tratteggiata)

# Aggiungi la serie storica per NY
lines(ts_amazon_NY, col = "red", lwd = 2)  # Serie storica di NY
lines(forecasted_values_NY$mean, col = "red", lwd = 2, lty = 2)  # Previsione di NY (linea tratteggiata)

# Aggiungi la serie storica per MA
lines(ts_amazon_MA, col = "green", lwd = 2)  # Serie storica di MA
lines(forecasted_values_MA$mean, col = "green", lwd = 2, lty = 2)  # Previsione di MA (linea tratteggiata)

# Aggiungi la serie storica per GA
lines(ts_amazon_GA, col = "purple", lwd = 2)  # Serie storica di GA
lines(forecasted_values_GA$mean, col = "purple", lwd = 2, lty = 2)  # Previsione di GA (linea tratteggiata)

# Aggiungi la serie storica per OR
lines(ts_amazon_OR, col = "yellow", lwd = 2)
lines(forecasted_values_OR$mean, col = "yellow", lwd = 2, lty = 2)

# Aggiungi la serie storica per ME
lines(ts_amazon_ME, col = "pink", lwd = 2)  # Serie storica di ME
lines(forecasted_values_ME$mean, col = "pink", lwd = 2, lty = 2)

# Aggiungi la serie storica per CA
lines(ts_amazon_CA, col = "orange", lwd = 2)  # Serie storica di CA
lines(forecasted_values_CA$mean, col = "orange", lwd = 2, lty = 2)

# Aggiungi la serie storica per WA
lines(ts_amazon_WA, col = "brown", lwd = 2)  # Serie storica di WA
lines(forecasted_values_WA$mean, col = "brown", lwd = 2, lty = 2)


# Aggiungi la legenda per distinguere gli stati
legend(
  "topleft", 
  legend = c("TX", "NY", "MA", "GA", "OR", "ME", "CA", "WA"), 
  col = c("blue", "red", "green", "purple", "yellow", "pink", "orange", "brown"), 
  lty = c(1, 1, 1, 1, 1, 1, 1, 1), 
  lwd = 2, 
  bty = "n"
)
