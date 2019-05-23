## cleaning counties variable
data$person.country <- tolower(data$person.country)
data$person.country <- trimws(data$person.country)
data$person.country <- gsub("us", "usa", data$person.country)
data$person.country <- gsub("usaa", "usa", data$person.country)
data$person.country <- gsub("united states", "usa", data$person.country)
data$person.country <- gsub("unitef states", "usa", data$person.country)
data$person.country <- gsub("uunited states", "usa", data$person.country)
data$person.country <- gsub("uusa", "usa", data$person.country)
data$person.country <- gsub("u.s", "usa", data$person.country)
data$person.country <- gsub("unites states", "usa", data$person.country)
data$person.country <- gsub("united kingdom", "uk", data$person.country)
data$person.country <- gsub("engladn", "uk", data$person.country)
data$person.country <- gsub("england", "uk", data$person.country)
data$person.country <- gsub("wales", "uk", data$person.country)
data$person.country <- gsub("ausatralia", "australia", data$person.country)
data$person.country <- gsub("ausatralia ", "australia", data$person.country)
data$person.country <- gsub("rusaia", "russia", data$person.country)
data$person.country <- gsub("polish", "poland", data$person.country)
data$person.country <- gsub("cyprusa", "cyprus", data$person.country)
data$person.country <- gsub("ausatria", "austria", data$person.country)
data$person.country <- gsub("soutu africa", "south africa", data$person.country)

unique(data$person.country)


## Leaflet
sPDF <- joinCountryData2Map(data,
                            joinCode = "NAME",
                            nameJoinColumn = "person.country", verbose = TRUE)
existing_countries <- subset(sPDF, !is.na(person.gender))
num_country <- data %>%
  group_by(person.country) %>%
  summarise(n=n()) 
num_country <- num_country[-41,]
num_country <- num_country[order(match(num_country$person.country, existing_countries$person.country)), ]
factpal <- colorFactor(brewer.pal(n = 11, name ="Spectral") , existing_countries$person.country)
labels <- paste0("<strong>", toupper(existing_countries$person.country), "</strong><br/>", 
                 format(num_country$n, digits = 0, big.mark = ".", decimal.mark = ",", scientific = FALSE),
                 " Ispitanika") %>% lapply(htmltools::HTML)
leaflet(existing_countries) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(
    fillColor = ~factpal(person.country),
    stroke = FALSE,
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>%
  setView(25,20,2)


## Summary data
data1 <- data
data1$person.income <- factor(data1$person.income, 
                           levels=c("<$500", "$500-$799", "$800-$1000", "$1000-$1499", "$1500-$3000", ">$3000"), 
                           ordered=TRUE)
data1$person.age <- factor(data1$person.age, levels=c("<20", "20-29", "30-39", "40-49", "50-59", ">60"), 
                              ordered=TRUE)

g_summary1 <- ggplot(data1 %>%
                       group_by(person.gender) %>%
                       summarize(n=n())) +
  geom_col(aes(x=person.gender, y=n, fill=person.gender)) +
  labs(x="Spol", y="Broj ispitanika") +
  theme(legend.position="none")

g_summary2 <- ggplot(data1 %>%
                       group_by(person.age) %>%
                       summarize(n=n())) +
  geom_col(aes(x=person.age, y=n, fill=person.age)) +
  labs(x="Godine", y="Broj ispitanika") +
  theme(legend.position="none")

g_summary3 <- ggplot(data1 %>%
                       group_by(person.income) %>%
                       summarize(n=n())) +
  geom_col(aes(x=person.income, y=n, fill=person.income)) +
  labs(x="Mjesečni prihod", y="Broj ispitanika") +
  theme(legend.position="none")

g_summary4 <- ggplot(data1 %>%
                       group_by(behave.skincare.use) %>%
                       summarize(n=n())) +
  geom_col(aes(x=behave.skincare.use, y=n, fill=behave.skincare.use)) +
  labs(x="Korištenje skincare proizvoda", y="Broj ispitanika") +
  theme(legend.position="none")

subplot(ggplotly(g_summary1), ggplotly(g_summary2), ggplotly(g_summary3), ggplotly(g_summary4),nrows=2,
        titleX=TRUE, titleY=TRUE, margin=c(0.04, 0.04, 0.04, 0.18)) %>%
  layout(annotations = list(
    list(x = 0 , y = 1.05, text = "Distribucija ispitanika po spolu", showarrow = F, xref='paper', yref='paper'),
    list(x = 0.8 , y = 1.05, text = "Distribucija ispitanika po dobi", showarrow = F, xref='paper', yref='paper'),
    list(x = 0 , y = 0.48, text = "Distribucija ispitanika po mjesečnom prihodu", showarrow = F, xref='paper', yref='paper'),
    list(x = 0.97 , y = 0.48, text = "Distribucija prema korištenju skincare proizvoda", showarrow = F, xref='paper', yref='paper'))
  )
