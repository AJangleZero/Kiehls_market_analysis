## Kiehls analysis 

data.kiehls <- data[,c(39:45,55)]

g_kiehls1 <- ggplot(data.kiehls %>%
                      group_by(kiehls.use) %>%
                      summarise(n=n()), aes(x=kiehls.use, y=n, fill=kiehls.use)) +
  geom_col() +
  labs(x="Koriste Kiehl's", y="Broj ispitanika") +
  theme(legend.position = "none")

g_kiehls2 <- ggplot(data.kiehls %>%
                      mutate(use.kiehls=as.factor(use.kiehls)) %>%
                      group_by(use.kiehls) %>%
                      summarise(n=n()), aes(x=use.kiehls, y=n, fill=use.kiehls)) +
  geom_col() +
  labs(x="Želja za korištenjem Kiehl's e-trgovine", y="Broj ispitanika") +
  theme(legend.position = "none")

subplot(ggplotly(g_kiehls1), ggplotly(g_kiehls2),
        titleX=TRUE, titleY=TRUE, margin=c(0.04, 0.04, 0.04, 0.18)) %>%
  layout(annotations = list(
    list(x = 0 , y = 1.05, text = "Koliko ispitanika koristi Kiehl's", showarrow = F, xref='paper', yref='paper'),
    list(x = 0.8 , y = 1.05, text = "Koliko ispitanika želi koristiti Kiehl's e-trgovinu", showarrow = F, xref='paper', yref='paper'))
  )

ggplotly(ggplot(data.kiehls [complete.cases(data.kiehls),c(3,4,5)] %>%
         gather(variable, value), aes(x=variable, y=value)) +
  geom_boxplot() +
  labs(x="", y="Ocijena (1-10)"))

ggplotly(ggplot(data.frame(associate=unlist(strsplit(data.kiehls [complete.cases(data.kiehls),7], split = ";")))) +
  geom_bar(aes(associate, fill=associate)) +
  theme (legend.position = "none") +
  labs(y="Frekvencija", x="Asocijacija"))

data [,c(-1, -29)] %>%
  filter(kiehls.use=="Yes") %>%
  mutate_if(is.character, as.factor) %>%
  summary
