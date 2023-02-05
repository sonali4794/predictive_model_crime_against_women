#this part of the code creates a plot of total crime cases against women in india over last 10 years
crime_state = read_csv(here("data/year wise crime.csv"))
crime_state$Year <- as.integer(crime_state$Year)

#plot a line graph of crime cases across years
ggplot(crime_state) +
  geom_line(aes(x=Year, y=Crime_rate))+
  labs(title = "Trend of crime cases against women over last 10 years",
       xlab = "Year",
       ylab = "Total cases of violence against women") +
  scale_x_continuous(breaks = 2011:2020)
ggsave(here("figures/total_crime_cases.png"), width=8, height=4.5)




