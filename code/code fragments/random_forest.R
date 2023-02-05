crime_against_women = crime = read_csv(here("data/crime against women raw data.csv"))

crime_combined = data.frame(crime_against_women,
                            pc1$x)
#create the test and train splits
train_frac = 0.8
N = nrow(crime_combined)
N_train = floor(train_frac*N)
N_test = N - N_train
train_ind = sample.int(N, N_train, replace=FALSE) %>% sort
crime_train = crime_combined[train_ind,]
crime_test = crime_combined[-train_ind,]

#perform random forest to predict number of cases at district level
crime_forest = randomForest(tot_crime ~ .,
                            data = crime_train, ntree=500, importance = TRUE)

yhat_crime_forest = predict(crime_forest, crime_test)

#group the predicted number of cases by state
df4 = crime_test %>%
  mutate(yhat_crime_forest = predict(crime_forest, crime_test)) %>%
  select(State, yhat_crime_forest)
df5 = df4 %>%
  group_by(State) %>%
  mutate(tot_pred_crime = round(sum(yhat_crime_forest),2))%>%
  select(State, tot_pred_crime)
df6 = df5[!duplicated(df5), ]

#convert the predicted number of cases at state level to number of cases to be handled by each centre in every state
osc = read_csv(here("data/OSC.csv"))
dfn = merge(df6, osc, by = "State")
dfn = dfn %>%
  mutate(cases_per_osc = round(tot_pred_crime/OSCs)) %>%
  kbl(caption = "Number of cases to be handled by each OSC as per predictions") %>%
  kable_classic(full_width = F, html_font = "Cambria")
dfn

rf = modelr::rmse(crime_forest, crime_test)

#make a variable importance plot
vi = varImpPlot(crime_forest, type=1)

#plotting only 4 pdp graphs (Selectively)
pp1 = partialPlot(crime_forest, as.data.frame(crime_test), 'PC1', las=1)
pp2 = partialPlot(crime_forest, as.data.frame(crime_test), 'PC2', las=1)
pp3 = partialPlot(crime_forest, as.data.frame(crime_test), 'emp', las=1)
pp4 = partialPlot(crime_forest, as.data.frame(crime_test), 'head', las=1)
grid.arrange(grobs = list(pp1, pp2, pp3, pp4), ncol=3, nrow=3)