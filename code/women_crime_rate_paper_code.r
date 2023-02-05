*************************************************************************************LIBRARIES********************************************************************
# installs the librarian package if you don't have it
if (!("librarian" %in% rownames(utils::installed.packages()))) {
  utils::install.packages("librarian")
}
# put all of the packages that you import here
librarian::shelf( 
  cran_repo = "https://cran.microsoft.com/", 
  ask = FALSE,
  stats, 
  here,
  kableExtra,
  rlang,
  ggthemes,
  tidyverse,
  janitor,
  magrittr,
  glue,
  lubridate,
  haven,
  snakecase,
  sandwich,
  lmtest,
  gganimate,
  gapminder,
  stargazer,
  snakecase,
  rpart,
  rpart.plot,
  rsample,
  randomForest,
  modelr,
  gbm,
  pdp,
  remotes,
  urbnmapr,
  ggmap,
  maps,
  mapdata,
  usmap,
  scales,
  foreach,
  caret,
  mosaic,
  LICORS,
  ggcorrplot,
  arules,
  arulesViz,
  igraph,
  RColorBrewer,
  parallel,
  iterators,
  doParallel,
  gifski,
  htmlTable,
  magick,
  factoextra
)

here::i_am("code/libraries.R")
devtools::install_github('yihui/knitr')

*************************************************************************************OVERVIEW STATISTICS**********************************************************

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

*************************************************************************************CLUSTERING********************************************************************

#reading the file and scaling and centering
fempos = read_csv(here("data/Position of females.csv"))
X = fempos[,-1]
X = scale(X, center=TRUE, scale=TRUE)
X_dist = dist(X, method='euclidean')

#hierarchical clustering based on women empowerment factors
h2 = hclust(X_dist, method='complete')
#plot(h2)
cluster2 = cutree(h2, k=5)

D2 = data.frame(X, z1 = cluster2, z2 = fempos$State)
ggplot(D2) + geom_point(aes(x=literacy, y=I4, col=factor(z1)))+
  labs(title = "Clustering of literacy rates among women who think they should be involved in major decision making",
       x = "Female Literacy Rate",
       y = "Decision Making")
ggsave(here("figures/Clustering of literacy rates among women who think they should be involved in major decision making.png"), width=8, height=4.5)

#summarise state and cluster mapping
# df1 = D2 %>%
#   select(z1,z2) %>%
#   arrange(z1)
# colnames(df1) = c("Cluster","State/UT")
# df1 = df1 %>%
#   kbl(caption = "Cluster grouping of states according to factors affecting women empowerment") %>%
#   kable_classic(full_width = F, html_font = "Cambria")
# df1

#law enforcement clustering
lawenf = read_csv(here("data/Law Enforcement.csv"))
Y = lawenf[,-1]
Y = scale(Y, center=TRUE, scale=TRUE)
Y_dist = dist(Y, method='euclidean')

#hierarchical clustering based on law enforcement factors 
h1 = hclust(Y_dist, method='complete')
#plot(h1)
cluster1 = cutree(h1, k=5)
D1 = data.frame(Y, a1 = cluster1, a2 = lawenf$State)
ggplot(D1) + geom_point(aes(x=court_pending, y=court_convic, col=factor(a1)))+
  labs(title = "Clustering of states based on law enforcement factors",
       x = "Number of cases pending for trail",
       y = "Number of cases convicted in court")
ggsave(here("figures/Clustering of states based on law enforcement factors.png"), width=8, height=4.5)


# df2 = D1 %>%
#   select(a1,a2) %>%
#   arrange(a1)
# colnames(df2) = c("Cluster","State/UT")
# df2 = df2 %>%
#   kbl(caption = "Cluster grouping of states according to law enforcement factors") %>%
#   kable_classic(full_width = F, html_font = "Cambria")
# df2


*************************************************************************************PCA***************************************************************************

#reading the file and scaling and centering
crime = read_csv(here("data/Crime raw data.csv"))
L = crime[, -(1:2)]
L = scale(L, center=TRUE, scale=TRUE)

pc1 = prcomp(L, scale=TRUE, rank=18)

#summary(pc1)
#var_explained = pc1$sd^2/sum(pc1$sd^2)*100

#plot the scree plot to decide the right number of components
# qplot(c(1:37), var_explained) + 
#   geom_line() + 
#   xlab("Principal Component") + 
#   ylab("Variance Explained") +
#   ggtitle("Scree Plot") +
#   ylim(0, 20)

loadings_summary = pc1$rotation %>%
  as.data.frame()%>%
  rownames_to_column('crime_category')

#for simplicity we plot only first 2 components
ggplot(loadings_summary) +geom_col(aes(x=reorder(crime_category, PC1),y=PC1))+xlab("PC1")+ylab("Crime Category")+coord_flip()
ggsave(here("figures/PC1.png"), width=8, height=4.5)

ggplot(loadings_summary) +geom_col(aes(x=reorder(crime_category, PC2),y=PC2))+xlab("PC2")+ylab("Crime Category")+coord_flip()
ggsave(here("figures/PC2.png"), width=8, height=4.5)

df3 = round(pc1$rotation,2) %>%
  kbl(caption = "Principal Components of crime categories") %>%
  kable_classic(full_width = F, html_font = "Cambria")
df3


*************************************************************************************RANDOM FOREST*****************************************************************

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