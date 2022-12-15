# Introduction-to-Statistical-Machine-Learning
This was my code for Pstat 131 homework regarding statistical Machine Learning that is used to discover patterns and relationships in large data sets


# 1. Descriptive summary statistics 
Given the lack of further information on the problem domain, it is wise to investigate some of the statistical properties of the data, so as to get a better grasp of the problem. It is always a good idea to start our analysis with some kind of exploratory data analysis. A first idea of the statistical properties of the data can be obtained through a summary of its descriptive statistics.

```{r}
algae %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(n=n(), na.rm = TRUE)
```

```{r}
#checking for missing values
# is.na(algae) > returns a lot of values, some of them being TRUE
chem_tibb <- algae %>% dplyr::select(mxPH:Chla)
chem_mean <- chem_tibb%>%
  summarise_all(mean, na.rm =TRUE) 
chem_var <- chem_tibb%>%
  summarise_all(var, na.rm =TRUE) 
chem_sum1 <- rbind(chem_mean, chem_var)
chem_sum1
```

```{r}
chem_med <- chem_tibb%>%
  summarise_all(median, na.rm = TRUE)
chem_mad <- chem_tibb%>%
  summarise_all(mad, na.rm =TRUE) 
```
```{r}
chem_summ <- rbind(chem_mean, chem_var, chem_med, chem_mad)
rownames(chem_summ) <- c("Mean", "Variance", "Median", "MAD")
chem_summ
"not sure why this isn't working to name the rows but
1: mean
2: variance
3: median
4: mad "
```

# 2. Data visualization 
```{r}
algae_mxPH <- algae %>% ggplot(aes(x = mxPH)) +
  geom_histogram(mapping = aes(y = ..density..), binwidth = 0.1, na.rm = TRUE, fill = 'dark green') 
mxPH_hist <- algae_mxPH + ggtitle("Histogram of mxPH")
mxPH_hist

```{r}
mxPH_hitplusdens <- mxPH_hist +
  geom_density(mapping = aes(x = mxPH, y = ..density..), na.rm = TRUE, color = "blue") +
  geom_rug(sides="b")
mxPH_hitplusdens
```

```{r}
ggplot(data = algae) + 
  geom_boxplot(mapping = aes(x = size, y = a1, fill = size), na.rm = TRUE)
```

```{r}
no3_box <- ggplot(data = algae) + 
  geom_boxplot(mapping = aes(x = size, y = NO3, fill = size), na.rm = TRUE)
nh4_box <- ggplot(data = algae) +
  geom_boxplot(mapping = aes(x = size, y = NH4, fill = size), na.rm = TRUE)
no3_box
nh4_box
```

```{r}
NO3_summ <- c(mean(algae$NO3, na.rm = TRUE), var(algae$NO3, na.rm =TRUE),
              median(algae$NO3, na.rm = TRUE), mad(algae$NO3, na.rm = TRUE))
NH4_summ <- c(mean(algae$NH4, na.rm = TRUE), var(algae$NH4, na.rm =TRUE),
              median(algae$NH4, na.rm = TRUE), mad(algae$NH4, na.rm = TRUE))
sum_col <- c("Mean", "Variance", "Median","MAD" )
sum_row <- c("NO3", "NH4")
chem_summary <- rbind(NO3_summ, NH4_summ)
colnames(chem_summary) <- sum_col
rownames(chem_summary) <- sum_row
chem_summary
```

# 3. Dealing with missing values
```{r}
algae %>%
  dplyr::select(season:a1) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))
```

```{r}
algae.del <- algae%>%
  dplyr :: select(season:a1)%>%
  filter(complete.cases(.))
algae.del
```


```{r}
algae.med <- algae %>% 
  dplyr::select(season:a1)%>%
  mutate_at(vars(starts_with("m")|starts_with("C") | starts_with('N') | starts_with('o') |
                   starts_with("P")), funs(ifelse(is.na(.),median(., na.rm = TRUE),.)))
spec_algae.med <- algae.med %>%
  slice(which(row_number() == 48 | row_number() == 62 | row_number() == 199))
spec_algae.med
```

```{r}
algae_use <- algae.del%>%
  dplyr::select(mxPH:Chla)
cor(algae_use)
```

```{r}
po4_opo4.lm <- lm(algae$PO4 ~ algae$oPO4)
summary(po4_opo4.lm)
```
```{r}
algae%>%
  dplyr::select(season:a1)%>%
  slice(which(row_number()==28))
```

```{r}
ci <- predict(po4_opo4.lm, data.frame(g=4), interval = "confidence", level = 0.95, 
             type = "response")
ci[28] #need to do this or get a value at every observation, only need 28th
```

```{r}
algae$PO4[28] <- 48.07
algae%>%
  dplyr::select(season:a1)%>%
  slice(which(row_number()==28))
```
# 4. Cross-validation: 
```{r}
set.seed(561)
ob_id <- (1:200)
chonk <- cut(ob_id, breaks = 5, label = FALSE)
chonkdef <- sample(chonk, 200, replace = TRUE)
chonkdef
```

```{r}
do.chunk <- function(chunkid, chunkdef, dat){ # function argument
  train = (chunkdef != chunkid)
  
  Xtr = dat[train,1:11] # get training set
  Ytr = dat[train,12] # get true response values in trainig set
  Xvl = dat[!train,1:11] # get validation set
  Yvl = dat[!train,12] # get true response values in validation set
  lm.a1 <- lm(a1~., data = dat[train,1:12])
  predYtr = predict(lm.a1) # predict training values
  predYvl = predict(lm.a1,Xvl) # predict validation values
  
  data.frame(fold = chunkid,
      train.error = mean((predYtr - Ytr$a1)^2), # compute and store training error
      val.error = mean((predYvl - Yvl$a1)^2)) # compute and store test error
}
```

```{r}
set.seed(679)
errors <- ldply(1:5, do.chunk, chunkdef = chonkdef, dat = algae.med)
errors
```

```{r}
errors%>%
  dplyr::select(val.error)%>%
  summarise_all(mean)
```


```{r}
algae.Test <- read_table2('algae_data/algaeTest.txt',
                    col_names=c('season','size','speed','mxPH','mnO2','Cl','NO3',
                                'NH4','oPO4','PO4','Chla','a1'),
                    na=c('XXXXXXX'))
```

```{r}
# using the model we got from question 4
lm.a1 <- lm(a1~., data = algae.Test)
# use the predictions from the Test file
algae.predictions <- predict(lm.a1, algae.Test)
TrueTestError <- mean((algae.predictions-algae.Test$a1)^2)
TrueTestError
```


# Cross Validation (CV) for Model Selection
```{r}
library(ISLR)
head(Wage)
```

```{r}
ggplot(data = Wage, mapping = aes(x = age, y = wage)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
```

i.
```{r}
folds.mod <- cut(1:nrow(Wage), breaks = 5, labels= FALSE)%>%sample()
do.chonk <- function(chunkid, chunkdef, dat,p){ # function argument
  train = (chunkdef != chunkid)
  
  Xtr = dat[train,] # get training set
  Ytr = dat[train,] # get true response values in trainig set
  Xvl = dat[!train,] # get validation set
  Yvl = dat[!train,] # get true response values in validation set
  
  if(p==0)
  {
    lm.mod = lm(wage~1, data = dat[train,])
  }
  else
  {
    lm.mod = lm(wage~poly(age, degree = p, raw = FALSE), data = dat[train,])
  }
  predYtr = predict(lm.mod) # predict training values
  predYvl = predict(lm.mod,Xvl) # predict validation values
  
  data.frame(fold = chunkid,
      train.error = mean((predYtr - Ytr$wage)^2), # compute and store training error
      val.error = mean((predYvl - Yvl$wage)^2)) # compute and store test error
}
```

ii.
```{r}
errors.mod = NULL
set.seed(834)
for(k in 0:10)
{
  temp.mod <- ldply(1:5, do.chonk, chunkdef = folds.mod, dat = Wage, p = k)
  temp.mod$degree<-k
  errors.mod <- rbind(errors.mod, temp.mod)
}
errors.mod
```

```{r}
errors.mod%>%
  dplyr::select(train.error:val.error)%>%
  summarise_all(mean)
```

```{r dplyr}
"grouping errors by degrees"
use_new_error_sum <- errors.mod%>%
  group_by(degree)%>%
  summarise_at(vars(train.error, val.error), list(name = mean))
use_new_error_sum
```


```{r}
# Plot errors
ggplot() + 
  geom_line(data = use_new_error_sum, aes(x = degree, y = val.error_name, color = "red")) + 
  geom_line(data = use_new_error_sum, aes(x = degree, y = train.error_name, color = "blue")) +
  labs(title = "degrees vs. error", x="degrees", y="error", color = "Type of Errors") +
  scale_color_discrete(name = "Error Term", labels = c("Training", "Validation"))
"Not sure why my code is assigning a different color to the values but the graph 
is correct in terms of values
i.e. Validation error ends up beinging higher than training error"
```
