### Holt and Holt-Winters Forecasting
#### Author: Andrew Pierson
#### Date: 11/12/2018


```r
#Enable knit to csv
knitr::opts_chunk$set(echo = TRUE)

#Read in packages that have already been installed
library(forecast)
library(expsmooth)
library(seasonal)
library(fpp2)
```

```
## Loading required package: ggplot2
```

```
## Loading required package: fma
```

```r
library(ggplot2)
```


Here I changed the forecast parameters to use the default settings and also have varying alpha and beta parameter settings. The second and third fits both have the same alpha value, this is to give us an indication to how our beta parameter is affecting the forecast. The third and fourth fits have a constant beta value to give a better understanding of how the alpha parameter changes the forecast.

```r
#Forecast the specified window of data
fc1 <- holt(eggs, h=15)
fc2 <- holt(eggs,  alpha=0.9, beta=0.1, initial="simple", h = 15)
fc3 <- holt(eggs,  alpha=0.9, beta=0.6, initial="simple", h = 15)
fc4 <- holt(eggs,  alpha=0.5, beta=0.8, initial="simple", h = 15)
fc5 <- holt(eggs,  alpha=0.1, beta=0.8, initial="simple", h = 15)

#Plot the forecasts
autoplot(eggs) + 
  autolayer(fc1, series = "Holt's Method", PI = FALSE, color = 'red') + 
  autolayer(fc2, series = "Holt's Method", PI = FALSE, color = 'blue') + 
  autolayer(fc3, series = "Holt's Method", PI = FALSE, color = 'green') + 
  autolayer(fc4, series = "Holt's Method", PI = FALSE, color = 'yellow') + 
  autolayer(fc5, series = "Holt's Method", PI = FALSE, color = 'purple') +   
  ggtitle("Forecast from Holt's Linear Method") + xlab("Year") + ylab("Thousands of cars") + 
  guides(color = guide_legend(title = "Forecast"))
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/1-1.png)<!-- -->


The intuition that I would explain when changing the parameters for alpha and beta would be different. While changing the parameter of alpha, I would describe an increase in the parameter value as having an effect where the forecasted series' level is increasing. For the beta parameter, I would describe changes to this parameter as having an effect on the trend of the forecast. 

```r
#Forecast the specified window of data
fc1_h100 <- holt(eggs, h=100)
fc2_h100 <- holt(eggs,  alpha=0.9, beta=0.1, initial="simple", h = 100)
fc3_h100 <- holt(eggs,  alpha=0.9, beta=0.6, initial="simple", h = 100)
fc4_h100 <- holt(eggs,  alpha=0.5, beta=0.8, initial="simple", h = 100)
fc5_h100 <- holt(eggs,  alpha=0.1, beta=0.8, initial="simple", h = 100)

#Plot the forecasts
autoplot(eggs) + 
  autolayer(fc1, series = "Holt's Method", PI = FALSE, color = 'red') + 
  autolayer(fc2, series = "Holt's Method", PI = FALSE, color = 'blue') + 
  autolayer(fc3, series = "Holt's Method", PI = FALSE, color = 'green') + 
  autolayer(fc4, series = "Holt's Method", PI = FALSE, color = 'yellow') + 
  autolayer(fc5, series = "Holt's Method", PI = FALSE, color = 'purple') +   
  ggtitle("Forecast from Holt's Linear Method") + xlab("Year") + ylab("Thousands of cars") + 
  guides(color = guide_legend(title = "Forecast"))
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/2-1.png)<!-- -->


The model with the lowest RMSE is the first model, using the automatically adjusted alpha and beta parameters. Respectively they are 0.8124 and 1e-04.

```r
#RMSE of the 5 different holt models
summary(fc1)
```

```
## 
## Forecast method: Holt's method
## 
## Model Information:
## Holt's method 
## 
## Call:
##  holt(y = eggs, h = 15) 
## 
##   Smoothing parameters:
##     alpha = 0.8124 
##     beta  = 1e-04 
## 
##   Initial states:
##     l = 314.7232 
##     b = -2.7222 
## 
##   sigma:  27.1665
## 
##      AIC     AICc      BIC 
## 1053.755 1054.437 1066.472 
## 
## Error measures:
##                      ME     RMSE      MAE       MPE     MAPE      MASE
## Training set 0.04499087 26.58219 19.18491 -1.142201 9.653791 0.9463626
##                    ACF1
## Training set 0.01348202
## 
## Forecasts:
##      Point Forecast      Lo 80     Hi 80       Lo 95    Hi 95
## 1994       59.78553  24.970286  94.60078    6.540207 113.0309
## 1995       57.06373  12.206005 101.92145  -11.540238 125.6677
## 1996       54.34192   1.308668 107.37517  -26.765440 135.4493
## 1997       51.62011  -8.488401 111.72863  -40.307926 143.5482
## 1998       48.89831 -17.537664 115.33428  -52.706742 150.5034
## 1999       46.17650 -26.035964 118.38896  -64.262933 156.6159
## 2000       43.45469 -34.106500 121.01589  -75.164916 162.0743
## 2001       40.73289 -41.832449 123.29822  -85.539898 167.0057
## 2002       38.01108 -49.273098 125.29526  -95.478551 171.5007
## 2003       35.28927 -56.472472 127.05102 -105.048206 175.6268
## 2004       32.56747 -63.464327 128.59926 -114.300487 179.4354
## 2005       29.84566 -70.275216 129.96654 -123.276007 182.9673
## 2006       27.12385 -76.926479 131.17418 -132.007398 186.2551
## 2007       24.40205 -83.435566 132.23966 -140.521350 189.3254
## 2008       21.68024 -89.816966 133.17744 -148.840022 192.2005
```

```r
summary(fc2)
```

```
## 
## Forecast method: Holt's method
## 
## Model Information:
## Holt's method 
## 
## Call:
##  holt(y = eggs, h = 15, initial = "simple", alpha = 0.9, beta = 0.1) 
## 
##   Smoothing parameters:
##     alpha = 0.9 
##     beta  = 0.1 
## 
##   Initial states:
##     l = 276.79 
##     b = 38.63 
## 
##   sigma:  29.1696
## Error measures:
##                     ME     RMSE      MAE       MPE     MAPE     MASE
## Training set -4.972222 29.16955 22.31416 -2.409186 10.93392 1.100724
##                    ACF1
## Training set 0.02152577
## 
## Forecasts:
##      Point Forecast       Lo 80     Hi 80       Lo 95    Hi 95
## 1994       58.82279   21.440499  96.20508    1.651512 115.9941
## 1995       55.38779    2.521252 108.25433  -25.464603 136.2402
## 1996       51.95279  -15.023081 118.92867  -50.477967 154.3836
## 1997       48.51780  -32.092840 129.12843  -74.765533 171.8011
## 1998       45.08280  -49.043395 139.20900  -98.870793 189.0364
## 1999       41.64780  -66.049523 149.34513 -123.061044 206.3567
## 2000       38.21281  -83.207718 159.63333 -147.483861 223.9095
## 2001       34.77781 -100.574983 170.13060 -172.226423 241.7820
## 2002       31.34281 -118.186337 180.87197 -197.342288 260.0279
## 2003       27.90782 -136.063698 191.87933 -222.864975 278.6806
## 2004       24.47282 -154.220771 203.16641 -248.815445 297.7611
## 2005       21.03782 -172.665923 214.74157 -275.206493 317.2821
## 2006       17.60283 -191.403947 226.60960 -302.045451 337.2511
## 2007       14.16783 -210.437199 238.77286 -329.335920 357.6716
## 2008       10.73283 -229.766340 251.23201 -357.078913 378.5446
```

```r
summary(fc3)
```

```
## 
## Forecast method: Holt's method
## 
## Model Information:
## Holt's method 
## 
## Call:
##  holt(y = eggs, h = 15, initial = "simple", alpha = 0.9, beta = 0.6) 
## 
##   Smoothing parameters:
##     alpha = 0.9 
##     beta  = 0.6 
## 
##   Initial states:
##     l = 276.79 
##     b = 38.63 
## 
##   sigma:  32.2677
## Error measures:
##                      ME     RMSE      MAE       MPE     MAPE     MASE
## Training set -0.8570933 32.26771 25.85968 -0.709139 13.16484 1.275619
##                    ACF1
## Training set -0.2249222
## 
## Forecasts:
##      Point Forecast      Lo 80     Hi 80        Lo 95     Hi 95
## 1994      57.103879   15.75115  98.45661    -6.139662  120.3474
## 1995      52.227823  -22.32187 126.77751   -61.786091  166.2417
## 1996      47.351767  -67.09896 161.80249  -127.685515  222.3890
## 1997      42.475711 -117.41556 202.36699  -202.056892  287.0083
## 1998      37.599656 -172.60892 247.80823  -283.886615  359.0859
## 1999      32.723600 -232.22446 297.67166  -372.479498  437.9267
## 2000      27.847544 -295.92082 351.61591  -467.313435  523.0085
## 2001      22.971488 -363.42766 409.37064  -567.975019  613.9180
## 2002      18.095432 -434.52336 470.71422  -674.125272  710.3161
## 2003      13.219376 -509.02155 535.46030  -785.479190  811.9179
## 2004       8.343321 -586.76253 603.44917  -901.792538  918.4792
## 2005       3.467265 -667.60738 674.54191 -1022.852842 1029.7874
## 2006      -1.408791 -751.43375 748.61617 -1148.472983 1145.6554
## 2007      -6.284847 -838.13278 825.56309 -1278.486479 1265.9168
## 2008     -11.160903 -927.60678 905.28498 -1412.743927 1390.4221
```

```r
summary(fc4)
```

```
## 
## Forecast method: Holt's method
## 
## Model Information:
## Holt's method 
## 
## Call:
##  holt(y = eggs, h = 15, initial = "simple", alpha = 0.5, beta = 0.8) 
## 
##   Smoothing parameters:
##     alpha = 0.5 
##     beta  = 0.8 
## 
##   Initial states:
##     l = 276.79 
##     b = 38.63 
## 
##   sigma:  35.1083
## Error measures:
##                    ME     RMSE      MAE       MPE     MAPE     MASE
## Training set -1.18793 35.10831 25.78781 -1.097409 13.03675 1.272074
##                   ACF1
## Training set 0.3036492
## 
## Forecasts:
##      Point Forecast       Lo 80     Hi 80        Lo 95     Hi 95
## 1994     59.7814402    14.78832  104.7746    -9.029592  128.5925
## 1995     53.7452867   -20.04891  127.5395   -59.113197  166.6038
## 1996     47.7091333   -72.17874  167.5970  -135.643540  231.0618
## 1997     41.6729798  -135.52223  218.8682  -229.323701  312.6697
## 1998     35.6368263  -207.49258  278.7662  -336.197516  407.4712
## 1999     29.6006729  -286.79404  345.9954  -454.283281  513.4846
## 2000     23.5645194  -372.63046  419.7595  -582.363409  629.4924
## 2001     17.5283659  -464.44431  499.5010  -719.585226  754.6420
## 2002     11.4922125  -561.81260  584.7970  -865.301837  888.2863
## 2003      5.4560590  -664.39760  675.3097 -1018.996709 1029.9088
## 2004     -0.5800945  -771.92011  770.7599 -1180.242843 1179.0827
## 2005     -6.6162479  -884.14350  870.9110 -1348.678367 1335.4459
## 2006    -12.6524014 -1000.86348  975.5587 -1523.990818 1498.6860
## 2007    -18.6885549 -1121.90103 1084.5239 -1705.906439 1668.5293
## 2008    -24.7247083 -1247.09749 1197.6481 -1894.182547 1844.7331
```

```r
summary(fc5)
```

```
## 
## Forecast method: Holt's method
## 
## Model Information:
## Holt's method 
## 
## Call:
##  holt(y = eggs, h = 15, initial = "simple", alpha = 0.1, beta = 0.8) 
## 
##   Smoothing parameters:
##     alpha = 0.1 
##     beta  = 0.8 
## 
##   Initial states:
##     l = 276.79 
##     b = 38.63 
## 
##   sigma:  54.8783
## Error measures:
##                     ME     RMSE      MAE       MPE    MAPE     MASE
## Training set -4.730768 54.87828 42.97648 -0.821665 20.8104 2.119965
##                   ACF1
## Training set 0.8280457
## 
## Forecasts:
##      Point Forecast       Lo 80     Hi 80       Lo 95     Hi 95
## 1994       63.86687    -6.46247  134.1962   -43.69258  171.4263
## 1995       66.92150   -27.69696  161.5400   -77.78494  211.6279
## 1996       69.97612   -82.49427  222.4465  -163.20721  303.1594
## 1997       73.03074  -159.69458  305.7561  -282.89192  428.9534
## 1998       76.08536  -252.58677  404.7575  -426.57529  578.7460
## 1999       79.13998  -358.09139  516.3714  -589.54770  747.8277
## 2000       82.19460  -474.51891  638.9081  -769.22524  933.6144
## 2001       85.24922  -600.77739  771.2758  -963.93794 1134.4364
## 2002       88.30384  -736.08087  912.6886 -1172.48378 1349.0915
## 2003       91.35847  -879.82331 1062.5402 -1393.93589 1576.6528
## 2004       94.41309 -1031.51556 1220.3417 -1627.54618 1816.3724
## 2005       97.46771 -1190.75010 1385.6855 -1872.69142 2067.6268
## 2006      100.52233 -1357.17951 1558.2242 -2128.84025 2329.8849
## 2007      103.57695 -1530.50231 1737.6562 -2395.53162 2602.6855
## 2008      106.63157 -1710.45330 1923.7164 -2672.35993 2885.6231
```



```r
#Read in packages that have already been installed
library(forecast)
library(expsmooth)
library(seasonal)
library(ggplot2)
library(fpp2)
```


The ukcars dataset ranges from 1977 to 2005 and contains observations of the number of cars in thousands. The plot of data spans 112 periods, each lasting a quarter of a year. This plot appears to be seasonal and trending throughout certain ranges of years. One example of a trend from this dataset is the positive trend from around 1985 to 2000, which has quarterly seasonality.

```r
#Plot the ukcars dataset
autoplot(ukcars) + 
  ggtitle("UK Cars") + xlab("Year") + ylab("Thousands of cars")
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/5-1.png)<!-- -->


The first chart below is the ukcars dataset decomposed to compare the data to its seasonality, trend, and autocorrelation plots. This allows us to see the trend, which seems to start out decreasing and increase for about 20 years before seeing a sharp decrease. The seasonality also appears to be constant. The seasonally adjusted data seems to remove the constant seasonality from most of the data.

```r
library(zoo)
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
#Convert to zoo object
ukcarsdata <- as.zoo(ukcars)

#Decomposed data lists 8 seasonally adjusted data points
stl <- stl(ukcarsdata, s.window="periodic")
plot(stl)
```
![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/6-1.png)<!-- -->


```r
#Obtain seasonally adjusted data using x11
sad <- seasadj(stl)
plot(sad)
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/6-2.png)<!-- -->


```r
#seasonal::series(sad, "forecast.forecasts")
```


In this part I printed a summary of the fit that I was using and the forecasted data over 2 years which equated to eight periods in the holt forecasting function. The forecasted data indicates a trend that is increasing linearly over the next 2 years. 

The parameters of the holt's method function that I used were sad and h = 8. These parameters indicate that the data I am using has been seasonally adjusted and that I am forecasting eight periods. The damped parameter has been automatically set to false and the initial set to simple. It has also indicated that it is not using an exponential trend.

```r
#Use Holt's method to create a forecast
fc <- holt(sad, h=8)

#Run the summary of the forecasting method
summary(fc)
```

```
## 
## Forecast method: Holt's method
## 
## Model Information:
## Holt's method 
## 
## Call:
##  holt(y = sad, h = 8) 
## 
##   Smoothing parameters:
##     alpha = 0.6049 
##     beta  = 1e-04 
## 
##   Initial states:
##     l = 334.5744 
##     b = 0.8354 
## 
##   sigma:  25.7197
## 
##      AIC     AICc      BIC 
## 1274.003 1274.563 1287.640 
## 
## Error measures:
##                     ME     RMSE      MAE       MPE     MAPE    MASE
## Training set -0.311188 25.26041 20.10954 -0.638754 6.490918 0.65536
##                    ACF1
## Training set 0.03183994
## 
## Forecasts:
##         Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
## 2005 Q2       407.6327 374.6716 440.5939 357.2230 458.0425
## 2005 Q3       408.4646 369.9401 446.9890 349.5465 467.3826
## 2005 Q4       409.2964 365.9149 452.6779 342.9501 475.6427
## 2006 Q1       410.1282 362.3798 457.8767 337.1033 483.1531
## 2006 Q2       410.9601 359.2107 462.7095 331.8162 490.1039
## 2006 Q3       411.7919 356.3282 467.2556 326.9675 496.6163
## 2006 Q4       412.6237 353.6783 471.5692 322.4744 502.7731
## 2007 Q1       413.4556 351.2217 475.6894 318.2771 508.6340
```

```r
#Plot Holt's linear method forecast
autoplot(sad) + 
  autolayer(fc, series = "Holt's Method", PI = FALSE) + 
  ggtitle("Forecast from Holt's Linear Method") + xlab("Year") + ylab("Thousands of cars") + 
  guides(color = guide_legend(title = "Forecast"))
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/7-1.png)<!-- -->


The re-seasonalized forecasts look reasonable from a standpoint of having a similar variation across periods. However, there does not appear to be any trend to the forecasted seasonality.

```r
#Decompose the data using the stl() function
decomp <- stl(ukcars, s.window="periodic")

#Fit the data using Holt's linear method
fit <- holt(sad, h = 8)

#Define the data from the previous period
lastyear <- rep(decomp$time.series[110:113,"seasonal"],2) 
fc <- fit$mean + lastyear

#Plot the forecast using Holt's method with seasonalized data
autoplot(ukcars) + 
  autolayer(fc, series = "Holt's Method", PI = FALSE, color = 'green') + 
  ggtitle("Forecast from Holt's Linear Method") + xlab("Year") + ylab("Thousands of cars") + 
  guides(color = guide_legend(title = "Forecast"))
```

```
## Warning: Ignoring unknown parameters: PI
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/8-1.png)<!-- -->


After applying the ets function to the ukcars data I would recommend using a ETS(A, N, A) model. This result comes from the ets parameter model = "ZZZ" which allows the model to choose the framework of Hyndman where A denotes additive, M denotes multiplicative, and N denotes none. Additionally, the place of the framework identifies (error type, trend type, season type). 

```r
#Use ets() function to choose a seasonal model
ets(ukcars, model = "ZZZ")
```

```
## ETS(A,N,A) 
## 
## Call:
##  ets(y = ukcars, model = "ZZZ") 
## 
##   Smoothing parameters:
##     alpha = 0.6199 
##     gamma = 1e-04 
## 
##   Initial states:
##     l = 314.2568 
##     s = -1.7579 -44.9601 21.1956 25.5223
## 
##   sigma:  25.9302
## 
##      AIC     AICc      BIC 
## 1277.752 1278.819 1296.844
```


Apply both an X11 and a SEATS model using a two year horizon. 

```r
#Load seasonal package from library
library(seasonal)
library(seasonalview)
```

```
## 
## Attaching package: 'seasonalview'
```

```
## The following object is masked from 'package:seasonal':
## 
##     view
```

```r
library(shiny)
ukcars <- ukcars
#Define seas function for X-11ARIMA
x11 <- seas(ukcars,
          x11 = "",
          transform.function = "auto",
          forecast.maxlead = 12,
          forecast.probability = 0.9,
          forecast.exclude = 4)

#Print the x11 forecasted data
a <- seasonal::series(x11, "forecast.forecasts")
print("X11 Forecast:")
```

```
## [1] "X11 Forecast:"
```

```r
print(a)
```

```
##         forecast  lowerci  upperci
## 2005 Q2 417.6321 373.2136 462.0506
## 2005 Q3 372.9189 316.8020 429.0358
## 2005 Q4 398.2493 336.1996 460.2991
## 2006 Q1 434.2930 368.9625 499.6235
## 2006 Q2 418.7879 346.8414 490.7344
## 2006 Q3 373.8113 298.1959 449.4267
## 2006 Q4 398.9383 321.2182 476.6584
## 2007 Q1 434.8249 355.8770 513.7729
## 2007 Q2 419.1986 336.3681 502.0290
## 2007 Q3 374.1283 289.0678 459.1889
## 2007 Q4 399.1831 312.8206 485.5456
## 2008 Q1 435.0139 347.8846 522.1432
```

```r
#Define seas function for SEATS
seats <- seas(ukcars,
          transform.function = "auto",
          forecast.maxlead = 12,
          forecast.probability = 0.9,
          forecast.exclude = 4)

#Print the SEATS forecasted data
b <- seasonal::series(seats, "forecast.forecasts")
print("SEATS Forecast:")
```

```
## [1] "SEATS Forecast:"
```

```r
print(b)
```

```
##         forecast  lowerci  upperci
## 2004 Q2 427.0291 382.6106 471.4476
## 2004 Q3 381.1440 325.0271 437.2609
## 2004 Q4 408.8675 346.8177 470.9173
## 2005 Q1 441.6548 376.3243 506.9854
## 2005 Q2 424.0928 352.1463 496.0393
## 2005 Q3 378.8769 303.2615 454.4924
## 2005 Q4 407.1172 329.3971 484.8372
## 2006 Q1 440.3035 361.3555 519.2514
## 2006 Q2 423.0494 340.2190 505.8799
## 2006 Q3 378.0714 293.0109 463.1319
## 2006 Q4 406.4952 320.1328 492.8577
## 2007 Q1 439.8233 352.6940 526.9526
```


The RMSE of the fitted model, which is the holt linear trend method applied to seasonally adjusted data and forecasted over an eight period horizon gives the optimal value. The X11 model and SEATS model both give an output of 26.52 for their RMSE values. I would explain this by saying that there wasn't any variation between methods.

```r
#RMSE of the X11 series model
print("X11 RMSE:")
```

```
## [1] "X11 RMSE:"
```

```r
sqrt(mean((x11$series$rsd)**2))
```

```
## [1] 26.52227
```

```r
#RMSE of the fitted model
print("SEATS RMSE:")
```

```
## [1] "SEATS RMSE:"
```

```r
sqrt(mean((seats$series$rsd)**2))
```

```
## [1] 26.52227
```

```r
#RMSE of the fitted model
print("FIT RMSE:")
```

```
## [1] "FIT RMSE:"
```

```r
summary(fit)
```

```
## 
## Forecast method: Holt's method
## 
## Model Information:
## Holt's method 
## 
## Call:
##  holt(y = sad, h = 8) 
## 
##   Smoothing parameters:
##     alpha = 0.6049 
##     beta  = 1e-04 
## 
##   Initial states:
##     l = 334.5744 
##     b = 0.8354 
## 
##   sigma:  25.7197
## 
##      AIC     AICc      BIC 
## 1274.003 1274.563 1287.640 
## 
## Error measures:
##                     ME     RMSE      MAE       MPE     MAPE    MASE
## Training set -0.311188 25.26041 20.10954 -0.638754 6.490918 0.65536
##                    ACF1
## Training set 0.03183994
## 
## Forecasts:
##         Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
## 2005 Q2       407.6327 374.6716 440.5939 357.2230 458.0425
## 2005 Q3       408.4646 369.9401 446.9890 349.5465 467.3826
## 2005 Q4       409.2964 365.9149 452.6779 342.9501 475.6427
## 2006 Q1       410.1282 362.3798 457.8767 337.1033 483.1531
## 2006 Q2       410.9601 359.2107 462.7095 331.8162 490.1039
## 2006 Q3       411.7919 356.3282 467.2556 326.9675 496.6163
## 2006 Q4       412.6237 353.6783 471.5692 322.4744 502.7731
## 2007 Q1       413.4556 351.2217 475.6894 318.2771 508.6340
```


The forecast from the x11 approach seems the most reasonable because it appears to have a consistent seasonality.

```r
#Plot the data
autoplot(ukcars) + 
  autolayer(a, series = "Holt's Method", PI = FALSE, color = 'green') + 
  ggtitle("Forecast from Holt's Linear Method") + xlab("Year") + ylab("Thousands of cars") + 
  guides(color = guide_legend(title = "Forecast"))
```

```
## For a multivariate timeseries, specify a seriesname for each timeseries. Defaulting to column names.
```

```
## Warning: Ignoring unknown parameters: PI

## Warning: Ignoring unknown parameters: PI

## Warning: Ignoring unknown parameters: PI
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/12-1.png)<!-- -->


```r
#Plot the data
autoplot(ukcars) + 
  autolayer(b, series = "Holt's Method", PI = FALSE, color = 'green') + 
  ggtitle("Forecast from Holt's Linear Method") + xlab("Year") + ylab("Thousands of cars") + 
  guides(color = guide_legend(title = "Forecast"))
```

```
## For a multivariate timeseries, specify a seriesname for each timeseries. Defaulting to column names.
```

```
## Warning: Ignoring unknown parameters: PI

## Warning: Ignoring unknown parameters: PI

## Warning: Ignoring unknown parameters: PI
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/12-2.png)<!-- -->


```r
#Plot the data
autoplot(ukcars) + 
  autolayer(fit, series = "Holt's Method", PI = FALSE, color = 'green') + 
  ggtitle("Forecast from Holt's Linear Method") + xlab("Year") + ylab("Thousands of cars") + 
  guides(color = guide_legend(title = "Forecast"))
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/12-3.png)<!-- -->


```r
#Read in packages that have already been installed
library(expsmooth)
library(forecast)
library(fpp2)
```


This data is a time series from 1985 to 2005 which seems to have an increasing seasonality over periods and an increasing trend. There also appears to be an outlier that creates a large decrease in the time series around 1989 and 2003.

```r
#Plot the data
autoplot(visitors) + 
  xlab("Year") + ylab("Visitor nights (millions") + 
  ggtitle("International visitors nights in Australia")
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/14-1.png)<!-- -->


The multiplicative seasonality is necessary when forecasting using the Holt-Winters' method to ensure that the seasonality is increasing. 

```r
#Fit the forecast using Holt-Winters' multiplicative method
hwm <- hw(visitors, seasonal = "multiplicative")
#Plot the Holt-Winters' forecast
autoplot(visitors) + 
  autolayer(hwm, series = "HW multiplicative forecasts", PI = FALSE) + 
  xlab("Year") + ylab("Visitor nights (millions") + 
  ggtitle("International visitors nights in Australia")
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/15-1.png)<!-- -->


I chose the multiplicative seasonal parameter and damped parameter set to true to see if the dampening effect was similar to the trend of the original time series. The other trenf that I chose was to see a Holt-Winters' model with an additive parameter. I was interested to see what would happen if the seasonality did not keep increasing as it had in the original time series.

```r
#Holt-Winters multiplicative method with damped trend
hwmd <- hw(visitors, damped = TRUE, seasonal = "multiplicative")

#Holt-Winters multiplicative method with exponential trend
hwme <- hw(visitors, seasonal = "additive")

#Plot the Damped Holt-Winters' forecast
autoplot(visitors) + 
  autolayer(hwmd, series = "HW multiplicative damped forecasts", PI = FALSE) + 
  xlab("Year") + ylab("Visitor nights (millions") + 
  ggtitle("International visitors nights in Australia")
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/16-1.png)<!-- -->


```r
#Plot the Additive Holt-Winters' forecast
autoplot(visitors) + 
  autolayer(hwme, series = "HW additive forecasts", PI = FALSE) + 
  xlab("Year") + ylab("Visitor nights (millions") + 
  ggtitle("International visitors nights in Australia")
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/16-2.png)<!-- -->


The Holt-Winters' default method, Holt-Winters' Damped Multiplicative Method, and Holt-Winters' Additive method returned respective RMSE values of 14.66, 14.41, and 18.02. Multiplicative Damped Holt-Winters' Method returns the smallest RMSE value, therefore I would choose this method. 

```r
#RMSE of Holt-Winters' Method
summary(hwm)
```

```
## 
## Forecast method: Holt-Winters' multiplicative method
## 
## Model Information:
## Holt-Winters' multiplicative method 
## 
## Call:
##  hw(y = visitors, seasonal = "multiplicative") 
## 
##   Smoothing parameters:
##     alpha = 0.5653 
##     beta  = 0.0215 
##     gamma = 5e-04 
## 
##   Initial states:
##     l = 91.7613 
##     b = 2.4333 
##     s = 0.935 1.0545 1.0841 0.9724 1.3037 1.0824
##            1.0258 0.9102 0.9304 1.0521 0.8518 0.7976
## 
##   sigma:  0.0565
## 
##      AIC     AICc      BIC 
## 2628.219 2630.976 2687.390 
## 
## Error measures:
##                       ME    RMSE      MAE        MPE     MAPE      MASE
## Training set -0.09495709 14.6622 10.97229 -0.3070136 4.188878 0.4051965
##                    ACF1
## Training set 0.07998858
## 
## Forecasts:
##          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
## May 2005       363.6434 337.2901 389.9967 323.3395 403.9474
## Jun 2005       389.5974 356.8764 422.3184 339.5550 439.6399
## Jul 2005       482.7709 437.0247 528.5172 412.8081 552.7338
## Aug 2005       428.5001 383.4951 473.5050 359.6709 497.3293
## Sep 2005       420.4548 372.1236 468.7860 346.5386 494.3710
## Oct 2005       475.5963 416.3290 534.8636 384.9547 566.2379
## Nov 2005       503.4598 435.9471 570.9725 400.2080 606.7116
## Dec 2005       608.3779 521.1122 695.6436 474.9165 741.8393
## Jan 2006       455.1525 385.6597 524.6452 348.8725 561.4324
## Feb 2006       509.0590 426.6702 591.4478 383.0562 635.0618
## Mar 2006       496.8101 411.8771 581.7431 366.9162 626.7039
## Apr 2006       441.9586 362.3923 521.5249 320.2724 563.6448
## May 2006       378.2099 306.6932 449.7266 268.8345 487.5852
## Jun 2006       405.1516 324.8788 485.4244 282.3849 527.9183
## Jul 2006       501.9810 397.9885 605.9736 342.9382 661.0239
## Aug 2006       445.4943 349.1776 541.8110 298.1906 592.7980
## Sep 2006       437.0751 338.6243 535.5258 286.5076 587.6425
## Oct 2006       494.3345 378.5064 610.1626 317.1908 671.4783
## Nov 2006       523.2309 395.8806 650.5813 328.4653 717.9965
## Dec 2006       632.1913 472.5654 791.8172 388.0646 876.3180
## Jan 2007       472.9103 349.1848 596.6358 283.6884 662.1322
## Feb 2007       528.8557 385.6501 672.0612 309.8416 747.8697
## Mar 2007       516.0680 371.5831 660.5529 295.0974 737.0386
## Apr 2007       459.0351 326.2850 591.7853 256.0113 662.0590
```

```r
#RMSE of Holt-Winters' Damped Method
summary(hwmd)
```

```
## 
## Forecast method: Damped Holt-Winters' multiplicative method
## 
## Model Information:
## Damped Holt-Winters' multiplicative method 
## 
## Call:
##  hw(y = visitors, seasonal = "multiplicative", damped = TRUE) 
## 
##   Smoothing parameters:
##     alpha = 0.6668 
##     beta  = 0.0043 
##     gamma = 1e-04 
##     phi   = 0.98 
## 
##   Initial states:
##     l = 91.5731 
##     b = 2.1794 
##     s = 0.9303 1.0531 1.086 0.9822 1.3144 1.0796
##            1.025 0.9094 0.9322 1.05 0.8485 0.7895
## 
##   sigma:  0.0568
## 
##      AIC     AICc      BIC 
## 2628.489 2631.584 2691.140 
## 
## Error measures:
##                    ME     RMSE      MAE       MPE     MAPE      MASE
## Training set 1.286455 14.41189 10.67154 0.2674105 4.065573 0.3940899
##                     ACF1
## Training set -0.02073956
## 
## Forecasts:
##          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
## May 2005       356.7701 330.8024 382.7378 317.0559 396.4842
## Jun 2005       383.6952 350.0518 417.3386 332.2420 435.1484
## Jul 2005       475.1096 427.3402 522.8790 402.0526 548.1666
## Aug 2005       422.1456 374.8352 469.4560 349.7906 494.5006
## Sep 2005       412.0505 361.5166 462.5844 334.7656 489.3354
## Oct 2005       464.8117 403.2339 526.3896 370.6366 558.9869
## Nov 2005       489.8499 420.4189 559.2808 383.6644 596.0354
## Dec 2005       596.7632 506.9344 686.5921 459.3819 734.1446
## Jan 2006       446.1817 375.2749 517.0884 337.7391 554.6242
## Feb 2006       493.6657 411.2359 576.0956 367.6002 619.7313
## Mar 2006       479.0113 395.3086 562.7139 350.9991 607.0234
## Apr 2006       423.3885 346.2252 500.5518 305.3774 541.3996
## May 2006       359.5283 291.3831 427.6736 255.3093 463.7474
## Jun 2006       386.6002 310.5842 462.6163 270.3437 502.8567
## Jul 2006       478.6323 381.2146 576.0499 329.6448 627.6197
## Aug 2006       425.2109 335.7988 514.6230 288.4668 561.9549
## Sep 2006       414.9807 324.9827 504.9786 277.3407 552.6207
## Oct 2006       468.0489 363.5183 572.5794 308.1831 627.9146
## Nov 2006       493.1910 379.9208 606.4611 319.9592 666.4228
## Dec 2006       600.7496 459.0398 742.4594 384.0232 817.4760
## Jan 2007       449.1007 340.4175 557.7839 282.8841 615.3173
## Feb 2007       496.8290 373.6073 620.0506 308.3778 685.2801
## Mar 2007       482.0174 359.6150 604.4199 294.8190 669.2158
## Apr 2007       425.9909 315.3301 536.6517 256.7497 595.2321
```

```r
#RMSE of Holt-Winters' Additive Method
summary(hwme)
```

```
## 
## Forecast method: Holt-Winters' additive method
## 
## Model Information:
## Holt-Winters' additive method 
## 
## Call:
##  hw(y = visitors, seasonal = "additive") 
## 
##   Smoothing parameters:
##     alpha = 0.4819 
##     beta  = 1e-04 
##     gamma = 0.3245 
## 
##   Initial states:
##     l = 104.4488 
##     b = 1.4956 
##     s = -16.1688 14.864 24.4611 -6.1019 90.3471 23.995
##            8.7615 -33.1694 -17.4486 10.1896 -43.028 -56.7016
## 
##   sigma:  18.65
## 
##      AIC     AICc      BIC 
## 2737.200 2739.957 2796.371 
## 
## Error measures:
##                     ME     RMSE     MAE        MPE     MAPE      MASE
## Training set 0.0515815 18.01758 13.7496 -0.1392964 5.413221 0.5077597
##                   ACF1
## Training set 0.1379352
## 
## Forecasts:
##          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
## May 2005       378.7927 354.8918 402.6936 342.2395 415.3460
## Jun 2005       414.4400 387.9072 440.9729 373.8615 455.0185
## Jul 2005       510.6476 481.7202 539.5751 466.4069 554.8883
## Aug 2005       450.9204 419.7811 482.0598 403.2969 498.5440
## Sep 2005       451.2590 418.0537 484.4642 400.4759 502.0420
## Oct 2005       499.4119 464.2611 534.5627 445.6533 553.1704
## Nov 2005       519.2091 482.2141 556.2042 462.6301 575.7882
## Dec 2005       615.7079 576.9556 654.4603 556.4413 674.9746
## Jan 2006       472.9433 432.5091 513.3775 411.1045 534.7821
## Feb 2006       508.4579 466.4084 550.5075 444.1487 572.7672
## Mar 2006       497.9718 454.3660 541.5776 431.2825 564.6611
## Apr 2006       435.5321 390.4230 480.6412 366.5437 504.5205
## May 2006       396.7569 347.6900 445.8237 321.7156 471.7981
## Jun 2006       432.4042 381.9954 482.8130 355.3106 509.4977
## Jul 2006       528.6118 476.8953 580.3283 449.5182 607.7053
## Aug 2006       468.8846 415.8920 521.8772 387.8395 549.9297
## Sep 2006       469.2231 414.9840 523.4623 386.2715 552.1748
## Oct 2006       517.3760 461.9177 572.8344 432.5598 602.1922
## Nov 2006       537.1733 480.5215 593.8251 450.5318 623.8148
## Dec 2006       633.6721 575.8509 691.4933 545.2422 722.1020
## Jan 2007       490.9075 431.9395 549.8754 400.7237 581.0912
## Feb 2007       526.4221 466.3287 586.5155 434.5172 618.3270
## Mar 2007       515.9359 454.7374 577.1345 422.3408 609.5311
## Apr 2007       453.4962 391.2116 515.7809 358.2401 548.7524
```


The model chose the same as the one that I suggested had the best RMSE from the above Holt-Winters' models. I would explain that my model also chose multiplicative error type, an additive trend, and a multiplicative seasonality. I would then explain the parameters of the Holt-Winters' method that I used to generate the forecast. 

```r
#Use ets() function to select a model automatically
visitors_ets <- ets(visitors )
print(visitors_ets)
```

```
## ETS(M,A,M) 
## 
## Call:
##  ets(y = visitors) 
## 
##   Smoothing parameters:
##     alpha = 0.6146 
##     beta  = 2e-04 
##     gamma = 0.192 
## 
##   Initial states:
##     l = 92.9631 
##     b = 2.2221 
##     s = 0.9378 1.0666 1.0669 0.9625 1.3768 1.113
##            1.0012 0.8219 0.9317 1.0046 0.8755 0.8413
## 
##   sigma:  0.0536
## 
##      AIC     AICc      BIC 
## 2603.654 2606.411 2662.825
```

```r
#Plot the decomposed data 
plot(visitors_ets)
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/18-1.png)<!-- -->


```r
#Plot forecast of model
autoplot(forecast(visitors_ets, h = 24), ylab = "Visitor nights (in millions)")
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/18-2.png)<!-- -->


```r
#Enable knit to csv
knitr::opts_chunk$set(echo = TRUE)

#Set appropriate working directory
getwd()
```

```
## [1] "C:/Users/Andrew/Desktop/Stuff for GitHub/Assignment 2"
```

```r
#setwd("C:/Users/Andrew/Desktop/Assignment 2")

#Read in packages that have already been installed
library(expsmooth)
library(forecast)
library(fpp2)
library(zoo)
library(xts)
library(ggplot2)
```


This time serious plot of the log of alcohol demand over time ranges from April 1870 to December 1938. The time series data appears to be stochastic, it does not have a strong trend and the patterns in a periodical view do not have any pattern that I would describe as seasonal. 

```r
#Import the data csv. stringsasFactors ensures the numeric values can be treated as decimals later
df <- read.csv("C:/Users/Andrew/Desktop/Stuff for GitHub/Assignment 2/alcohol-demand-log-spirits-consu.csv", stringsAsFactors = FALSE)

#Look at the first rows of the data frame
print(head(df))
```

```
##     Month Alcohol.demand..log.spirits.consumption.per.head...UK..1870.1938
## 1 1870-04                                                            1.957
## 2 1870-08                                                            1.979
## 3 1870-12                                                            2.012
## 4 1871-04                                                            2.045
## 5 1871-08                                                            2.056
## 6 1871-12                                                            2.068
```

```r
print(tail(df))
```

```
##                                                 Month
## 204                                           1937-12
## 205                                           1938-04
## 206                                           1938-08
## 207                                           1938-12
## 208 Alcohol demand (log spirits consumption per head)
## 209                                         1870-1938
##     Alcohol.demand..log.spirits.consumption.per.head...UK..1870.1938
## 204                                                            2.496
## 205                                                            2.484
## 206                                                            2.464
## 207                                                            2.458
## 208                                                               UK
## 209
```

```r
#Change column names to something easier for following code
colnames(df) <- c("Month", "logconsump")

#The last two rows should not be included in the file, they are informational and not true data
df <- df[-c(208, 209),]

#Confirm the last two rows are gone
print(tail(df))
```

```
##       Month logconsump
## 202 1937-04      2.505
## 203 1937-08      2.502
## 204 1937-12      2.496
## 205 1938-04      2.484
## 206 1938-08      2.464
## 207 1938-12      2.458
```

```r
#Convert the date column to a date object and sort from oldest to newest
df$Month = as.yearmon(df$Month)
df=df[order(df$Month),]

#Convert the log consumption into a numerical object
df$logconsump <- as.numeric(df$logconsump)

#Subset the desired data to exclude the null column
df <- subset(df, select = c("Month", "logconsump"))

#Convert the data frame into a time series object
alc <- read.zoo(df, FUN = as.yearmon)

#Plot the raw time series data
autoplot.zoo(alc, main = "Log Alcohol Consumption per Head UK 1870-1938") + xlab("Month and Year") + ylab("Log Alcohol Consumption")
```

```
## Don't know how to automatically pick scale for object of type yearmon. Defaulting to continuous.
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/20-1.png)<!-- -->


From the decomposed time series object there appears to be a weak positive trend. I still do not see any evidence of seasonality.


```r
#Decompose the series using stl() function
alc_decomp <- stl(alc, s.window = 3)

#Plot the decomposed data
autoplot(alc_decomp)
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/21-1.png)<!-- -->


Test for normality using the Jaques-Berra test as well as a qqlot.  

```r
#Read in packages that have already been installed
library(fBasics)
```

```
## Loading required package: timeDate
```

```
## Loading required package: timeSeries
```

```
## 
## Attaching package: 'timeSeries'
```

```
## The following object is masked from 'package:zoo':
## 
##     time<-
```

```
## The following objects are masked from 'package:seasonal':
## 
##     outlier, series
```

```r
#Jaques-Berra test
normalTest(alc, method = c("jb"))
```

```
## 
## Title:
##  Jarque - Bera Normalality Test
## 
## Test Results:
##   STATISTIC:
##     X-squared: 37.7296
##   P VALUE:
##     Asymptotic p Value: 6.414e-09 
## 
## Description:
##  Tue Apr 02 20:03:43 2019 by user: Andrew
```

```r
#QQ Normality plot with regression line
qqnorm(alc)
qqline(alc, col = 2)
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/22-1.png)<!-- -->


Transformation parameter increased showed better results, more points appear to have a closer fit to the mean normal distribution regression line.


```r
#QQ Normality plot with regression line for Box-Cox transformed data
qqnorm(BoxCox(alc, 4))
qqline(BoxCox(alc, 4), col = 2)
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/23-1.png)<!-- -->


This suggests that the lag is decreasing linearly as the lag number increases. The more periods out that are used to train the forecast, the less of an effect the data points will have in the data produced from the forecast.


```r
#Run acf() function to display a visual of the lag structure
acf(alc)
```

![](https://github.com/apierson3/Time-Series-and-Forecasting/blob/master/Holt_and_Holt-Winters_Forecasting_files/figure-html/24-1.png)<!-- -->


Test to determine if the autocorrelation shows significant relationships between observations. The X-squared value is twice as large when the lag is doubled. The degrees of freedom is the same number as the number of lags indicated by the function.


```r
#LJUNG-BOX test with 3 lags 
Box.test(alc, lag = 3, type = "Ljung")
```

```
## 
## 	Box-Ljung test
## 
## data:  alc
## X-squared = 555.41, df = 3, p-value < 2.2e-16
```

```r
#LJUNG-BOX test with 6 lags 
Box.test(alc, lag = 6, type = "Ljung")
```

```
## 
## 	Box-Ljung test
## 
## data:  alc
## X-squared = 1000.8, df = 6, p-value < 2.2e-16
```
