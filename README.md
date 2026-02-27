#  TGV Express Revenue Management

Unconstrained historical booking data affected by inventory closure 
and built a period-to-period growth rate forecasting model across 
15 booking windows (180 to 1 day before departure).

##  Business Problem
Train booking data is distorted when inventory closes early — 
actual demand is higher than recorded. Without unconstraining, 
forecasts systematically underestimate true demand.

##  What This Analysis Does
- Detects constrained bookings (capacity hit or frozen values)
- Applies unconstraining algorithm using historical growth rates
- Forecasts future bookings across 15 time windows (180→1 days prior)
- Visualizes historical vs forecasted booking curves

## 📊 Technical Stack
R, tidyverse, ggplot2, readxl

## 🗂️ Files
| File | Data |
|------|-------------|
| `41107_Forecast.R` | TGV_Express_Forecast-Assign2.xls |
