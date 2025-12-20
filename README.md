# **📊 Pharmaceutical Sales Forecasting and Planning**
🔍 Project Overview

This project focuses on analyzing and forecasting pharmaceutical sales using historical daily sales data from a retail pharmacy.
The objective is to build reliable forecasting models that can support inventory planning, purchasing decisions, and operational strategy in the pharmaceutical sector.

The analysis includes data cleaning, exploratory data analysis (EDA), time-series modeling, and the generation of daily, weekly, and monthly sales forecasts.

# 📁 Dataset

The dataset is sourced from **Kaggle** and contains daily pharmaceutical sales aggregated across selected drug categories classified under the **Anatomical Therapeutic Chemical (ATC) system.**

* Time span: 2014–2019 (historical data)

* Frequency: Daily

* Target variable: **Total daily sales**

Drug categories:

* **M01AB** – Anti-inflammatory (Acetic acid derivatives)

* **M01AE** – Anti-inflammatory (Propionic acid derivatives)

* **N02BA** – Analgesics (Salicylic acid derivatives)

* **N02BE** – Analgesics (Pyrazolones & Anilides)

* **N05B** – Anxiolytics

* **N05C** – Hypnotics & sedatives

* **R03** – Drugs for obstructive airway diseases

* **R06** – Antihistamines

Dataset link:
🔗 https://www.kaggle.com/datasets/milanzdravkovic/pharma-sales-data

# 🧹 Data Preparation & Exploration

Key preprocessing steps include:

* Removing incorrectly generated columns

* Creating a unified **date variable**

* Aggregating drug-level sales into **total daily sales**

* Identifying **zero-sales days** (pharmacy closure)

* Engineering time-based features (lags, rolling means, weekdays, months)

Exploratory analysis revealed:

* Strong **weekly seasonality**

* Clear **monthly patterns**

* Stationary behavior confirmed using **ADF and KPSS tests**

# 🤖 Modeling Approach

Multiple forecasting models were implemented and compared:

* **Elastic Net Regression** with lag and rolling features

* **ARIMA** (baseline time-series model)

* **ARIMAX** with calendar-based regressors

* **Prophet** with weekly/yearly seasonality and closure indicators

* **Ensemble model** combining ARIMAX and Prophet forecasts

Model performance was evaluated using:

* RMSE

* MAE

* MAPE

# 📈 Results

* **Elastic Net** achieved the best short-term accuracy

* **Prophet** showed strong robustness for longer horizons

* **Ensemble** forecasting reduced model-specific bias

* **Plain ARIMA** performed weakest due to lack of exogenous information

Forecasts were generated at:

* **Daily level** (operational planning)

* **Weekly level** (procurement planning)

* **Monthly level** (strategic planning)

# 📄 Report

The full analytical report (including methodology, results, and business insights) is available as a PDF:

📎 [Download the full report](https://raw.githubusercontent.com/ahmedm3laa/pharma-sales-ml-project/main/Reports/Pharmaceutical%20Sales%20Forecasting.pdf)


# 🛠 Tools & Libraries

* R, tidyverse, ggplot2

* forecast, prophet, glmnet

* lubridate, zoo, tseries, caret

# 🚀 Future Work

* Category-level forecasting (Part 2)

* Holiday calendars and external demand drivers

* Probabilistic forecasting and uncertainty intervals

* Deployment as a decision-support dashboard

# 👤 Author
## [Ahmed M. Alaa](https://www.linkedin.com/in/ahmedm3laa/)

