import pandas as pd
import os
from autogluon.timeseries import TimeSeriesDataFrame, TimeSeriesPredictor
r.datos_train.head()
r.datos_train.isnull().values.any()

train_data = TimeSeriesDataFrame.from_data_frame(
    r.datos_train,
    id_column="cod",
    timestamp_column="fecha_corte"
)

train_data.tail()
train_data.isnull().any()

predictor = TimeSeriesPredictor(
    prediction_length=30,
    path="modelos/autogluon_m3",
    target="creci_dia",
    eval_metric="RMSE",
    quantile_levels = [0.05,0.5,0.95],
    freq = "D",
    known_covariates_names=["ipc_lag", "ibr", "trm_lag"]
)

predictor.fit(
    train_data,
    presets="best_quality",
    time_limit=3600,
    num_val_windows = 1,
    random_seed =4981,
    refit_full = True
)

r.datos_test.head()
r.datos_test.isnull().values.any()
test_data = r.datos_test.drop("creci_dia", axis=1)
test_data.head()

datos_test = TimeSeriesDataFrame.from_data_frame(
    test_data,
    id_column="cod",
    timestamp_column="fecha_corte")

datos_comp = TimeSeriesDataFrame.from_data_frame(
    r.datos_test,
    id_column="cod",
    timestamp_column="fecha_corte")

datos_test.tail()

predictions = predictor.predict(train_data, known_covariates=datos_test)
predictions.head()

import matplotlib.pyplot as plt

plt.figure(figsize=(20, 3))

item_id = "5_16_1_10936"
y_past = train_data.loc[item_id]["creci_dia"]
y_pred = predictions.loc[item_id]
y_test = datos_comp.loc[item_id]["creci_dia"]

plt.plot(y_past[-200:], label="Past time series values")
plt.plot(y_pred["mean"], label="Mean forecast")
plt.plot(y_test, label="Future time series values")

plt.fill_between(
    y_pred.index, y_pred["0.05"], y_pred["0.95"], color="red", alpha=0.1, label=f"10%-90% confidence interval"
)
plt.legend()
plt.show()

predictions_full = predictions.reset_index()
predictions_full.head()




