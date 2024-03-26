import pandas as pd
import numpy as np
import gc
from autogluon.timeseries import TimeSeriesDataFrame, TimeSeriesPredictor

r.datos_train.head()
r.datos_train.isnull().values.any()

## train_simple
datos_train_simple = r.datos_train.drop(["ipc", "ibr", "trm"], axis=1)
datos_train_simple

ts_train_simple = TimeSeriesDataFrame.from_data_frame(
    datos_train_simple,
    id_column="cod",
    timestamp_column="fecha_corte"
)
ts_train_simple

predictor_ts_train_simple = TimeSeriesPredictor(
    prediction_length=28,
    path="modelos/autogluon_ts_train_simple",
    target="creci_dia",
    #eval_metric="RMSE",
    quantile_levels = [0.05, 0.25, 0.75, 0.95],
    freq = "D",
    known_covariates_names=["dia_no_habil"]
)

predictor_ts_train_simple.fit(
    ts_train_simple,
    presets="best_quality",
    time_limit=7200,
    num_val_windows = 13,
    random_seed =4981
)

predictor_ts_train_simple.leaderboard()


r.datos_test
datos_comp = TimeSeriesDataFrame.from_data_frame(
    r.datos_test,
    id_column="cod",
    timestamp_column="fecha_corte")
datos_comp

test_data_ts_simple = r.datos_test.drop(["creci_dia","trm", "ipc", "ibr"], axis=1)
test_data_ts_simple


ts_test_simple = TimeSeriesDataFrame.from_data_frame(
    test_data_ts_simple,
    id_column="cod",
    timestamp_column="fecha_corte")

ts_test_simple
    

predictions_test_simple = predictor_ts_train_simple.predict(ts_train_simple, known_covariates=ts_test_simple)
predictions_test_simple

def rmse(predictions, targets):
    return np.sqrt(((predictions - targets) ** 2).mean())

rmse(predictions_test_simple["mean"].values, datos_comp["creci_dia"].values)


## train_ibr
datos_train_ibr = r.datos_train.drop(["ipc", "trm"], axis=1)
datos_train_ibr

ts_train_ibr = TimeSeriesDataFrame.from_data_frame(
    datos_train_ibr,
    id_column="cod",
    timestamp_column="fecha_corte"
)
ts_train_ibr

predictor_ts_train_ibr = TimeSeriesPredictor(
    prediction_length=28,
    path="modelos/autogluon_ts_train_ibr",
    target="creci_dia",
    #eval_metric="RMSE",
    quantile_levels = [0.05, 0.25, 0.75, 0.95],
    freq = "D",
    known_covariates_names=["dia_no_habil", "ibr"]
)

predictor_ts_train_ibr.fit(
    ts_train_ibr,
    presets="best_quality",
    time_limit=7200,
    num_val_windows = 13,
    random_seed =4981
)

predictor_ts_train_ibr.leaderboard()

test_data_ts_ibr = r.datos_test.drop(["creci_dia","trm", "ipc"], axis=1)
test_data_ts_ibr


ts_test_ibr = TimeSeriesDataFrame.from_data_frame(
    test_data_ts_ibr,
    id_column="cod",
    timestamp_column="fecha_corte")

ts_test_ibr
    

predictions_test_ibr = predictor_ts_train_ibr.predict(ts_train_ibr, known_covariates=ts_test_ibr)
predictions_test_ibr

rmse(predictions_test_simple["mean"].values, datos_comp["creci_dia"].values)
rmse(predictions_test_ibr["mean"].values, datos_comp["creci_dia"].values)

## train_ipc
datos_train_ipc = r.datos_train.drop(["ibr", "trm"], axis=1)
datos_train_ipc

ts_train_ipc = TimeSeriesDataFrame.from_data_frame(
    datos_train_ipc,
    id_column="cod",
    timestamp_column="fecha_corte"
)
ts_train_ipc

predictor_ts_train_ipc = TimeSeriesPredictor(
    prediction_length=28,
    path="modelos/autogluon_ts_train_ipc",
    target="creci_dia",
    #eval_metric="RMSE",
    quantile_levels = [0.05, 0.25, 0.75, 0.95],
    freq = "D",
    known_covariates_names=["dia_no_habil", "ipc"]
)

predictor_ts_train_ipc.fit(
    ts_train_ipc,
    presets="best_quality",
    time_limit=7200,
    num_val_windows = 13,
    random_seed =4981
)

predictor_ts_train_ipc.leaderboard()

test_data_ts_ipc = r.datos_test.drop(["creci_dia","trm", "ibr"], axis=1)
test_data_ts_ipc


ts_test_ipc = TimeSeriesDataFrame.from_data_frame(
    test_data_ts_ipc,
    id_column="cod",
    timestamp_column="fecha_corte")

ts_test_ipc
    

predictions_test_ipc = predictor_ts_train_ipc.predict(ts_train_ipc, known_covariates=ts_test_ipc)
predictions_test_ipc

rmse(predictions_test_simple["mean"].values, datos_comp["creci_dia"].values)
rmse(predictions_test_ibr["mean"].values, datos_comp["creci_dia"].values)
rmse(predictions_test_ipc["mean"].values, datos_comp["creci_dia"].values)

## train_trm
datos_train_trm = r.datos_train.drop(["ibr", "ipc"], axis=1)
datos_train_trm

ts_train_trm = TimeSeriesDataFrame.from_data_frame(
    datos_train_trm,
    id_column="cod",
    timestamp_column="fecha_corte"
)
ts_train_trm

predictor_ts_train_trm = TimeSeriesPredictor(
    prediction_length=28,
    path="modelos/autogluon_ts_train_trm",
    target="creci_dia",
    #eval_metric="RMSE",
    quantile_levels = [0.05, 0.25, 0.75, 0.95],
    freq = "D",
    known_covariates_names=["dia_no_habil", "trm"]
)

predictor_ts_train_trm.fit(
    ts_train_trm,
    presets="best_quality",
    time_limit=7200,
    num_val_windows = 13,
    random_seed =4981
)

predictor_ts_train_trm.leaderboard()

test_data_ts_trm = r.datos_test.drop(["creci_dia","ipc", "ibr"], axis=1)
test_data_ts_trm


ts_test_trm = TimeSeriesDataFrame.from_data_frame(
    test_data_ts_trm,
    id_column="cod",
    timestamp_column="fecha_corte")

ts_test_trm
    

predictions_test_trm = predictor_ts_train_trm.predict(ts_train_trm, known_covariates=ts_test_trm)
predictions_test_trm

rmse(predictions_test_simple["mean"].values, datos_comp["creci_dia"].values)
rmse(predictions_test_ibr["mean"].values, datos_comp["creci_dia"].values)
rmse(predictions_test_ipc["mean"].values, datos_comp["creci_dia"].values)
rmse(predictions_test_trm["mean"].values, datos_comp["creci_dia"].values)

#######################################################################
import matplotlib.pyplot as plt

plt.figure(figsize=(20, 3))

item_id = "5_16_1_10936"
y_past = ts_train_simple.loc[item_id]["creci_dia"]
y_pred = predictions_test_trm.loc[item_id]
y_test = datos_comp.loc[item_id]["creci_dia"]

plt.plot(y_past[-200:], label="Past time series values")
plt.plot(y_pred["mean"], label="Mean forecast")
plt.plot(y_test, label="Future time series values")

plt.fill_between(
    y_pred.index, y_pred["0.05"], y_pred["0.95"], color="red", alpha=0.1, label=f"5%-95% confidence interval"
)
plt.fill_between(
    y_pred.index, y_pred["0.25"], y_pred["0.75"], color="blue", alpha=0.1, label=f"25%-75% confidence interval"
)
plt.legend()
plt.show()

gc.collect()

############
predictions_full = predictions_test_trm.reset_index()
predictions_full.head()

## dato completos ----

r.datos_modelar.head()
r.datos_modelar.isnull().values.any()

train_data = TimeSeriesDataFrame.from_data_frame(
    r.datos_modelar.drop(["ipc", "ibr"], axis=1),
    id_column="cod",
    timestamp_column="fecha_corte"
)

train_data

predictor = TimeSeriesPredictor(
    prediction_length=28,
    path="modelos/autogluon_full",
    target="creci_dia",
    #eval_metric="RMSE",
    quantile_levels = [0.05,0.25,0.75,0.95],
    freq = "D",
    known_covariates_names=["dia_no_habil", "trm"]
)

predictor.fit(
    train_data,
    presets="best_quality",
    time_limit=7200,
    num_val_windows = 13,
    random_seed =4981
)

predictor = TimeSeriesPredictor.load("modelos/autogluon_full")

predictor.leaderboard()

r.datos_forecast.head()
r.datos_forecast.isnull().values.any()
test_data = r.datos_forecast.drop(["creci_dia", "ipc", "ibr"], axis=1)
test_data

datos_test = TimeSeriesDataFrame.from_data_frame(
    test_data,
    id_column="cod",
    timestamp_column="fecha_corte")

datos_comp = TimeSeriesDataFrame.from_data_frame(
    r.datos_forecast,
    id_column="cod",
    timestamp_column="fecha_corte")

datos_test

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
    y_pred.index, y_pred["0.05"], y_pred["0.95"], color="red", alpha=0.1, label=f"5%-95% confidence interval"
)
plt.fill_between(
    y_pred.index, y_pred["0.25"], y_pred["0.75"], color="blue", alpha=0.1, label=f"25%-75% confidence interval"
)
plt.legend()
plt.show()

predictions_full = predictions.reset_index()
predictions_full.head()



