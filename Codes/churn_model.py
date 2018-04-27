# -*- coding: utf-8 -*-
"""
Created on Wed Apr 11 21:42:03 2018

@author: nirmi
"""

from __future__ import division
import datetime

import pandas as pd
import numpy as np
import xgboost as xgb
import os
import lightgbm as lgb
from sklearn import svm
import pickle
import calendar
import datetime
from sklearn.linear_model import LogisticRegression
import matplotlib.pyplot as plt
from xgboost import plot_importance
from sklearn.ensemble import RandomForestClassifier

from sklearn.preprocessing import StandardScaler, normalize, MinMaxScaler
from sklearn.cross_validation import train_test_split
from sklearn.metrics import accuracy_score
from sklearn.metrics import confusion_matrix
from sklearn.metrics import roc_curve
from sklearn import metrics
from sklearn.model_selection import cross_val_score, GridSearchCV, cross_validate, train_test_split

import pickle
from sklearn.externals import joblib


os.chdir('E:\\mgt_6203\\project\\data')


def normalize(data):
    for i in range(3, data.shape[1]):
        data.iloc[:, i] = np.log(data.iloc[:, i] + 0.00001)
    return data

# data_parent = pd.read_csv('scvo2_features_data_filtered.csv')
data_parent = pd.read_csv('final_data_use.csv')

#data_parent = pd.read_csv('results.csv')

imp_vars = ['total_sales', 'overall_weekly_purchase', 'order_num', 'three_weeks_sales', 'three_weeks_disc', 'three_weeks_freq', 'five_weeks_sales','five_weeks_disc','five_weeks_freq']

#datax = data_parent[['total_sales','total_disc','num_orders','total_qty','overall_weekly_purchase','frac_share','overall_market_share','row_id','three_weeks_sales','three_weeks_disc','three_weeks_freq','five_weeks_sales','five_weeks_disc','five_weeks_freq','weeklag']]
datax = data_parent[imp_vars]
# datax = normalize(datax)

#datax = data_parent[['tissue.extraction','temp','ph','hb','lactate','tissue.extraction-SMA','tissue.extraction-momentum','temp-SMA','temp-momentum','ph-SMA','ph-Momentum','hb-SMA','hb-momentum','lactate-SMA','lactate-momentum']]

datay = data_parent[['if_churn']]
X_train, X_test, y_train, y_test = train_test_split(datax, datay, test_size=0.20, random_state=42)

scaler = StandardScaler().fit(X_train)
X_train = scaler.transform(X_train)
X_test = scaler.transform(X_test)
joblib.dump(scaler, "scaler.save")


# xg-boost model
xgbmodel = xgb.XGBClassifier(silent=False, max_depth= 10, learning_rate=0.1, scale_pos_weight = 1)
xgbmodel.fit(X_train, np.ravel(y_train.iloc[:,0]), eval_metric = 'auc')
y_pred = pd.DataFrame(xgbmodel.predict(X_test))
accuracy_score(np.ravel(y_test), y_pred)
tn, fp, fn, tp = confusion_matrix(y_test, y_pred).ravel()
recall = tp/(tp+fn)
precision = tp/(tp+fp)
f_score = 2 * precision * recall / (precision + recall)
fpr, tpr, thresholds = metrics.roc_curve(y_test, y_pred, pos_label=1)
auc_xgb = metrics.auc(fpr, tpr)

# grid search cross validation try XGB
param_xgb = { 
    'scale_pos_weight': [1, 4, 7, 10],
    'max_depth': [2, 5, 10, 15]
}
cv_xgb_model = GridSearchCV(estimator = xgbmodel, param_grid = param_xgb, scoring='roc_auc', cv = 5)
cv_xgb_model.fit(X_train, y_train)
cv_xgb_model.best_params_
cv_xgb_model.best_score_

xgb_cv_pred = pd.DataFrame(cv_xgb_model.predict(X_test))
accuracy_score(np.ravel(y_test), xgb_cv_pred)
tn_cv, fp_cv, fn_cv, tp_cv = confusion_matrix(y_test, xgb_cv_pred).ravel()
recall_cv = tp_cv/(tp_cv+fn_cv)
precision_cv = tp_cv/(tp_cv+fp_cv)
fscore_xgb_cv = 2*recall_cv*precision_cv/(precision_cv + recall_cv)
fpr_cv, tpr_cv, thresholds_cv = metrics.roc_curve(y_test, xgb_cv_pred, pos_label=1)
auc_xgb_cv = metrics.auc(fpr_cv, tpr_cv)

pickle.dump(cv_xgb_model, open("xgboost.dat", "wb"))




# logistic regression

logisticRegr = LogisticRegression()
logisticRegr.fit(X_train, y_train)
lr_output = logisticRegr.predict(X_test)
accuracy_score(y_test, lr_output)
tn_lr, fp_lr, fn_lr, tp_lr = confusion_matrix(y_test, lr_output).ravel()
recall_lr = tp_lr/(tp_lr+fn_lr)
precision_lr = tp_lr/(tp_lr+fp_lr)
f_score_lr = 2 * precision_lr * recall_lr / (precision_lr + recall_lr)
fpr_lr, tpr_lr, thresholds_lr = metrics.roc_curve(y_test, lr_output, pos_label=1)
auc_lr = metrics.auc(fpr_lr, tpr_lr)

# grid search cross validation Logistic Regression

param_lr = { 
    'C': [0.001, 0.01, 1],
    'penalty': ['l1', 'l2'], 
    'class_weight': ['balanced']
}
cv_lr_model = GridSearchCV(estimator = logisticRegr, param_grid = param_lr, scoring='roc_auc', cv = 5)
cv_lr_model.fit(X_train, y_train)
cv_lr_model.best_params_
cv_lr_model.best_score_

lr_cv_pred = pd.DataFrame(cv_lr_model.predict(X_test))
accuracy_score(np.ravel(y_test), lr_cv_pred)
tn_cv_lr, fp_cv_lr, fn_cv_lr, tp_cv_lr = confusion_matrix(y_test, lr_cv_pred).ravel()
recall_cv_lr = tp_cv_lr/(tp_cv_lr+fn_cv_lr)
precision_cv_lr = tp_cv_lr / (tp_cv_lr + fp_cv_lr)
fscore_lr_cv = 2 * recall_cv_lr * precision_cv_lr/(precision_cv_lr + recall_cv_lr)
fpr_cv_lr, tpr_cv_lr, thresholds_cv_lr = metrics.roc_curve(y_test, lr_cv_pred, pos_label=1)
auc_lr_cv = metrics.auc(fpr_cv_lr, tpr_cv_lr)

pickle.dump(cv_lr_model, open("lr.dat", "wb"))



# random forest
rfmodel = RandomForestClassifier(n_jobs=2, random_state=0)
rfmodel.fit(X_train, y_train)
rf_pred = rfmodel.predict(X_test)
accuracy_score(y_test, rf_pred)
tn_rf, fp_rf, fn_rf, tp_rf = confusion_matrix(y_test, rf_pred).ravel()
recall_rf = tp_rf/(tp_rf+fn_rf)
precision_rf = tp_rf/(tp_rf+fp_rf)
f_score_rf = 2 * precision_rf * recall_rf / (precision_rf + recall_rf)
fpr_rf, tpr_rf, thresholds_rf = metrics.roc_curve(y_test, rf_pred, pos_label=1)
auc_rf = metrics.auc(fpr_rf, tpr_rf)

# rf cross validation
param_rf = { 
    'n_estimators': [20, 50, 100],
    'max_depth': [10, 20, 50], 
    'class_weight' : ['balanced']
}
cv_rf_model = GridSearchCV(estimator = rfmodel, param_grid = param_rf, scoring='roc_auc', cv = 10)
cv_rf_model.fit(X_train, y_train)
cv_rf_model.best_params_
cv_rf_model.best_score_

rf_cv_pred = pd.DataFrame(cv_rf_model.predict(X_test))
accuracy_score(np.ravel(y_test), rf_cv_pred)
tn_cv_rf, fp_cv_rf, fn_cv_rf, tp_cv_rf = confusion_matrix(y_test, rf_cv_pred).ravel()
recall_cv_rf = tp_cv_rf/(tp_cv_rf+fn_cv_rf)
precision_cv_rf = tp_cv_rf / (tp_cv_rf + fp_cv_rf)
fscore_rf_cv = 2*recall_cv_rf*precision_cv_rf/(precision_cv_rf + recall_cv_rf)
fpr_cv_rf, tpr_cv_rf, thresholds_cv_rf = metrics.roc_curve(y_test, rf_cv_pred, pos_label=1)
auc_rf_cv = metrics.auc(fpr_cv_rf, tpr_cv_rf)

pickle.dump(cv_rf_model, open("randomforest.dat", "wb"))

## svm

svmmodel = svm.SVC(C=1, cache_size=200, class_weight=None, coef0=0.0,
    decision_function_shape='ovr', degree=3, gamma='auto', kernel='rbf',
    max_iter=-1, probability=True, random_state=None, shrinking=True,
    tol=0.001, verbose=False)
svmmodel.fit(X_train, y_train)
svm_test_output = svmmodel.predict(X_test)
accuracy_score(y_test, svm_test_output)

tn_svm, fp_svm, fn_svm, tp_svm = confusion_matrix(y_test, svm_test_output).ravel()

recall_svm = tp_svm/(tp_svm+fn_svm)
precision_svm = tp_svm/(tp_svm+fp_svm)
fscore_svm = 2 * recall_svm * precision_svm / (recall_svm + precision_svm)
fpr_svm, tpr_svm, thresholds_svm = metrics.roc_curve(y_test, svm_test_output, pos_label=1)
auc_svm = metrics.auc(fpr_svm, tpr_svm)

## grid search cross validation svm

param_svm = { 
    'C': [0.01, 1, 100],
    'kernel': ['rbf'], 
    'class_weight' : ['balanced']
}
cv_svm_model = GridSearchCV(estimator = svmmodel, param_grid = param_svm, scoring='roc_auc', cv = 5)
cv_svm_model.fit(X_train, y_train)
cv_svm_model.best_params_
cv_svm_model.best_score_

svm_cv_pred = pd.DataFrame(cv_svm_model.predict(X_test))
accuracy_score(np.ravel(y_test), svm_cv_pred)
tn_cv_svm, fp_cv_svm, fn_cv_svm, tp_cv_svm = confusion_matrix(y_test, svm_cv_pred).ravel()
recall_cv_svm = tp_cv_svm/(tp_cv_svm+fn_cv_svm)
precision_cv_svm = tp_cv_svm/(tp_cv_svm+fp_cv_svm)
fscore_svm_cv = 2*recall_cv_svm*precision_cv_svm/(precision_cv_svm + recall_cv_svm)
fpr_cv_svm, tpr_cv_svm, thresholds_cv_svm = metrics.roc_curve(y_test, svm_cv_pred, pos_label=1)
auc_svm_cv = metrics.auc(fpr_cv_svm, tpr_cv_svm)



pickle.dump(cv_svm_model, open("svm.dat", "wb"))


# ROC Curve Plot

plt.figure(0).clf()
plt.title('ROC Curves')
plt.plot(fpr_cv, tpr_cv, label="XGB, auc="+str(round(auc_xgb_cv, 2)))
plt.plot(fpr_cv_rf , tpr_cv_rf , label="RF, auc="+str(round(auc_rf_cv, 2)))
plt.plot(fpr_cv_svm, tpr_cv_svm, label="SVM, auc="+str(round(auc_svm_cv, 2)))
plt.plot(fpr_cv_lr, tpr_cv_lr, label="LR, auc="+str(round(auc_lr_cv, 2)))

plt.legend(loc = 'lower right')
plt.plot([0, 1], [0, 1],'r--')
plt.xlim([0, 1])
plt.ylim([0, 1])
plt.ylabel('True Positive Rate')
plt.xlabel('False Positive Rate')
