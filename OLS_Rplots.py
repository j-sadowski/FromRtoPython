import matplotlib.pyplot as plt
import pandas as pd
import statsmodels.api as sm
from statsmodels.nonparametric.smoothers_lowess import lowess
import numpy as np


def Rplot1(fitted_model, resids = pd.Series([]), fits = pd.Series([])):
    if resids.empty:
        resids = fitted_model.resid
    if fits.empty:
        fits = fitted_model.fittedvalues
    smoothed = lowess(resids,fits)
    plt.rcParams.update({'font.size': 16})
    plt.rcParams["figure.figsize"] = (8,7)
    fig, ax = plt.subplots()
    ax.scatter(fits, resids, edgecolors = 'k', facecolors = 'none')
    ax.plot(smoothed[:,0],smoothed[:,1],color = 'r')
    ax.set_ylabel('Residuals')
    ax.set_xlabel('Fitted Values')
    ax.set_title('Residuals vs. Fitted')
    ax.plot([min(fits),max(fits)],[0,0],color = 'k',linestyle = ':')
    return(ax)
    
def Rplot2(resids):
    plt.rcParams.update({'font.size': 16})
    plt.rcParams["figure.figsize"] = (8,7)
    fig, ax = plt.subplots()
    sm.qqplot(resids, line = 's', ax = ax, color = 'k')
    ax.set_title('Normal QQ Plot of Residuals')
    return(ax)
   
def Rplot3(fitted_model, sqrt_norm_resid = pd.Series([]), fits = pd.Series([])):
    if sqrt_norm_resid.empty:
        norm_resid = fitted_model.get_influence().resid_studentized_internal
        sqrt_norm_resid = np.sqrt(np.abs(norm_resid))
    if fits.empty:
        fits = fitted_model.fittedvalues
    smoothed = lowess(sqrt_norm_resid,fits)
    plt.rcParams.update({'font.size': 16})
    plt.rcParams["figure.figsize"] = (8,7)
    fig, ax = plt.subplots()
    ax.scatter(fitted, sqrt_norm_resid, edgecolors = 'k', facecolors = 'none')
    ax.plot(smoothed[:,0],smoothed[:,1],color = 'r')
    ax.set_ylabel('Sqrt Residuals')
    ax.set_xlabel('Fitted Values')
    ax.set_title('Scale-Location')
    return(ax)
    
def Rplot4(fitted_model, norm_resids = pd.Series([]), levrge = pd.Series([])): 
    if norm_resids.empty:
        norm_resid = fitted_model.get_influence().resid_studentized_internal
    if levrge.empty: 
        levrge = results.get_influence().hat_matrix_diag
    smoothed = lowess(norm_residuals,levrge)
    plt.rcParams.update({'font.size': 16})
    plt.rcParams["figure.figsize"] = (8,7)
    fig, ax = plt.subplots()
    ax.scatter(levrge, norm_resid, edgecolors = 'k', facecolors = 'none')
    ax.plot(smoothed[:,0],smoothed[:,1],color = 'r')
    ax.set_ylabel('Studentized Residuals')
    ax.set_xlabel('Leverage')
    ax.set_title('Residuals vs. Leverage')
    return(ax)