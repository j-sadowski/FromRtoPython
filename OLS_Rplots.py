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
    top3 = abs(resids).sort_values(ascending = False)[:3]
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
    for i in top3.index:
        ax.annotate(i, xy = (fits[i],resids[i]))
    return(ax)
    
def Rplot2(fitted_model):
    resids = fitted_model.resid
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
    top3 = abs(pd.Series(sqrt_norm_resid)).sort_values(ascending = False)[:3]
    smoothed = lowess(sqrt_norm_resid,fits)
    plt.rcParams.update({'font.size': 16})
    plt.rcParams["figure.figsize"] = (8,7)
    fig, ax = plt.subplots()
    ax.scatter(fits, sqrt_norm_resid, edgecolors = 'k', facecolors = 'none')
    ax.plot(smoothed[:,0],smoothed[:,1],color = 'r')
    ax.set_ylabel('Sqrt Residuals')
    ax.set_xlabel('Fitted Values')
    ax.set_title('Scale-Location')
    for i in top3.index:
        ax.annotate(i,xy=(fits[i],sqrt_norm_resid[i]))
    return(ax)
    
def Rplot4(fitted_model, norm_resid = pd.Series([]), levrge = pd.Series([])): 
    if norm_resid.empty:
        norm_resid = fitted_model.get_influence().resid_studentized_internal
    if levrge.empty: 
        levrge = fitted_model.get_influence().hat_matrix_diag
    top3 = abs(pd.Series(norm_resid)).sort_values(ascending = False)[:3]
    smoothed = lowess(norm_resid,levrge)
    plt.rcParams.update({'font.size': 16})
    plt.rcParams["figure.figsize"] = (8,7)
    fig, ax = plt.subplots()
    ax.scatter(levrge, norm_resid, edgecolors = 'k', facecolors = 'none')
    ax.plot(smoothed[:,0],smoothed[:,1],color = 'r')
    ax.set_ylabel('Studentized Residuals')
    ax.set_xlabel('Leverage')
    ax.set_title('Residuals vs. Leverage')
    for i in top3.index:
        ax.annotate(i,xy=(levrge[i],norm_resid[i]))
    return(ax)