import matplotlib.pyplot as plt
import pandas as pd
import statsmodels.api as sm
from statsmodels.nonparametric.smoothers_lowess import lowess
import numpy as np
import scipy.stats as stats


def Rplot1(fitted_model, resids = None, fits = None, ax = None):
    if isinstance(resids,type(None)):
        resids = fitted_model.resid
    if isinstance(fits,type(None)):
        fits = fitted_model.fittedvalues
    top3 = abs(resids).sort_values(ascending = False)[:3]
    smoothed = lowess(resids,fits)
    if isinstance(ax,type(None)):
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
    
def Rplot2(fitted_model, norm_resid = None, ax = None):
    if isinstance(norm_resid,type(None)):
        norm_resid = fitted_model.get_influence().resid_studentized_internal
        sorted_norm_resid = pd.Series(norm_resid)\
                                .sort_values(ascending = False)
    else:
        sorted_norm_resid = pd.Series(norm_resid)\
                                .sort_values(ascending = False)
    top3 = pd.Series(abs(norm_resid)).sort_values(ascending = False)[:3]
    temp = sorted_norm_resid.copy().reset_index()
    sortedidx = temp[temp.iloc[:,1].isin(top3)].index
    theoretical_quantiles = pd.Series(\
                               stats.probplot(sorted_norm_resid, \
                                              dist = 'norm', fit = False)[0])\
                                              .sort_values(ascending = False)
    if isinstance(ax,type(None)):
        fig, ax = plt.subplots()
    x = theoretical_quantiles
    y = sorted_norm_resid
    ax.scatter(x,y, edgecolor = 'k',facecolor = 'none')
    ax.set_title('Normal Q-Q')
    ax.set_ylabel('Standardized Residuals')
    ax.set_xlabel('Theoretical Quantiles')
    ax.plot([np.min([x,y]),np.max([x,y])],[np.min([x,y]),np.max([x,y])], color = 'r', ls = '--')
    for i, val in enumerate(top3.index):
        ax.annotate(val,xy=(theoretical_quantiles.iloc[sortedidx[i]],\
                            sorted_norm_resid[val]))
    return(ax)
   
def Rplot3(fitted_model, sqrt_norm_resid = None, fits = None, ax = None):
    if isinstance(sqrt_norm_resid,type(None)):
        norm_resid = fitted_model.get_influence().resid_studentized_internal
        sqrt_norm_resid = np.sqrt(np.abs(norm_resid))
    if isinstance(fits,type(None)):
        fits = fitted_model.fittedvalues
    top3 = abs(pd.Series(sqrt_norm_resid)).sort_values(ascending = False)[:3]
    smoothed = lowess(sqrt_norm_resid,fits)
    if isinstance(ax,type(None)):
        fig, ax = plt.subplots()
    ax.scatter(fits, sqrt_norm_resid, edgecolors = 'k', facecolors = 'none')
    ax.plot(smoothed[:,0],smoothed[:,1],color = 'r')
    ax.set_ylabel('$\sqrt{|Studentized Residuals|}$')
    ax.set_xlabel('Fitted Values')
    ax.set_title('Scale-Location')
    for i in top3.index:
        ax.annotate(i,xy=(fits[i],sqrt_norm_resid[i]))
    return(ax)
    
def Rplot4(fitted_model, norm_resid = None, levrge = None, ax = None): 
    if isinstance(norm_resid,type(None)):
        norm_resid = fitted_model.get_influence().resid_studentized_internal
    if isinstance(levrge,type(None)): 
        levrge = fitted_model.get_influence().hat_matrix_diag
    top3 = abs(pd.Series(levrge)).sort_values(ascending = False)[:3]
    smoothed = lowess(norm_resid,levrge)
    if isinstance(ax,type(None)):
        fig, ax = plt.subplots()
    ax.scatter(levrge, norm_resid, edgecolors = 'k', facecolors = 'none')
    ax.plot(smoothed[:,0],smoothed[:,1],color = 'r')
    ax.set_ylabel('Studentized Residuals')
    ax.set_xlabel('Leverage')
    ax.set_title('Residuals vs. Leverage')
    ax.set_ylim(min(norm_resid)-0.1,max(norm_resid)+0.1)

    cooksx = np.linspace(min(levrge), max(levrge), 50)
    p = len(fitted_model.params)
    poscooks1y = np.sqrt((p*(1-cooksx)**2)/cooksx)
    poscooks05y = np.sqrt(0.5*(p*(1-cooksx)**2)/cooksx)
    negcooks1y = -np.sqrt((p*(1-cooksx)**2)/cooksx)
    negcooks05y = -np.sqrt(0.5*(p*(1-cooksx)**2)/cooksx)
    
    ax.plot(cooksx,poscooks1y,label = "Cook's Distance", ls = ':', color = 'r')
    ax.plot(cooksx,poscooks05y, ls = ':', color = 'r')
    ax.plot(cooksx,negcooks1y, ls = ':', color = 'r')
    ax.plot(cooksx,negcooks05y, ls = ':', color = 'r')
    ax.legend()
    for i in top3.index:
        ax.annotate(i,xy=(levrge[i],norm_resid[i]))
    return(ax)
    
def allplots(fitted_model):
    fig, axs = plt.subplots(2,2)
    Rplot1(fitted_model,ax = axs[0,0])
    Rplot2(fitted_model,ax = axs[0,1])
    Rplot3(fitted_model,ax = axs[1,0])
    Rplot4(fitted_model,ax = axs[1,1])
    fig.tight_layout()
    return(axs)
    