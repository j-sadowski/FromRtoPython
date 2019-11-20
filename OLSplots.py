"""
A module to construct the base R plot(lm_object) linear regression plots.

By: Jason Sadowski
Date: 2019-11-19
"""

import matplotlib.pyplot as plt
import pandas as pd
import statsmodels.api as sm
from statsmodels.nonparametric.smoothers_lowess import lowess
import numpy as np
import scipy.stats as stats

def ResidFitted(fitted_model, residuals = None, fits = None, ax = None):
  """
  Parameters
  ---------------------------------------------------------
  fitted_model: A fitted linear regression model from the statsmodels package.
                Class: <statsmodels.regression.linear_model.OLS>
  residuals: A pandas series of the OLS residuals
  fits: A pandas series of the fitted values from the OLS model
  ax: A specific matplotlib axis. Used if creating subplots
  
  Returns
  ---------------------------------------------------------
  ax: A matplotlib axis object
  
  By: Jason Sadowski
  Date: 2019-11-19
  """
  if isinstance(residuals,type(None)):
    residuals = fitted_model.resid
  if isinstance(fits,type(None)):
    fits = fitted_model.fittedvalues
  top3 = abs(residuals).sort_values(ascending = False)[:3]
  smoothed = lowess(residuals,fits)
  if isinstance(ax,type(None)):
    fig, ax = plt.subplots()
  ax.scatter(fits, residuals, edgecolors = 'k', facecolors = 'none')
  ax.plot(smoothed[:,0],smoothed[:,1],color = 'r')
  ax.set_ylabel('Residuals')
  ax.set_xlabel('Fitted Values')
  ax.set_title('Residuals vs. Fitted')
  ax.plot([min(fits),max(fits)],[0,0],color = 'k',linestyle = ':')
  for i in top3.index:
    ax.annotate(i, xy = (fits[i],residuals[i]))
  return(ax)
    
def qqplot(fitted_model, student_residuals = None, ax = None):
  """
  Parameters
  ---------------------------------------------------------
  fitted_model: A fitted linear regression model from the statsmodels package.
                Class: <statsmodels.regression.linear_model.OLS>
  student_residuals: A pandas series of the internally studentized residuals.
  ax: A specific matplotlib axis. Used if creating subplots
  
  Returns
  ---------------------------------------------------------
  ax: A matplotlib axis object

  By: Jason Sadowski
  Date: 2019-11-19
  """
  if isinstance(student_residuals,type(None)):
    student_residuals = pd.Series(fitted_model.get_influence() \
                                  .resid_studentized_internal)
  else:
    student_residuals = pd.Series(student_residuals)
  sorted_student_residuals = student_residuals.copy()
  sorted_student_residuals.index = fitted_model.resid.index
  sorted_student_residuals = sorted_student_residuals \
                                .sort_values(ascending = True)
  df = pd.DataFrame(sorted_student_residuals)
  df.columns = ['sorted_student_residuals']
  df['theoretical_quantiles'] = stats.probplot(df['sorted_student_residuals'], \
                                              dist = 'norm', fit = False)[0]
  rankings = abs(df['sorted_student_residuals']).sort_values(ascending = False)
  top3 = rankings[:3]

  if isinstance(ax,type(None)):
    fig, ax = plt.subplots()

  x = df['theoretical_quantiles']
  y = df['sorted_student_residuals']
  ax.plot([np.min([x,y]),np.max([x,y])],[np.min([x,y]),np.max([x,y])], \
           color = 'r', ls = '--')
  ax.scatter(x,y, edgecolor = 'k',facecolor = 'none')
  ax.set_title('Normal Q-Q')
  ax.set_ylabel('Standardized Residuals')
  ax.set_xlabel('Theoretical Quantiles')

  for val in top3.index:
    ax.annotate(val,xy=(df['theoretical_quantiles'].loc[val], \
                        df['sorted_student_residuals'].loc[val]))

  return(ax)
   
def ScaleLocation(fitted_model, sqrt_student_resid = None, \
                  fits = None, ax = None):
  """
  Parameters
  ---------------------------------------------------------
  fitted_model: A fitted linear regression model from the statsmodels package.
                Class: <statsmodels.regression.linear_model.OLS>
  sqrt_student_residuals: A pandas series of the square root transformd
                          internally studentized residuals.    
  ax: A specific matplotlib axis. Used if creating subplots
  
  Returns
  ---------------------------------------------------------
  ax: A matplotlib axis object

  By: Jason Sadowski
  Date: 2019-11-19
  """
  if isinstance(sqrt_student_resid,type(None)):
    student_residuals = fitted_model.get_influence().resid_studentized_internal
    sqrt_student_resid = pd.Series(np.sqrt(np.abs(student_residuals)))
  if isinstance(fits,type(None)):
    fits = fitted_model.fittedvalues
  sqrt_student_resid.index = fitted_model.resid.index
  top3 = abs(pd.Series(sqrt_student_resid)).sort_values(ascending = False)[:3]
  smoothed = lowess(sqrt_student_resid,fits)
  if isinstance(ax,type(None)):
    fig, ax = plt.subplots()
  ax.scatter(fits, sqrt_student_resid, edgecolors = 'k', facecolors = 'none')
  ax.plot(smoothed[:,0],smoothed[:,1],color = 'r')
  ax.set_ylabel('$\sqrt{|Studentized \ Residuals|}$')
  ax.set_xlabel('Fitted Values')
  ax.set_title('Scale-Location')
  for i in top3.index:
    ax.annotate(i,xy=(fits[i],sqrt_student_resid[i]))
  return(ax)
    
def Leverage(fitted_model, student_residuals = None,\
            leverage = None, ax = None):
  """
  Parameters
  ---------------------------------------------------------
  fitted_model: A fitted linear regression model from the statsmodels package.
                Class: <statsmodels.regression.linear_model.OLS>
  student_residuals: A pandas series of the internally studentized residuals.
  ax: A specific matplotlib axis. Used if creating subplots
  
  Returns
  ---------------------------------------------------------
  ax: A matplotlib axis object

  The approach for coding the Cook's D lines comes from:
  https://emredjan.github.io/blog/2017/07/11/emulating-r-plots-in-python/
  
  By: Jason Sadowski
  Date: 2019-11-19
  """
  if isinstance(student_residuals,type(None)):
    student_residuals = pd.Series(fitted_model\
                           .get_influence().resid_studentized_internal)
    student_residuals.index = fitted_model.resid.index
  if isinstance(leverage,type(None)): 
    leverage = fitted_model.get_influence().hat_matrix_diag
  df = pd.DataFrame(student_residuals)
  df.columns = ['student_residuals']
  df['leverage'] = leverage
  sorted_student_resid = abs(df['student_residuals'])\
                          .sort_values(ascending = False)
  top3 = sorted_student_resid[:3]
  smoothed = lowess(student_residuals,leverage)
  if isinstance(ax,type(None)):
    fig, ax = plt.subplots()
  x = df['leverage']
  y = df['student_residuals']
  xpos = max(x)+max(x)*0.05  
  ax.scatter(x, y, edgecolors = 'k', facecolors = 'none')
  ax.plot(smoothed[:,0],smoothed[:,1],color = 'r')
  ax.set_ylabel('Studentized Residuals')
  ax.set_xlabel('Leverage')
  ax.set_title('Residuals vs. Leverage')
  ax.set_ylim(min(y)-min(y)*0.15,max(y)+max(y)*0.15)
  ax.set_xlim(-0.01,max(x)+max(x)*0.05)

  cooksx = np.linspace(min(x), xpos, 50)
  p = len(fitted_model.params)
  poscooks1y = np.sqrt((p*(1-cooksx))/cooksx)
  poscooks05y = np.sqrt(0.5*(p*(1-cooksx))/cooksx)
  negcooks1y = -np.sqrt((p*(1-cooksx))/cooksx)
  negcooks05y = -np.sqrt(0.5*(p*(1-cooksx))/cooksx)
  
  ax.plot(cooksx,poscooks1y,label = "Cook's Distance", ls = ':', color = 'r')
  ax.plot(cooksx,poscooks05y, ls = ':', color = 'r')
  ax.plot(cooksx,negcooks1y, ls = ':', color = 'r')
  ax.plot(cooksx,negcooks05y, ls = ':', color = 'r')
  ax.plot([0,0],ax.get_ylim(), ls=":", color = 'k', alpha = 0.3)
  ax.plot(ax.get_xlim(), [0,0], ls=":", color = 'k', alpha = 0.3)
  ax.annotate('1.0', xy = (xpos, poscooks1y[-1]), color = 'r')
  ax.annotate('0.5', xy = (xpos, poscooks05y[-1]), color = 'r')
  ax.annotate('1.0', xy = (xpos, negcooks1y[-1]), color = 'r')
  ax.annotate('0.5', xy = (xpos, negcooks05y[-1]), color = 'r')
  ax.legend()
  for val in top3.index:
    ax.annotate(val,xy=(x.loc[val],y.loc[val]))
  return(ax)
    
def allplots(fitted_model):
  """
  Parameters
  ---------------------------------------------------------
  fitted_model: A fitted linear regression model from the statsmodels package.
                Class: <statsmodels.regression.linear_model.OLS>
  
  Returns
  ---------------------------------------------------------
  axs: A matplotlib axis object with 4 subplots. The order of the subplots is
       as follows:
       [0,0]: Residuals vs. Fitted values
       [0,1]: Normal QQ Plot
       [1,0]: Studentized Residuals vs. Fitted Values
       [1,1]: Studentized Residuals vs. Leverage

  By: Jason Sadowski
  Date: 2019-11-19
  """
  fig, axs = plt.subplots(2,2)
  ResidFitted(fitted_model,ax = axs[0,0])
  qqplot(fitted_model,ax = axs[0,1])
  ScaleLocation(fitted_model,ax = axs[1,0])
  Leverage(fitted_model,ax = axs[1,1])
  fig.tight_layout()
  return(axs)
    