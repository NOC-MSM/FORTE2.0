#!/usr/bin/env python
# coding: utf-8

##################################################################################################################################
####### THE FOLLOWING SCRIPT ALLOWS THE CONSTRUCTION OF INPUT U,V,T and SP FIELDS, AS WELL AS SPATIAL AND TEMPORAL FILTERS #######
################################ TO FEED INTO FORTE2 AS A NUDGING REFERENCE STATE ################################################
##################################################################################################################################

from IPython.core.display import display, HTML
display(HTML("<style>.container { width:100% !important; }</style>"))

# To import both packages:
import cf,cfplot as cfp
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Polygon
from matplotlib.collections import PatchCollection
import pandas as pd
import datetime
from netCDF4 import Dataset, num2date, date2num
import glob
import os
import xarray as xr
import cftime
from itertools import chain
import scipy as sp
from datetime import datetime, timedelta
from scipy.io import netcdf
from matplotlib.mlab import griddata
import iris
from windspharm.iris import VectorWind
from iris.coord_categorisation import add_month, add_season_membership
import cartopy.crs as ccrs
import iris.plot as iplt
from sklearn.utils import resample
from mpl_toolkits.basemap import Basemap
from matplotlib.ticker import (MultipleLocator, FormatStrFormatter, AutoMinorLocator)
import matplotlib.patches as mpatches
import matplotlib.lines as mlines
import scipy as sp
from netCDF4 import Dataset
from __future__ import division
import numpy as np
import pygeode as pyg
import spharm
from netCDF4 import Dataset
from scipy import interpolate
from spharm import Spharmt
import numpy.ma as ma
import spharm
import iris
from windspharm.iris import VectorWind

def get_spliney2(v, cyc=True):
    '''Finds 2nd derivatives at each grid point for cubic spline 
   interpolation.  This is implemented in IGCM with a dimensionless 
   timestep (i.e. the difference between each of the points at which
   the variabile is specified must be the same and is defined as unity 
   to make the equations below simpler. 

   If cyc is true, the spline is taken to be periodic; if it is False, 
   the first derivatives are set to 0 at each end point.'''

    import pygeode as pyg

    NT = len(v.time)
   
    # Degenerate case; treat v as a constant in time
    if NT < 2: return (0*v).rename(v.name + '2').load() 

    vt = v.transpose('time') # ensure first axis is time
    V = vt[:] # extract data
    V = V.reshape(NT,-1)

    No = V.shape[1]

    M = np.zeros((NT, NT), 'd')
    B = np.zeros((NT, No), 'd')

    if cyc:
        i = np.arange(NT)
        ip = (i + 1) % NT
        im = (i - 1) % NT
    else:
        i = np.arange(1, NT-1)
        ip = i + 1
        im = i - 1
      
        #BC: y' = 0 at ends
        M[0,0] = 1. / 3.
        M[NT-1,NT-1] = 1. / 3.
        B[0,:] = V[1,:] - V[0,:]
        B[NT-1,:] = V[NT-2,:] - V[NT-1,:]

    M[i,i] = 2./3.
    M[i,im] = 1./6.
    M[i,ip] = 1./6.
    B[i,:] = V[ip,:] - 2*V[i,:] + V[im,:]

    V2 = np.linalg.solve(M, B)
    v2 = pyg.Var(vt.axes, name=v.name + '2', values=V2.reshape(vt.shape))

    return v2.transpose(*v.axes)


#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################

################################# PART 1 ############################################
############ Making and visualising the spatial and temporal filters ################
#####################################################################################

#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################


### Plotting 'nudge_spa_strength' for visual check

#Template to allow construction of spatial nudging filter

forte2_template = cf.read()

#Defining from equation 2 in Watson et al 2016

f__phi_phi1_phi2 = []
f__lambdaa_lambdaa1_lambdaa2 = []
gamma__sn_ew = []

#Radians = Degrees * np.pi / 180

lats_rad = np.asarray(forte2_template.dim('latitude'))*(np.pi/180)
lons_rad = np.asarray(forte2_template.dim('longitude'))*(np.pi/180)

#Defining extents of the spatial filter. sl = Southern Latitude, nl = Northern latitude, wl = western longitude, el = eastern longitude

sl = 38. 
nl = 80.
wl = 110.
el = 200.

#Coefficients from equation 2 to define shape of spatial taper

sigma = 0.15

sigma2 = 0.106
sigma3 = 0.2

#Converting sl,nl,wl,el to radians

south_lat = sl * (np.pi/180) 
north_lat = nl * (np.pi/180)
west_lon = wl * (np.pi/180)
east_lon = el * (np.pi/180)

# Equations and workflow based on Eq 2 from Watson et al. 2016

f__phi_phi1_phi2 = np.asarray([1 / (1 + np.exp(-(lats_rad - south_lat)/sigma2))])*np.asarray([1 - 1/(1 + np.exp(-(lats_rad - north_lat)/sigma3))])
f__lambdaa_lambdaa1_lambdaa2 = np.asarray([1 / (1 + np.exp(-(lons_rad - west_lon)/sigma))])*np.asarray([1 - 1/(1 + np.exp(-(lons_rad - east_lon)/sigma))])

tt = f__phi_phi1_phi2.T
ff = f__lambdaa_lambdaa1_lambdaa2

#Define empty array and set equal to tt

make_arr = np.empty([64,128])
make_arr[:,:] = tt

make_arr2 = make_arr
make_arr2 = make_arr2 * ff

#Normalising spatial weights

maxi = 1.
maxo = make_arr2.max()
cons = maxi/maxo

scaled_arr = make_arr2 * cons

#Set nudg_spa_strenght

nudg_spa_strengthz = scaled_arr
nudg_spa_strength = np.roll(nudg_spa_strengthz,64,axis=1)

# if nudg_spa_strenght < or equal to 0, set to 0

check = (nudg_spa_strength > 0.1).astype(int)
nudg_spa_strength = nudg_spa_strength * check


##############
##############
##############

### use cf plot or iris to check what the spatial filter looks like ###
### example code here is cf plot ###

#lats = np.asarray(forte2_template.dim('latitude'))
#lons = np.asarray(forte2_template.dim('longitude'))

#cfp.gopen()

#cfp.mapset(lonmin=180, lonmax=480,latmin=-80, latmax=80)
#cfp.levs(min=0, max=1, step=0.1)
#cfp.setvars(axis_label_fontsize = 30, text_fontsize = 30, title_fontsize = 30, colorbar_fontsize=30)
#cfp.con(f=nudg_spa_strength,ptype=1, title='Spatial Filter', x=lons, y=lats, blockfill=False, lines=False, fill=True, colorbar_label_skip=5, colorbar_position = [0.1, 0.1, 0.82, 0.02])

##############
##############
##############

# ## Defining Temporal Nudging strength over time period where nudging takes place (e.g. NDJFM)

#Function fitting

from matplotlib import pyplot as mp

#Defining Gaussian Fuction

def gaussian(x, mu, sig):
    return np.exp(-np.power(x - mu, 2.) / (2 * np.power(sig, 0.2)))

#Defining Super Gaussian Function (Flat top Gaussian)

def flat_top_gauss(d,mu,beta):
    return (1/((np.exp((np.power((-0.5*((d*d)/(beta*beta))),2*mu))))))

# X values defined based on number of days over which nudging takes place
# e.g. if nudging takes place over NDJFM = 5 months x 30 days = 150 days
# (-2,2,150) Respresents 5 months and 150 days [-2,-1,0,1,2]

x_values_gauss = np.linspace(-2, 2, 120)
x_values_supergauss = np.linspace(-2, 2, 120)

# Choose shape of Gaussian / Super Gaussian Function

gauss1 = gaussian(x_values_gauss, 0, 2)
supergauss1 = flat_top_gauss(x_values_supergauss, 2, 1.2)

###################

# e.g. Check chosen function -> plot

#mp.figure(figsize=(20,10))

#mp.plot(x_values_supergauss, supergauss1,linewidth=4, label = 'Super Gaussian')
#mp.plot(x_values_gauss, gauss1,linewidth=4, label='Gauss')

#plt.xticks(fontsize=20)
#plt.yticks(fontsize=20)

#plt.ylabel('Temporal Nudging Strength', fontsize=20)
#plt.legend(loc=2, prop={'size': 16})
#mp.title('Temporal nudging strength over DJFM - Maxima centered on February 1st', fontsize=26)

#mp.show()

###################

## DNUDG values in L20 ##
## values taken from printing vertical nudging strength coefficients ##
## Defined L332 CREATE.basic_gpnudg.L20 ##
 
DNUND1=  0.000000000000000E+000
DNUND2=  8.163228127675025E-004
DNUND3=  2.487540788453165E-003
DNUND4=  4.230812306400144E-003
DNUND5=  6.044793665161751E-003
DNUND6=  7.948732609120662E-003
DNUND7=  9.988054594863518E-003
DNUND8=  1.223714766048863E-002
DNUND9=  1.479804241919918E-002
DNUND10=  1.779407023785941E-002
DNUND11=  2.135822830279751E-002
DNUND12=  2.561681479673579E-002
DNUND13=  3.066979802681144E-002
DNUND14=  3.657019183218063E-002
DNUND15=  4.330526041162808E-002    
DNUND16=  5.078252998980447E-002
DNUND17=  5.882326295148920E-002
DNUND18=  6.716525537282701E-002
DNUND19=  7.547564475931298E-002
DNUND20=  7.957747154594243E-002

y = (20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)

# normalising DNUDG

DNUND1 = (DNUND1/DNUND20)*100
DNUND2 = (DNUND2/DNUND20)*100
DNUND3 = (DNUND3/DNUND20)*100
DNUND4 = (DNUND4/DNUND20)*100
DNUND5 = (DNUND5/DNUND20)*100
DNUND6 = (DNUND6/DNUND20)*100
DNUND7 = (DNUND7/DNUND20)*100
DNUND8 = (DNUND8/DNUND20)*100
DNUND9 = (DNUND9/DNUND20)*100
DNUND10 = (DNUND10/DNUND20)*100
DNUND11 = (DNUND11/DNUND20)*100
DNUND12 = (DNUND12/DNUND20)*100
DNUND13 = (DNUND13/DNUND20)*100
DNUND14 = (DNUND14/DNUND20)*100
DNUND15 = (DNUND15/DNUND20)*100
DNUND16 = (DNUND16/DNUND20)*100
DNUND17 = (DNUND17/DNUND20)*100
DNUND18 = (DNUND18/DNUND20)*100
DNUND19 = (DNUND19/DNUND20)*100
DNUND20 = (DNUND20/DNUND20)*100

### Plotting routine to visualise nudging coefficieints ##

#################################
## uncomment to run if desired ##
#################################

#cfp.gopen(figsize=(35,5), rows=1, columns=3, bottom=0.2)

#cfp.gpos(1)

#lats = np.asarray(forte2_template.dim('latitude'))
#lons = np.asarray(forte2_template.dim('longitude'))

#cfp.mapset(lonmin=180, lonmax=480,latmin=-80, latmax=80)
#cfp.levs(min=0, max=100, step=10)
#cfp.setvars(axis_label_fontsize = 20, text_fontsize = 30, title_fontsize = 24, colorbar_fontsize=24)
#cfp.con(f=nudg_spa_strength*100,ptype=1, title='Spatial Filter over Nudging Region', x=lons, y=lats, blockfill=False, lines=False, fill=True, colorbar_label_skip=2, colorbar_title='Nudging strength (%)',colorbar_position = [0.12, 0.1, 0.23, 0.02])

#level = [0.96,0.62249,0.362485, 0.224245, 0.14152]
#level = np.around(level,2)
#level_pos=[1,5,9,13,17]

#plt.plot((DNUND1,DNUND2,DNUND3,DNUND4,DNUND5,DNUND6,DNUND7,DNUND8,DNUND9,DNUND10,DNUND11,DNUND12,DNUND13,DNUND14,DNUND15,DNUND16,DNUND17,DNUND18,DNUND19,DNUND20),y, linewidth=4)
#plt.xlabel('Nudging strength (%)', fontsize=24)
#plt.ylabel('Model Level', fontsize=24)
#plt.yticks(level_pos,level)
#plt.rc('font', size=24)          # controls default text sizes
#plt.rc('axes', titlesize=24)     # fontsize of the axes title
#plt.rc('axes', labelsize=24)    # fontsize of the x and y labels
#plt.rc('xtick', labelsize=24)    # fontsize of the tick labels
#plt.rc('ytick', labelsize=24)    # fontsize of the tick labels
#plt.rc('legend', fontsize=24)    # legend fontsize
#plt.rc('figure', titlesize=22)  # fontsize of the figure title
#plt.title('Vertical Nudging strength', fontsize=24)

#cfp.gpos(3)

#months=['Dec 1st','Feb 1st','Mar 30th']
#months_pos=[-2,0,2]

#plt.plot(x_values_supergauss, supergauss1*100,linewidth=4)
#mp.plot(x_values_gauss, gauss1,linewidth=4, label='Gauss')
#plt.xticks(months_pos,months,fontsize=20)
#plt.yticks(fontsize=20)
#plt.ylabel('Nudging Strength (%)', fontsize=20)
#plt.title('Temporal nudging strength', fontsize=24)

#cfp.gclose()

#################################
## uncomment to run if desired ##
#################################

#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################

################################# PART 2 ############################################
########## CONVERTING THE INPUTS (SP, U, V, T) AND FILTERS INTO .dat files ##########
########################### TO BE USED IN FORTE2.0 ##################################

#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################


#### Reading in climatologies for ps, temp, ua and va
 
ps_150250_clim = cf.read_field().squeeze()
temp_150250_clim = cf.read_field().squeeze()
ua_150250_clim = cf.read_field().squeeze()
va_150250_clim = cf.read_field().squeeze()

latss = np.asarray(ps_150250_clim.dim('latitude').data)
lonss = np.asarray(ps_150250_clim.dim('longitude').data)


#### Reading in [climatology - nudging reference state]

ps_diff = cf.read_field().squeeze() 
temp_diff = cf.read_field().squeeze()
ua_diff = cf.read_field().squeeze()
va_diff = cf.read_field().squeeze()

#Resizing/shaping to get into correct format - no input needed

ps_diff_expand = np.expand_dims(ps_diff,axis=0)
temp_diff_expand = np.expand_dims(temp_diff,axis=0)
ua_diff_expand = np.expand_dims(ua_diff,axis=0)
va_diff_expand = np.expand_dims(va_diff,axis=0)

ps_diff_expand_rsize = np.resize(ps_diff_expand,(360,1,64,128))
temp_diff_expand_rsize = np.resize(temp_diff_expand,(360,20,64,128))
ua_diff_expand_rsize = np.resize(ua_diff_expand,(360,20,64,128))
va_diff_expand_rsize = np.resize(va_diff_expand,(360,20,64,128))

ps_diff_arr = np.asarray(ps_diff_expand_rsize)
temp_diff_arr = np.asarray(temp_diff_expand_rsize)
ua_diff_arr = np.asarray(ua_diff_expand_rsize)
va_diff_arr = np.asarray(va_diff_expand_rsize)

ps_tapered_arr = np.copy(ps_diff_arr)
temp_tapered_arr = np.copy(temp_diff_arr)
ua_tapered_arr = np.copy(ua_diff_arr)
va_tapered_arr = np.copy(va_diff_arr)


# ### Multiply Temporal Function with [climatology] --> User input Needed

#Input gaussian function!

scale_arr = np.copy(supergauss1)

count = 0

# [i + 59] and [i -300 + 1] in the example are based on nudging taking place over NDJFM with peak in J, so Gaussian
# function has to be split over the year, starting in N and finishing in M

i = 0
while i < 360:
    if i >= 0 and i < 90:
        ps_tapered_arr[i] = (ps_tapered_arr[i] * scale_arr[i + 29])
        temp_tapered_arr[i] = (temp_tapered_arr[i] * scale_arr[i + 29])
        ua_tapered_arr[i] = (ua_tapered_arr[i] * scale_arr[i + 29])
        va_tapered_arr[i] = (va_tapered_arr[i] * scale_arr[i + 29])       
        
    elif i >= 330:
        ps_tapered_arr[i] = (ps_tapered_arr[i] * scale_arr[i - 330 + 1])
        temp_tapered_arr[i] = (temp_tapered_arr[i] * scale_arr[i - 330 + 1])
        ua_tapered_arr[i] = (ua_tapered_arr[i] * scale_arr[i - 330 + 1])
        va_tapered_arr[i] = (va_tapered_arr[i] * scale_arr[i - 330 + 1])
      
        
    i = i + 1
    count = count + 1
    
ps_resid =  ps_tapered_arr
temp_resid = temp_tapered_arr
ua_resid = ua_tapered_arr
va_resid = va_tapered_arr

#### Subtracting residual from climatology for NDJFM

#firstly, make climatologies arrays...

ps_150250_clim_arr = np.asarray(ps_150250_clim)
temp_150250_clim_arr = np.asarray(temp_150250_clim)
ua_150250_clim_arr = np.asarray(ua_150250_clim)
va_150250_clim_arr = np.asarray(va_150250_clim)

#make copies...

ps_clim_and_jan = np.copy(ps_150250_clim_arr)
temp_clim_and_jan = np.copy(temp_150250_clim_arr)
ua_clim_and_jan = np.copy(ua_150250_clim_arr)
va_clim_and_jan = np.copy(va_150250_clim_arr)

count = 0

i = 0
while i < 360:
    if i >= 0 and i < 90:
        ps_clim_and_jan[i] = (ps_150250_clim_arr[i] - ps_resid[i])
        temp_clim_and_jan[i] = (temp_150250_clim_arr[i] - temp_resid[i])
        ua_clim_and_jan[i] = (ua_150250_clim_arr[i] - ua_resid[i])
        va_clim_and_jan[i] = (va_150250_clim_arr[i] - va_resid[i])
        
    elif i >= 330:
        ps_clim_and_jan[i] = (ps_150250_clim_arr[i] - ps_resid[i])
        temp_clim_and_jan[i] = (temp_150250_clim_arr[i] - temp_resid[i])
        ua_clim_and_jan[i] = (ua_150250_clim_arr[i] - ua_resid[i])
        va_clim_and_jan[i] = (va_150250_clim_arr[i] - va_resid[i])
      
    else:
        ps_clim_and_jan[i] = (ps_150250_clim_arr[i])
        temp_clim_and_jan[i] = (temp_150250_clim_arr[i])
        ua_clim_and_jan[i] = (ua_150250_clim_arr[i])
        va_clim_and_jan[i] = (va_150250_clim_arr[i])
        
    i = i + 1
    count = count + 1
    
#### Final values to go into nudging dat file therefore..
##### Times by 100 to SP to convert to from hPa to Pa

ps_forte2_arr = ps_clim_and_jan * 100
temp_forte2_arr = temp_clim_and_jan
ua_forte2_arr = ua_clim_and_jan
va_forte2_arr = va_clim_and_jan

ps_forte2_arr_ts = ps_forte2_arr

#### Finalising Spatial mask array (here called dum_forte2_arr)

dum_forte2_arr = (np.asarray(temp_150250_clim)/np.asarray(temp_150250_clim)) * nudg_spa_strength


#### Timeramp variable goes into FORTE2 alongside variables to represent temporal function

timeramp_forte2_arr = (np.asarray(temp_150250_clim)/np.asarray(temp_150250_clim)) -1

count = 0

i = 0
while i < 360:
    if i >= 0 and i < 90:
        timeramp_forte2_arr[i] = ((timeramp_forte2_arr[i] +1) * scale_arr[i + 29])
        
    elif i >= 330:
        timeramp_forte2_arr[i] = ((timeramp_forte2_arr[i]+1) * scale_arr[i - 330 + 1])
        
    i = i + 1
    count = count + 1

#Expanding dim of original forte2 array to set up spliney array

ua_forte2_arr_na = np.expand_dims(ua_forte2_arr,axis=0)
va_forte2_arr_na = np.expand_dims(va_forte2_arr,axis=0)
temp_forte2_arr_na = np.expand_dims(temp_forte2_arr,axis=0)
ps_forte2_arr_na = np.expand_dims(ps_forte2_arr,axis=0)

dum_forte2_arr_na = np.expand_dims(dum_forte2_arr,axis=0)
timeramp_forte2_arr_na = np.expand_dims(timeramp_forte2_arr,axis=0)


### Writing conditioned variables into a .nc file prior to .dat

#CREATING A .NC FILE WITH ALL VARS (INCLUDING DUM) TO ALLOW READING IN PYG FOR USE IN SPLINEY CALCULATIONS
# THIS NEEDS TO BE DONE AS THERE HAS BEEN SOME TEMPORAL TAPERING DONE TO ORIGINAL DATA FURTHER UP 

levz = np.asarray(temp_150250_clim.dim('ncvar%level').data)
latitudes = np.asarray(temp_150250_clim.dim('ncvar%latitude').data)
longitudes = np.asarray(temp_150250_clim.dim('ncvar%longitude').data)
timez = np.asarray(temp_150250_clim.dim('ncvar%time').data)

lat_range = len(latitudes) + 1
lon_range = len(longitudes) + 1

#test_press = np.array(plevels,latitudes,longitudes)
p = np.ndarray(shape=(20,64,128), dtype=float, order='F')
p[0] = levz[0]
p[1] = levz[1]
p[2] = levz[2]
p[3] = levz[3]
p[4] = levz[4]
p[5] = levz[5]
p[6] = levz[6]
p[7] = levz[7]
p[8] = levz[8]
p[9] = levz[9]
p[10] = levz[10]
p[11] = levz[11]
p[12] = levz[12]
p[13] = levz[13]
p[14] = levz[14]
p[15] = levz[15]
p[16] = levz[16]
p[17] = levz[17]
p[18] = levz[18]
p[19] = levz[19]

# Combining into one .nc file

#####################

save_str = ''

dataset = Dataset(save_str, 'w', format='NETCDF4_CLASSIC')

lon = dataset.createDimension('lon',128)
lat = dataset.createDimension('lat',64)
lev = dataset.createDimension('lev',20)
time = dataset.createDimension('time',360)

lats = dataset.createVariable('lat', np.float32, ('lat',))
lons = dataset.createVariable('lon', np.float32, ('lon',))
levs = dataset.createVariable('lev', np.float32, ('lev',))
times = dataset.createVariable('time', np.float32, ('time',))

dum = dataset.createVariable('dum', np.float32, ('time','lev','lat','lon'))
timeramp = dataset.createVariable('timeramp', np.float32, ('time','lev','lat','lon'))

ua = dataset.createVariable('ua', np.float32, ('time','lev','lat','lon'))
va = dataset.createVariable('va', np.float32, ('time','lev','lat','lon'))
temp = dataset.createVariable('temp', np.float32, ('time','lev','lat','lon'))
sp = dataset.createVariable('sp', np.float32, ('time','lat','lon'))

lons.standard_name='longitude'
lons.units = 'degree_east'
lons.axis = 'X'
lats.standard_name = 'latitude'
lats.units = 'degree_north'
lats.axis = 'Y'
levs.standard_name = 'levels'
levs.units = 'hPa'
levs.axis = 'Z'

times.standard_name = 'time'
times.units = 'days'
times.axis = 'T'

dum.standard_name = 'nudging weights'
dum.units = 'dimensionless'

timeramp.standard_name = 'time ramp weights'
timeramp.units = 'dimensionless'

ua.standard_name = 'ua'
ua.units = 'ms-1'

va.standard_name = 'va'
va.units = 'ms-1'

temp.standard_name = 'temp'
temp.units = 'degC'

sp.standard_name = 'sp'
sp.units = 'Pa'

lons[:] = longitudes
lats[:] = latitudes
levs[:] = levz
times[:] = timez

dum[:] = dum_forte2_arr
timeramp[:] = timeramp_forte2_arr

ua[:] = ua_forte2_arr
va[:] = va_forte2_arr
temp[:] = temp_forte2_arr
sp[:] = ps_forte2_arr

dataset.close()

####################

### Reading .nc files (just written above) back in using pyg.open to aid setting up cubic spline interpolation

# Cubic spline set up
# Load pyg dataset

undg = pyg.open('')['ua']
vndg = pyg.open('')['va']
tempndg = pyg.open('')['temp']
psndg = pyg.open('')['sp']

#MAKE SURE THIS IS RIGHT!!

dumndg = pyg.open('')['dum']
timerampndg = pyg.open('')['timeramp']

### Find 2nd derivatives for cubic spline interpolation (if there
### is time variation in nudging ref climatology) 

undg2 = get_spliney2(undg)
undg2_arr = np.asarray(undg2.values)

vndg2 = get_spliney2(vndg)
vndg2_arr = np.asarray(vndg2.values)

tempndg2 = get_spliney2(tempndg)
tempndg2_arr = np.asarray(tempndg2.values)

psndg2 = get_spliney2(psndg)
psndg2_arr = np.asarray(psndg2.values)

dumndg2 = get_spliney2(dumndg)
dumndg2_arr = np.asarray(dumndg2.values)

timerampndg2 = get_spliney2(timerampndg)
timerampndg2_arr = np.asarray(timerampndg2.values)

def create_igcm4_2dclim_newsplin(clim):

    move = np.moveaxis(clim[:,:,:],2,1)
    clim_nh = np.expand_dims(move[:,:,:32],axis=2)
    clim_sh = np.expand_dims(move[:,:,32:],axis=2)
    clim_shx = clim_sh[:,:,:,::-1]
    
    # Put into one array and then into dummy array
    climz = np.append(clim_nh,clim_shx,axis=2)
    
    b = np.zeros([8192, 1])

    lsz = climz.shape[0]
    xsz = climz.shape[1]
    hsz = climz.shape[2]
    ysz = climz.shape[3]

    xc = 0
    for y in range(ysz):
        for h in range(hsz):
            byo = xc * xsz
            for x in range(xsz):
                by = x + byo
                line = ''
                for l in range(lsz):
                    bx = l
                    b[by, bx] = climz[l, x, h, y]
            xc += 1
    
    return b

def create_igcm4_3dclim_newsplin(clim):

    move = np.moveaxis(clim[:,:,:],2,1)
    clim_nh = np.expand_dims(move[:,:,:32],axis=2)
    clim_sh = np.expand_dims(move[:,:,32:],axis=2)
    clim_shx = clim_sh[:,:,:,::-1]
    
    # Put into one array and then into dummy array
    climz = np.append(clim_nh,clim_shx,axis=2)
    
    b = np.zeros([8192, 20])

    lsz = climz.shape[0]
    xsz = climz.shape[1]
    hsz = climz.shape[2]
    ysz = climz.shape[3]

    xc = 0
    for y in range(ysz):
        for h in range(hsz):
            byo = xc * xsz
            for x in range(xsz):
                by = x + byo
                line = ''
                for l in range(lsz):
                    bx = l
                    b[by, bx] = climz[l, x, h, y]
            xc += 1
    
    return b


# ### Split up the variables to save memory (on deallocation)

#Expanding dim of spliney forte2 array to set up appended array
undg2_arr_na = np.expand_dims(undg2_arr,axis=0)

#Appending raw with spliney
ua_rsplin = np.append(ua_forte2_arr_na,undg2_arr_na,axis=0)

ua_array_newsplin0 = np.zeros((360,8192,20))
ua_array_newsplin1 = np.zeros((360,8192,20))

x=0
while x < 360: 
    ua_array_newsplin0[x,:,:] = create_igcm4_3dclim_newsplin(ua_rsplin[0,x,:,:,:])
    x = x+1    
    
x=0
while x < 360: 
    ua_array_newsplin1[x,:,:] = create_igcm4_3dclim_newsplin(ua_rsplin[1,x,:,:,:])
    x = x+1

#Reversing order of levels - testing on forte2

ua_array_newsplin0_lrev = ua_array_newsplin0[:, :, ::-1]
ua_array_newsplin1_lrev = ua_array_newsplin1[:, :, ::-1]

x=0

while x < 360:
    np.savetxt('.../ua_d'+str(x)+'_0_dims.dat',ua_array_newsplin0_lrev[x,:,:])
    np.savetxt('.../ua_d'+str(x)+'_1_dims.dat',ua_array_newsplin1_lrev[x,:,:])
    
    x=x+1
    
## Clear arrays to save memory

undg2_arr_na = None
ua_rsplin = None
ua_array_newsplin0 = None
ua_array_newsplin1 = None
ua_array_newsplin0_lrev = None
ua_array_newsplin1_lrev = None

#Expanding dim of spliney forte2 array to set up appended array
vndg2_arr_na = np.expand_dims(vndg2_arr,axis=0)

#Appending raw with spliney
va_rsplin = np.append(va_forte2_arr_na,vndg2_arr_na,axis=0)

va_array_newsplin0 = np.zeros((360,8192,20))
va_array_newsplin1 = np.zeros((360,8192,20))

x=0
while x < 360: 
    va_array_newsplin0[x,:,:] = create_igcm4_3dclim_newsplin(va_rsplin[0,x,:,:,:])
    x = x+1    
    
x=0
while x < 360: 
    va_array_newsplin1[x,:,:] = create_igcm4_3dclim_newsplin(va_rsplin[1,x,:,:,:])
    x = x+1

#Reversing order of levels - testing on forte2

va_array_newsplin0_lrev = va_array_newsplin0[:, :, ::-1]
va_array_newsplin1_lrev = va_array_newsplin1[:, :, ::-1]

x=0

while x < 360:
    np.savetxt('.../va_d'+str(x)+'_0_dims.dat',va_array_newsplin0_lrev[x,:,:])
    np.savetxt('.../va_d'+str(x)+'_1_dims.dat',va_array_newsplin1_lrev[x,:,:])
    
    x=x+1

## Clear arrays to save memory
vndg2_arr_na = None
va_rsplin = None
va_array_newsplin0 = None
va_array_newsplin1 = None
va_array_newsplin0_lrev = None
va_array_newsplin1_lrev = None

#Expanding dim of spliney forte2 array to set up appended array
tempndg2_arr_na = np.expand_dims(tempndg2_arr,axis=0)

#Appending raw with spliney
temp_rsplin = np.append(temp_forte2_arr_na,tempndg2_arr_na,axis=0)

temp_array_newsplin0 = np.zeros((360,8192,20))
temp_array_newsplin1 = np.zeros((360,8192,20))

x=0
while x < 360: 
    temp_array_newsplin0[x,:,:] = create_igcm4_3dclim_newsplin(temp_rsplin[0,x,:,:,:])
    x = x+1    
    
x=0
while x < 360: 
    temp_array_newsplin1[x,:,:] = create_igcm4_3dclim_newsplin(temp_rsplin[1,x,:,:,:])
    x = x+1

#Reversing order of levels - testing on forte2

temp_array_newsplin0_lrev = temp_array_newsplin0[:, :, ::-1]
temp_array_newsplin1_lrev = temp_array_newsplin1[:, :, ::-1]

x=0

while x < 360:
    np.savetxt('.../temp_d'+str(x)+'_0_dims.dat',temp_array_newsplin0_lrev[x,:,:])
    np.savetxt('.../temp_d'+str(x)+'_1_dims.dat',temp_array_newsplin1_lrev[x,:,:])
    
    x=x+1
    
## Clear arrays to save memory
tempndg2_arr_na = None
temp_rsplin = None
temp_array_newsplin0 = None
temp_array_newsplin1 = None
temp_array_newsplin0_lrev = None
temp_array_newsplin1_lrev = None

#Expanding dim of spliney forte2 array to set up appended array
dumndg2_arr_na = np.expand_dims(dumndg2_arr,axis=0)

#Appending raw with spliney
dum_rsplin = np.append(dum_forte2_arr_na,dumndg2_arr_na,axis=0)

dum_array_newsplin0 = np.zeros((360,8192,20))
dum_array_newsplin1 = np.zeros((360,8192,20))

x=0
while x < 360: 
    dum_array_newsplin0[x,:,:] = create_igcm4_3dclim_newsplin(dum_rsplin[0,x,:,:,:])
    x = x+1    
    
x=0
while x < 360: 
    dum_array_newsplin1[x,:,:] = create_igcm4_3dclim_newsplin(dum_rsplin[1,x,:,:,:])
    x = x+1

#Reversing order of levels - testing on forte2

dum_array_newsplin0_lrev = dum_array_newsplin0[:, :, ::-1]
dum_array_newsplin1_lrev = dum_array_newsplin1[:, :, ::-1]

x=0

while x < 360:
    np.savetxt('.../dum_d'+str(x)+'_0_dims.dat',dum_array_newsplin0_lrev[x,:,:])
    np.savetxt('.../dum_d'+str(x)+'_1_dims.dat',dum_array_newsplin1_lrev[x,:,:])
    
    x=x+1
    
## Clear arrays to save memory
dumndg2_arr_na = None
dum_rsplin = None
dum_array_newsplin0 = None
dum_array_newsplin1 = None
dum_array_newsplin0_lrev = None
dum_array_newsplin1_lrev = None

#Expanding dim of spliney forte2 array to set up appended array
timerampndg2_arr_na = np.expand_dims(timerampndg2_arr,axis=0)

#Appending raw with spliney
timeramp_rsplin = np.append(timeramp_forte2_arr_na,timerampndg2_arr_na,axis=0)

timeramp_array_newsplin0 = np.zeros((360,8192,20))
timeramp_array_newsplin1 = np.zeros((360,8192,20))

x=0
while x < 360: 
    timeramp_array_newsplin0[x,:,:] = create_igcm4_3dclim_newsplin(timeramp_rsplin[0,x,:,:,:])
    x = x+1    
    
x=0
while x < 360: 
    timeramp_array_newsplin1[x,:,:] = create_igcm4_3dclim_newsplin(timeramp_rsplin[1,x,:,:,:])
    x = x+1

#Reversing order of levels - testing on forte2

timeramp_array_newsplin0_lrev = timeramp_array_newsplin0[:, :, ::-1]
timeramp_array_newsplin1_lrev = timeramp_array_newsplin1[:, :, ::-1]

x=0

while x < 360:
    np.savetxt('.../timeramp_d'+str(x)+'_0_dims.dat',timeramp_array_newsplin0_lrev[x,:,:])
    np.savetxt('.../timeramp_d'+str(x)+'_1_dims.dat',timeramp_array_newsplin1_lrev[x,:,:])
    
    x=x+1
    
## Clear arrays to save memory
timerampndg2_arr_na = None
timeramp_rsplin = None
timeramp_array_newsplin0 = None
timeramp_array_newsplin1 = None
timeramp_array_newsplin0_lrev = None
timeramp_array_newsplin1_lrev = None

ps_forte2_arr_na.shape

#Expanding dim of spliney forte2 array to set up appended array
psndg2_arr_na = np.expand_dims(psndg2_arr,axis=0)

#Appending raw with spliney
ps_rsplin = np.append(ps_forte2_arr_na,psndg2_arr_na,axis=0)
ps_rsplin_exp = np.expand_dims(ps_rsplin,axis=2)

ps_array_newsplin0 = np.zeros((360,8192,1))
ps_array_newsplin1 = np.zeros((360,8192,1))

x=0
while x < 360: 
    ps_array_newsplin0[x,:,:] = create_igcm4_2dclim_newsplin(ps_rsplin_exp[0,x,:,:,:])
    x = x+1    
    
x=0
while x < 360: 
    ps_array_newsplin1[x,:,:] = create_igcm4_2dclim_newsplin(ps_rsplin_exp[1,x,:,:,:])
    x = x+1

#Reversing order of levels - testing on forte2

ps_array_newsplin0_lrev = ps_array_newsplin0[:, :, ::-1]
ps_array_newsplin1_lrev = ps_array_newsplin1[:, :, ::-1]

x=0

while x < 360:
    np.savetxt('.../ps_d'+str(x)+'_0_dims.dat',ps_array_newsplin0_lrev[x,:,:])
    np.savetxt('.../ps_d'+str(x)+'_1_dims.dat',ps_array_newsplin1_lrev[x,:,:])
    
    x=x+1
    
## Clear arrays to save memory
psndg2_arr_na = None
ps_rsplin = None
ps_array_newsplin0 = None
ps_array_newsplin1 = None
ps_array_newsplin0_lrev = None
ps_array_newsplin1_lrev = None

