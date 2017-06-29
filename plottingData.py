# Plotting results from the Gibbs Sampling of the parameters of the linear model 
# (original Gibbs Sampler written in R). Main purpose of this program is to implement
# and learn d3 and Seaborn packages in python

import pandas as pd
from nvd3 import lineChart
import seaborn as sns
sns.set(style="ticks")

parameterlist = pd.read_csv('/home/prashant/Downloads/ParameterSample6_27.txt',sep = " ", header = None)
parameterlist.columns = ["p","Intercept", "Log_Reference" , "Cummulative_Exposure", "Lascar_47","Lascar_48","Lascar_49","Lascar_50","Lascar_51","Lascar_52","Lascar_53","Lascar_54","Lascar_55","Sigma","c","dd"]
parameterlist = pd.DataFrame(parameterlist)
#print parameterlist.columns


total_rows = len(parameterlist)

#print parameterlist[1:10]
#print total_rows
# File to save plot
output_file = open('LineChart.html','w')

type = "lineChart"
chart = lineChart(name=type,x_is_date=False, width = 750,height = 750)

xdata = list(range(0, total_rows))

ydata = parameterlist["p"]

kwargs1 = {'color': 'blue'}
kwargs2 = {'color': 'red'}

chart.add_serie(y=ydata, x=xdata, name='p, probability of on/off', **kwargs1)
chart.create_x_axis(name="Xaxis", label="Sampled Point")
chart.create_y_axis(name="Yaxis", label="Probability of On/Off")


chart.buildhtml()
#Thanks to Mahesh Kumar for figuring this out!
output_file.write(chart.htmlcontent.replace("Xa","xA").replace("Ya","yA"))
output_file.close()


ax = sns.pointplot(x = xdata,y = "p",data = parameterlist)
sns.plt.show()
