import pandas as pd
from nvd3 import lineChart

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
chart = lineChart(name=type,x_is_date=False, x_axis_format = "")

xdata = list(range(0, total_rows))

ydata = parameterlist["p"]

kwargs1 = {'color': 'blue'}
kwargs2 = {'color': 'red'}

chart.add_serie(y=ydata, x=xdata, name='p, probability of on/off', **kwargs1)

chart.buildhtml()

output_file.write(chart.htmlcontent)

output_file.close()