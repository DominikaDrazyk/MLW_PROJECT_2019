import pandas as pd
import scipy.stats as stats
import statsmodels.api as sm
import seaborn as sns
from statsmodels.formula.api import ols

import matplotlib.pyplot as plt

#funkcja do tworzenia histogramów
def create_hist(data,name):
    sns.distplot(data, norm_hist=False, kde=False)
    plt.ylabel('Count')
    plt.savefig('wykresy/hist_%s.png' % name)
    plt.gcf().clear()

#funkcja do tworzenia wykresów skrzynkowych
def create_box_plot(data,x_value,y_value,name):
    plot= sns.catplot(x=x_value, y=y_value, kind="box", data=data)
    plot.savefig("wykresy/%s.png"%name)
    plt.gcf().clear()



pd.set_option('display.max_columns', None)

df = pd.read_csv('final_data_ecg.csv',sep=';')

df['AE']=abs(df['Rd']-df['Td'])/df['Td']
df['RATIO']=df['Rd']/df['Td']

new_df=pd.DataFrame()

new_df['Rd_sd']=df.groupby(['participant','sound','duration','A2'])['Rd'].std(ddof=0)
#df['Rd_sd']=df.groupby(['participant','sound','duration','A2'])['Rd'].transform(x: x.std(ddof=0))
print(new_df.head(n=15))
new_df['Rd_mean']=df.groupby(['participant','sound','duration','A2'])['Rd'].mean()

new_df['CV']=new_df['Rd_sd']/new_df['Rd_mean']
print(new_df.head(n=15))
# df.to_csv('output2_AE_RATIO.csv',sep=';')
new_df.to_csv('output2_CV.csv',sep=';')

anova=pd.read_csv('output2_CV.csv',sep=';')

results_CV=ols('CV~C(sound)+C(duration)+C(A2)',data=anova).fit()
results_AE=ols('AE~C(sound)+C(duration)+C(A2)',data=df).fit()
results_RATIO=ols('RATIO~C(sound)+C(duration)+C(A2)',data=df).fit()

aov_table_CV=sm.stats.anova_lm(results_CV)
aov_table_RATIO=sm.stats.anova_lm(results_AE)
aov_table_AE=sm.stats.anova_lm(results_RATIO)

print('Anova CV','\n', aov_table_CV,'\n')
print('Anova RATIO','\n',aov_table_RATIO,'\n')
print('Anova AE', '\n', aov_table_AE,'\n')


sns.set()
sns.set_palette("bright")


# create_box_plot(df,"K6","RATIO","K6_RATIO")
# create_box_plot(df,"K6","AE","K6_AE")
# create_box_plot(df,"sound","RATIO","sound_RATIO")
# create_box_plot(df,"sound","AE","sound_AE")
# create_box_plot(anova,"sound","CV","sound_CV")
# create_box_plot(df,"duration","RATIO","duration_RATIO")
# create_box_plot(df,"duration","AE","duration_AE")
# create_box_plot(anova,"duration","CV","duration_CV")
# create_box_plot(df,"A2","AE","A2_AE")
# create_box_plot(anova,"A2","CV","A2_CV")
# create_box_plot(df,"A2","RATIO","A2_RATIO")
#
# create_hist(df['RATIO'],'RATIO')
# create_hist(df['AE'],'AE')
# create_hist(new_df['CV'],'CV')


