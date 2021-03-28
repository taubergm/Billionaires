import pandas as pd

data_2020 = pd.read_csv('american_billionaires_1.csv', sep=',')
data_2021 = pd.read_csv('americans_2021.csv', sep=',')

print(data_2021)

inner_merged_total = pd.merge(data_2020, data_2021, on=["name"])
inner_merged_total.to_csv( "american_billionaires_data.csv")


