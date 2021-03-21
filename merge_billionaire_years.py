import pandas as pd


m_file = open("male.txt", "r")
males = m_file.readlines()
males = [w.replace('\n', '').lower() for w in males]
f_file = open("female.txt", "r")
females = f_file.readlines()
females = [w.replace('\n', '').lower() for w in females]


def get_gender(row):
    
    first_name = row['name'].split()[0]

    if (first_name in males):
        gender = "male" 
    elif (first_name in females):
        gender = "female"
    else:
        gender = "unknown"

    return gender

data_2018 = pd.read_csv('billionaires_2018b.csv', sep=',')
data_2020 = pd.read_csv('billionaires_2020b.csv', sep=',')
data_2018['gender'] = data_2018.apply (lambda row: get_gender(row), axis=1)
data_2020['gender'] = data_2020.apply (lambda row: get_gender(row), axis=1)


inner_merged_total = pd.merge(data_2018, data_2020, on=["name"])
inner_merged_total.to_csv( "merged_billionaires_data.csv")


