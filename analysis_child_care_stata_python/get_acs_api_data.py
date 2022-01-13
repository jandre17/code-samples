### packages
import requests
import json
import pandas as pd
import os

### current working directory
cwd = os.getcwd()

### prep ACS API call
# API guidance: https://www.census.gov/programs-surveys/acs/guidance/handbooks/api.html
# details: https://api.census.gov/data/2019/acs/acs5.html
# variables: https://api.census.gov/data/2019/acs/acs5/variables.html
variables = {'B01001_003E':'pop_under5_male',
             'B01001_027E':'pop_under5_female'}

variables_keys = ','.join(variables.keys())

url = (f'''https://api.census.gov/data/2019/acs/acs5?get={variables_keys}&for=state%20legislative%20district%20(upper%20chamber):*&in=state:11''')

# request and format data
headers = {'Content-Type': 'application/json'}
response = requests.get(url, headers = headers)
if response.status_code == 200:
    data = json.loads(response.content.decode('utf-8'))

# to dataframe, rename columns
df = pd.DataFrame(data)
df = df.rename(columns = df.iloc[0]).drop([0]).rename(columns = variables)

# columns to numeric
df[df.columns[0:2]] = df[df.columns[0:2]].apply(pd.to_numeric)

# calculate total population under age 5
df['pop_under5'] = df['pop_under5_male'] + df['pop_under5_female']

# drop unneeded columns
df = df.drop(columns = ['state', 'pop_under5_male', 'pop_under5_female'], axis = 1)

# checks
df.head()
df.shape
df.dtypes
sum(df['pop_under5'])

# export to CSV
df.to_csv(os.path.join(cwd, "data", "acs_ward_under5_pop.csv"), index = False)