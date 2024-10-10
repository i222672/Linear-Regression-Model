import pandas as pd
from sklearn.linear_model import LinearRegression

# Load the data from the CSV file
data = pd.read_csv('C:/Users/User/Downloads/FASTNU Semester 4/P&S/archive(1)/archive (1)/data_weather/vlore/vlore2021.csv')

# Fit the linear regression model
model = LinearRegression()
model.fit(data[['wspd']].dropna(), data['prcp'].dropna())

# Calculate the coefficient of determination (R²)
r_squared = model.score(data[['wspd']].dropna(), data['prcp'].dropna())

print(f"The coefficient of determination (R²) for prcp and wspd is: {r_squared:.2f}")
