import pandas as pd
from procacc import filter_acceleration

data = pd.read_csv("../data/ID_119_hip_RAW.csv", delimiter=",", skiprows=10)

filter_acceleration(data, "resultant")
