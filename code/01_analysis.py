import pandas as pd
from test import find_acceleration_peaks

data = pd.read_csv("../data/ID_119_hip_RAW.csv", delimiter=",", skiprows=10)

find_acceleration_peaks(data, "resultant")
