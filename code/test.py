import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

data = pd.read_csv("../data/ID_119_hip_RAW.csv", delimiter=",", skiprows=10)

# print("Initiating conversion from string to datetime")
# data["Timestamp"] = pd.to_datetime(data["Timestamp"])
# print("Conversion done!")

time = range(0, len(data))
resultant = np.sqrt((data.iloc[:, 1] ** 2) +
                    (data.iloc[:, 2] ** 2) +
                    (data.iloc[:, 3] ** 2))

plt.plot(time, resultant)
plt.xlabel("Time (cs)")
plt.ylabel("Resultant acceleration (g)")
plt.show()
