import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.widgets import Cursor

data = pd.read_csv("../data/ID_119_hip_RAW.csv", delimiter=",", skiprows=10)

# print("Initiating conversion from string to datetime")
# data["Timestamp"] = pd.to_datetime(data["Timestamp"])
# print("Conversion done!")

# Plot acceleration x time
time = range(0, len(data))
resultant = np.sqrt((data.iloc[:, 1] ** 2) +
                    (data.iloc[:, 2] ** 2) +
                    (data.iloc[:, 3] ** 2))

fig = plt.figure(figsize=(15, 7))
ax = fig.add_subplot(1, 1, 1)
ax.plot(time, resultant)
plt.xlabel("Time (cs)")
plt.ylabel("Resultant acceleration (g)")
plt.title("Hello")
# Use the cursor to select a region of the plot
# 2 mouse clicks required
cursor = Cursor(ax, horizOn=False, useblit=True, color='k', linewidth=1)
coords_1 = plt.ginput(n=1, timeout=0, show_clicks=False)
x1, y1 = coords_1[0]
ax.axvline(x=x1, color="r")

coords_2 = plt.ginput(n=1, timeout=0, show_clicks=False)
x2, y2 = coords_2[0]
ax.axvline(x=x2, color="r")

plt.show()
