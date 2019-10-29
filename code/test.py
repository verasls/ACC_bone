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

fig1 = plt.figure(figsize=(15, 7))
ax11 = fig1.add_subplot(1, 1, 1)
ax11.plot(time, resultant)
plt.xlabel("Time (cs)")
plt.ylabel("Resultant acceleration (g)")
plt.title("Hello")
# Use the cursor to select a region of the plot
# 2 mouse clicks required
cursor = Cursor(ax11, horizOn=False, useblit=True, color='k', linewidth=1)
coords_1 = plt.ginput(n=1, timeout=0, show_clicks=False)
x1, y1 = coords_1[0]
ax11.axvline(x=x1, color="r")

coords_2 = plt.ginput(n=1, timeout=0, show_clicks=False)
x2, y2 = coords_2[0]
ax11.axvline(x=x2, color="r")

# Plot the subregion selected above
sub_time = range(int(x1), int(x2) + 1)
sub_resultant = resultant[int(x1):int(x2) + 1]

fig2 = plt.figure(figsize=(15, 7))
ax21 = fig2.add_subplot(1, 1, 1)
ax21.plot(sub_time, sub_resultant)
plt.xlabel("Time (cs)")
plt.ylabel("Resultant acceleration (g)")
plt.title("Subplot")

cursor = Cursor(ax21, useblit=True, color='k', linewidth=1)

plt.show()
