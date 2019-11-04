import numpy as np
import matplotlib.pyplot as plt
from matplotlib.widgets import Cursor
from statistics import mean
from scipy.signal import find_peaks


def plot_acceleration(data, axes):
    # Set data
    time = range(0, len(data))
    time = np.asarray(time)

    if axes == 1:
        aX = data.iloc[:, 1]
        aX = aX.to_numpy()
    elif axes == 2:
        aX = data.iloc[:, 1]
        aY = data.iloc[:, 2]
        aX = aX.to_numpy()
        aY = aY.to_numpy()
    elif axes == 3:
        aX = data.iloc[:, 1]
        aY = data.iloc[:, 2]
        aZ = data.iloc[:, 3]
        aX = aX.to_numpy()
        aY = aY.to_numpy()
        aZ = aZ.to_numpy()
    elif axes == "resultant":
        resultant = np.sqrt((data.iloc[:, 1] ** 2) +
                            (data.iloc[:, 2] ** 2) +
                            (data.iloc[:, 3] ** 2))
        resultant = resultant.to_numpy()

    # Plot acceleration x time
    fig = plt.figure(figsize=(15, 7))
    ax1 = fig.add_subplot(1, 1, 1)
    if axes == 1:
        ax1.plot(time, aX, "b-", label="X axis")
    elif axes == 2:
        ax1.plot(time, aX, "b-", label="X axis")
        ax1.plot(time, aY, "r-", label="Y axis")
    elif axes == 3:
        ax1.plot(time, aX, "b-", label="X axis")
        ax1.plot(time, aY, "r-", label="Y axis")
        ax1.plot(time, aZ, "g-", label="Z axis")
    elif axes == "resultant":
        ax1.plot(time, resultant, "b-", label="Resultant acceleration")

    plt.legend(loc="upper right")
    plt.xlabel("Time (cs)")
    plt.ylabel("Acceleration (g)")
    plt.title("Acceleration X Time")
    plt.show(block=False)


def select_acceleration_ROI(data, axes):
    # Set data
    time = range(0, len(data))
    time = np.asarray(time)

    if axes == "X":
        acceleration = data.iloc[:, 1]
    elif axes == "Y":
        acceleration = data.iloc[:, 2]
    elif axes == "Z":
        acceleration = data.iloc[:, 3]
    elif axes == "resultant":
        acceleration = np.sqrt((data.iloc[:, 1] ** 2) +
                               (data.iloc[:, 2] ** 2) +
                               (data.iloc[:, 3] ** 2))
    acceleration = acceleration.to_numpy()

    # Plot acceleration x time
    fig1 = plt.figure(figsize=(15, 7))
    ax11 = fig1.add_subplot(1, 1, 1)
    if axes == "X":
        ax11.plot(time, acceleration, "b-", label="X axis")
    elif axes == "Y":
        ax11.plot(time, acceleration, "b-", label="Y axis")
    elif axes == "Z":
        ax11.plot(time, acceleration, "b-", label="Z axis")
    elif axes == "resultant":
        ax11.plot(time, acceleration, "b-", label="Resultant acceleration")

    plt.legend(loc="upper right")
    plt.xlabel("Time (cs)")
    plt.ylabel("Acceleration (g)")
    plt.title("Click on the plot to select the limits of"
              "the region of interest\n"
              "Zoom or pan to view, press spacebar when ready to click")

    # Use the cursor to select a region of interest in the plot
    cursor = Cursor(ax11, horizOn=False, useblit=True, color="k", linewidth=1)
    zoom_ok = False
    while not zoom_ok:
        zoom_ok = plt.waitforbuttonpress()

    coords_1 = plt.ginput(n=1, timeout=0, show_clicks=False)
    x1, y1 = coords_1[0]
    ax11.axvline(x=x1, color="r")

    coords_2 = plt.ginput(n=1, timeout=0, show_clicks=False)
    x2, y2 = coords_2[0]
    ax11.axvline(x=x2, color="r")

    # Plot the selected region of interest
    ROI_time = range(int(x1), int(x2) + 1)
    ROI_time = np.asarray(ROI_time)

    ROI_acceleration = acceleration[int(x1):int(x2) + 1]

    fig2 = plt.figure(figsize=(15, 7))
    ax21 = fig2.add_subplot(1, 1, 1)
    if axes == "X":
        ax21.plot(ROI_time, ROI_acceleration, "b-", label="X axis")
    elif axes == "Y":
        ax21.plot(ROI_time, ROI_acceleration, "b-", label="Y axis")
    elif axes == "Z":
        ax21.plot(ROI_time, ROI_acceleration, "b-", label="Z axis")
    elif axes == "resultant":
        ax21.plot(ROI_time, ROI_acceleration, "b-",
                  label="Resultant acceleration")

    plt.legend(loc="upper right")
    plt.xlabel("Time (cs)")
    plt.ylabel("Acceleration (g)")
    plt.title("Selected region of interest")

    cursor = Cursor(ax21, useblit=True, color='k', linewidth=1)

    plt.show(block=False)

    return(ROI_time, ROI_acceleration)


def find_acceleration_peaks(data, axes, onlyROI=True):
    # Find peaks in the acceleration signal

    if onlyROI is True:
        # Consider only the selected region of interest
        time, acceleration = select_acceleration_ROI(data, axes)
    else:
        # Set data
        time = range(0, len(data))
        time = np.asarray(time)

        if axes == "X":
            acceleration = data.iloc[:, 1]
        elif axes == "Y":
            acceleration = data.iloc[:, 2]
        elif axes == "Z":
            acceleration = data.iloc[:, 3]
        elif axes == "resultant":
            acceleration = np.sqrt((data.iloc[:, 1] ** 2) +
                                   (data.iloc[:, 2] ** 2) +
                                   (data.iloc[:, 3] ** 2))
        acceleration = acceleration.to_numpy()

    height = mean(acceleration)
    distance = 0.4 * 100  # seconds x sampling frequency
    peaks, _ = find_peaks(acceleration, height=height, distance=distance)
    idx_peaks = peaks + time[0]

    fig3 = plt.figure(figsize=(15, 7))
    ax31 = fig3.add_subplot(1, 1, 1)
    if axes == "X":
        ax31.plot(time, acceleration, "b-", label="X axis")
    elif axes == "Y":
        ax31.plot(time, acceleration, "b-", label="Y axis")
    elif axes == "Z":
        ax31.plot(time, acceleration, "b-", label="Z axis")
    elif axes == "resultant":
        ax31.plot(time, acceleration, "b-",
                  label="Resultant acceleration")

    ax31.plot(idx_peaks, acceleration[peaks], "x", color="orange",
              label="Acceleration peaks")

    if onlyROI is True:
        plot_title = "Selected region of interest"
    else:
        plot_title = "Acceleration x Time"

    plt.legend(loc="upper right")
    plt.xlabel("Time (cs)")
    plt.ylabel("Acceleration (g)")
    plt.title(plot_title)

    cursor = Cursor(ax31, useblit=True, color='k', linewidth=1)

    plt.show(block=False)
