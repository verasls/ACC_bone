import numpy as np
import matplotlib.pyplot as plt
from matplotlib.widgets import Cursor
from statistics import mean
from scipy import signal


def plot_acceleration(data, axes):
    """Plots acceleration (g) versus time (cs).

    Args:
        data: A pandas dataframe with accelerometer data. X, Y and Z axes must
            be in the columns 1, 2 and 3, respectively. Time must be in the
            column 0.
        axes: If 1, plots X axis; if 2, plots X and Y axes; if 3 plots X, Y
            and Z axes; if "resultant" computes resultant vector and plots it.

    Returns:
        The acceleration x time plot.

    Raises:
        ValueError: when axes parameter is a non-suported value.
    """

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
    else:
        raise ValueError("axes parameter can only be 1, 2, 3 or resultant")

    # Plot acceleration x time
    fig = plt.figure(figsize=(15, 7))
    ax1 = fig.add_subplot(1, 1, 1)
    if axes == 1:
        ax1.plot(time, aX, label="X axis")
    elif axes == 2:
        ax1.plot(time, aX, label="X axis")
        ax1.plot(time, aY, label="Y axis")
    elif axes == 3:
        ax1.plot(time, aX, label="X axis")
        ax1.plot(time, aY, label="Y axis")
        ax1.plot(time, aZ, label="Z axis")
    elif axes == "resultant":
        ax1.plot(time, resultant, label="Resultant acceleration")

    plt.legend(loc="upper right")
    plt.xlabel("Time (cs)")
    plt.ylabel("Acceleration (g)")
    plt.title("Acceleration X Time")
    plt.show(block=False)


def select_acceleration_ROI(data, axes):
    """Plots acceleration (g) versus time (cs) and uses a GUI to select a
        region of interest in the plot.

    Args:
        data: A pandas dataframe with accelerometer data. X, Y and Z axes must
            be in the columns 1, 2 and 3, respectively. Time must be in the
            column 0.
        axes: If 1, plots X axis; if "resultant" computes resultant vector
            and plots it.

    Returns:
        The acceleration x time plot; a second plot containing only the
        selected region of interest; and two numpy arrays with the region
        of interest acceleration and time values.

    Raises:
        ValueError: when axes parameter is a non-suported value.
    """
    
    # Set data
    time = range(0, len(data))
    time = np.asarray(time)

    if axes == 1:
        acceleration = data.iloc[:, 1]
    elif axes == "resultant":
        acceleration = np.sqrt((data.iloc[:, 1] ** 2) +
                               (data.iloc[:, 2] ** 2) +
                               (data.iloc[:, 3] ** 2))
    else:
        raise ValueError("axes parameter can only be 1 or resultant")

    acceleration = acceleration.to_numpy()

    # Plot acceleration x time
    fig1 = plt.figure(figsize=(15, 7))
    ax11 = fig1.add_subplot(1, 1, 1)
    if axes == 1:
        ax11.plot(time, acceleration, label="X axis")
    elif axes == "resultant":
        ax11.plot(time, acceleration, label="Resultant acceleration")
    else:
        raise ValueError("axes parameter can only be 1 or resultant")

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
        ax21.plot(ROI_time, ROI_acceleration, label="X axis")
    elif axes == "Y":
        ax21.plot(ROI_time, ROI_acceleration, label="Y axis")
    elif axes == "Z":
        ax21.plot(ROI_time, ROI_acceleration, label="Z axis")
    elif axes == "resultant":
        ax21.plot(ROI_time, ROI_acceleration, label="Resultant acceleration")

    plt.legend(loc="upper right")
    plt.xlabel("Time (cs)")
    plt.ylabel("Acceleration (g)")
    plt.title("Selected region of interest")

    plt.show(block=False)

    return(ROI_time, ROI_acceleration)


def filter_acceleration(data, axes, onlyROI=True):
    """Creates a 4th order Butterwoth lowpass filter with a cut-off frequency
    of 20Hz and applies to the acceleration signal.

    Args:
        data: A pandas dataframe with accelerometer data. X, Y and Z axes must
            be in the columns 1, 2 and 3, respectively. Time must be in the
            column 0.
        axes: If 1, plots X axis; if "resultant" computes resultant vector.
        onlyROI: If True (default), uses the select_acceleration_ROI() function
        to select a region of interest before applying the filter.

    Returns:
        The acceleration x time plot; a second plot containing only the
        selected region of interest (if onlyROI is True); a third plot
        containing the raw and filtered acceleration signal; and two numpy
        arrays with the time and filtered acceleration values.

    Raises:
        ValueError: when axes parameter is a non-suported value.
    """
    
    if onlyROI is True:
        # Consider only the selected region of interest
        time, acceleration = select_acceleration_ROI(data, axes)
    else:
        # Set data
        time = range(0, len(data))
        time = np.asarray(time)

        if axes == 1:
            acceleration = data.iloc[:, 1]
            acceleration = acceleration.to_numpy()
        elif axes == "resultant":
            acceleration = np.sqrt((data.iloc[:, 1] ** 2) +
                                   (data.iloc[:, 2] ** 2) +
                                   (data.iloc[:, 3] ** 2))
            acceleration = acceleration.to_numpy()
        else:
            raise ValueError("axes parameter can only be 1 or resultant")

    # Create the lowpass filter for the acceleration signal
    N = 4  # Fourth order
    cutoff = 20  # cut-off frequency (Hz)
    fnyq = 100 / 2  # Nyquist frequency (half of the sampling frequency)
    Wn = cutoff / fnyq  # Digital filter parameter

    z, p, k = signal.butter(N, Wn, btype="low", output="zpk")
    sos = signal.zpk2sos(z, p, k)  # zero-pole-gain design

    # Filter the acceleration signal
    acceleration_filt = signal.sosfiltfilt(sos, acceleration)

    # Plot filtered and unfiltered data
    fig = plt.figure(figsize=(15, 7))
    ax1 = fig.add_subplot(1, 1, 1)
    ax1.plot(time, acceleration, label="Raw acceleration")
    ax1.plot(time, acceleration_filt, label="Filtered acceleration")

    if onlyROI is True:
        plot_title = "Selected region of interest"
    else:
        plot_title = "Acceleration x Time"

    plt.legend(loc="upper right")
    plt.xlabel("Time (cs)")
    plt.ylabel("Acceleration (g)")
    plt.title(plot_title)

    plt.show(block=False)

    return(time, acceleration_filt)


def find_acceleration_peaks(data, axes, onlyROI=True, filteracc=True):
    """Find peaks in the acceleration signal which have a minimum height equal
    to the average acceleration and a minimum distance of 0.4 seconds.

    Args:
        data: A pandas dataframe with accelerometer data. X, Y and Z axes must
            be in the columns 1, 2 and 3, respectively. Time must be in the
            column 0.
        axes: If 1, plots X axis; if "resultant" computes resultant vector.
        onlyROI: If True (default), uses the select_acceleration_ROI() function
        to select a region of interest before finding the peaks.
        filteracc: If True (default), uses the filter_acceleration() function to
        filter the acceleration signal before finding the peaks.

    Returns:
        The acceleration x time plot; a second plot containing only the
        selected region of interest (if onlyROI is True); a third plot
        containing the raw and filtered acceleration signal (if filteracc
        is True).
    """

    
    if onlyROI is True:
        if filteracc is True:
            time, acceleration = filter_acceleration(data, axes, onlyROI=True)
        elif filteracc is False:
            time, acceleration = select_acceleration_ROI(data, axes)
        else:
            raise ValueError("filteracc parameter can only be True or False")
    elif onlyROI is False:
        if filteracc is True:
            time, acceleration = filter_acceleration(data, axes, onlyROI=False)
        elif filteracc is False:
            # Set data
            time = range(0, len(data))
            time = np.asarray(time)

            if axes == 1:
                acceleration = data.iloc[:, 1]
                acceleration = acceleration.to_numpy()
            elif axes == "resultant":
                acceleration = np.sqrt((data.iloc[:, 1] ** 2) +
                                       (data.iloc[:, 2] ** 2) +
                                       (data.iloc[:, 3] ** 2))
                acceleration = acceleration.to_numpy()
            else:
                raise ValueError("axes parameter can only be 1 or resultant")
        else:
            raise ValueError("filteracc parameter can only be True or False")
    else:
        raise ValueError("onlyROI parameter can only be True or False")

    height = mean(acceleration)
    distance = 0.4 * 100  # seconds x sampling frequency
    peaks, _ = signal.find_peaks(acceleration, height=height,
                                 distance=distance)
    idx_peaks = peaks + time[0]

    fig = plt.figure(figsize=(15, 7))
    ax1 = fig.add_subplot(1, 1, 1)
    if axes == 1:
        if filteracc is True:
            ax1.plot(time, acceleration, label="Filtered X axis")
        elif filteracc is False:
            ax1.plot(time, acceleration, label="X axis")
    elif axes == "resultant":
        if filteracc is True:
            ax1.plot(time, acceleration, label="Filtered resultant acceleration")
        elif filteracc is False:
            ax1.plot(time, acceleration, label="Resultant acceleration")
    else:
        raise ValueError("axes parameter can only be 1 or resultant")

    ax1.plot(idx_peaks, acceleration[peaks], "x", color="orange",
              label="Acceleration peaks")

    if onlyROI is True:
        plot_title = "Selected region of interest"
    else:
        plot_title = "Acceleration x Time"

    plt.legend(loc="upper right")
    plt.xlabel("Time (cs)")
    plt.ylabel("Acceleration (g)")
    plt.title(plot_title)

    plt.show(block=False)
