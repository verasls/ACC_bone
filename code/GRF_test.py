import numpy as np
from scipy import signal
from statistics import mean
import matplotlib
matplotlib.use('MacOSX')
import matplotlib.pyplot as plt

# Imput from user ------------------------------------------------------------

# Path to data:
path = "../data/119/2017-12-09_Jumps_5cm_1_m1_119.txt"
# Subject's body mass (kg)
body_mass = 74
# Sample frequency (Hz)
samp_freq = 1000
# Minimum time to consider an interval (s)
threshold = 5 * samp_freq

# ----------------------------------------------------------------------------

# Read data
data = np.loadtxt(path, delimiter=",")

force = data[:, 2]  # ground reaction force (GRF; N)
time = np.array(range(1, len(force) + 1))
time = time / samp_freq  # time in seconds

# Filter GRF data
# Create the lowpass filter
N = 4  # Filter order
cutoff = 20  # cut-off frequency (Hz)
fnyq = samp_freq / 2  # # Nyquist frequency (half of the sampling frequency)
Wn = cutoff / fnyq  # Filter parameter

b, a = signal.butter(N, Wn, btype="low")

# Process GRF signal
force = signal.filtfilt(b, a, force)

# Get GRF in BW
g = 9.81  # gravity acceleration (m/s2)
body_weight = body_mass * g  # body weight (BW; N)
force_BW = force / body_weight  # ground reaction force (multiples of BW)

# Find peaks
height = 3 * mean(force_BW)
distance = 0.4 * samp_freq  # seconds * sampling frequency
peaks, properties = signal.find_peaks(force_BW, height=height,
                                      distance=distance)
time_of_peaks = peaks / samp_freq

# Divide bouts
# Vector indicating the peaks horizontal distance to its left neighbour
widths = []
# Vector indicating the widths greater than the established threshold
above_threshold = []
# Index of the widths in above_threshold
interval = []
for i in range(1, len(peaks)):
    widths.append(peaks[i] - peaks[i - 1])
for i in range(0, len(widths)):
    if widths[i] >= threshold:
        above_threshold.append(widths[i])
if len(above_threshold) == 1:
    interval = widths.index(above_threshold) + 1
elif len(above_threshold) > 1:
    for i in range(0, len(above_threshold)):
        interval.append(widths.index(above_threshold[i]) + 1)

# Mark first peaks of the bouts
if isinstance(interval, int) is True:
    first_peaks = np.array([peaks[0], peaks[interval]])
else:
    first_peaks = [peaks[0]]
    for i in range(0, len(interval)):
        first_peaks.append(peaks[interval[i]])
    first_peaks = np.asarray(first_peaks)

# Mark last peaks of the bouts
if isinstance(interval, int) is True:
    last_peaks = np.array([peaks[interval - 1], peaks[len(peaks) - 1]])
else:
    last_peaks = []
    for i in range(0, len(interval)):
        last_peaks.append(peaks[interval[i] - 1])
    last_peaks.append(peaks[len(peaks) - 1])
    last_peaks = np.asarray(last_peaks)

time_first_peaks = first_peaks / samp_freq
time_last_peaks = last_peaks / samp_freq

# Group peaks in bouts
# b = number of bouts
if isinstance(interval, int) is True:
    b = 2
else:
    b = interval.size + 1
# Put the peaks in a dictionary
bouts = {}
for i in range(1, b + 1):
    first_peak_idx = int(*np.where(peaks == first_peaks[i - 1]))
    last_peak_idx = int(*np.where(peaks == last_peaks[i - 1]))
    bout = peaks[first_peak_idx:last_peak_idx + 1]

    key_name = "bout_" + str(i)
    bouts[key_name] = bout

# Plot
fig = plt.figure(figsize=(15, 7))
ax1 = fig.add_subplot(1, 1, 1)
ax1.plot(time, force_BW)
ax1.plot(time_of_peaks, force_BW[peaks], "x", label="Ground reaction force "
         "peaks")
ax1.plot(time_first_peaks, force_BW[first_peaks], "x", label="First peak of "
         "the bout")
ax1.plot(time_last_peaks, force_BW[last_peaks], "x", label="Last peak of the "
         "bout")
plt.xticks(np.arange(min(time), max(time) + 1, 5))
plt.yticks(np.arange(0, 6, 0.5))
plt.grid()
plt.legend(loc="upper right")
plt.xlabel("Time (s)")
plt.ylabel("Vertical ground reaction force (BW)")
plt.show()

# Peak magnitude
E = force_BW[peaks]
print("Peak-to-peak strain magnitude (BW):", E)
# Number of bouts
print("Number of bouts:", b)
# Time between bouts
t = time_last_peaks[0] - time_first_peaks[1]
print("Time between bouts (s):", round(t, 2))
print("Time between bouts (h):", round(t / 3600, 4))
# Number of cycles (total)
print("Number of cycles (total):", len(peaks))
# Number of cycles (per bout)
print("Number of cycles (per bout):")
for i in range(0, len(bouts.keys())):
    message = "Bout " + str(i + 1) + ": "
    message = message + str(len(bouts[list(bouts.keys())[i]]))
    print(message)
