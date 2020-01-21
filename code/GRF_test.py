import numpy as np
from scipy import signal
from statistics import mean
import matplotlib
matplotlib.use('MacOSX')
import matplotlib.pyplot as plt

# Read data
path = "../data/119/2017-12-09_Jumps_5cm_1_m1_119.txt"
data = np.loadtxt(path, delimiter=",")
samp_freq = 1000  # sample frequency (Hz)

# Get variables of interest
body_mass = 74  # subject's mass (kg)
g = 9.81  # gravity acceleration (m/s2)

body_weight = body_mass * g  # body weight (BW; N)

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
force_BW = force / body_weight  # ground reaction force (multiples of BW)

# Find peaks
height = 3 * mean(force_BW)
distance = 0.4 * samp_freq  # seconds * sampling frequency
peaks, properties = signal.find_peaks(force_BW, height=height,
                                      distance=distance)
time_of_peaks = peaks / samp_freq

# Divide bouts
print("Peaks found:", peaks)
# Compute the peaks horizontal distance to its left neighbour
widths = []
for i in range(1, len(peaks)):
    widths.append(peaks[i] - peaks[i - 1])
print("Widths:", widths)
print("Max width:", max(widths))
max_width = widths.index(max(widths)) + 1
print("Peak with max width:", peaks[max_width])

# Mark first and last peaks of the bouts
first_peaks = np.array([peaks[0], peaks[max_width]])
last_peaks = np.array([peaks[max_width - 1], peaks[len(peaks) - 1]])
time_first_peaks = first_peaks / samp_freq
time_last_peaks = last_peaks / samp_freq

# Plot
fig = plt.figure(figsize=(15, 7))
ax1 = fig.add_subplot(1, 1, 1)
ax1.plot(time, force_BW)
ax1.plot(time_of_peaks, force_BW[peaks], "x")
ax1.plot(time_first_peaks, force_BW[first_peaks], "x")
ax1.plot(time_last_peaks, force_BW[last_peaks], "x")
plt.xticks(np.arange(min(time), max(time) + 1, 5))
plt.yticks(np.arange(0, 6, 0.5))
plt.grid()
plt.show()

# Osteogenic index (OI) variables
# Peak-to-peak strain magnitude
# E = force_BW[peaks]
# print("Peak-to-peak strain magnitude:", E)
