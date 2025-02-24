# ----- required imports -----

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# ----- helper functions -----

def calculate_point(theta):
    x1 = a * np.cos(theta)
    y1 = a * np.sin(theta)
    x2 = c * np.cos(theta) + l
    y2 = c * np.sin(theta)
    x3 = x2 + e * np.cos(np.arctan2(y2, x2) + np.arccos((x2**2 + y2**2 + e**2 - h**2) / (2 * e * np.sqrt(x2**2 + y2**2))))
    y3 = y2 + e * np.sin(np.arctan2(y2, x2) + np.arccos((x2**2 + y2**2 + e**2 - h**2) / (2 * e * np.sqrt(x2**2 + y2**2))))
    return x3, y3

def animate(frame):
    theta = frame * 2 * np.pi / 100
    x, y = calculate_point(theta)
    line.set_data(line.get_xdata() + [x], line.get_ydata() + [y])
    point.set_data([x], [y])  
    return line, point

# ----- execution code -----

a = 38
b = 41.5
c = 39.3
d = 40.1
e = 55.8
f = 39.4
g = 36.7
h = 65.7
i = 49
j = 50
k = 61.9
l = 7.8
m = 15

fig, ax = plt.subplots()
ax.set_xlim(-100, 100)
ax.set_ylim(-100, 100)
line, = ax.plot([], [], 'b-')
point, = ax.plot([], [], 'ro')

anim = FuncAnimation(fig, animate, frames=100, interval=50, blit=True)

plt.title("Strandbeest Foot Trajectory")
plt.xlabel("X position")
plt.ylabel("Y position")
plt.grid(True)
plt.show()
