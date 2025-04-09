# ----- required imports -----

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# ----- helper functions -----

def calculate_points(theta):
    ax = base_x + a * np.cos(theta)
    ay = base_y + a * np.sin(theta)
    bx = ax + b * np.cos(theta + np.pi / 4)  
    by = ay + b * np.sin(theta + np.pi / 4)
    cx = bx + c * np.cos(theta - np.pi / 6)  
    cy = by + c * np.sin(theta - np.pi / 6)
    dx = cx + d * np.cos(theta + np.pi / 3)  
    dy = cy + d * np.sin(theta + np.pi / 3)
    return (ax, ay), (bx, by), (cx, cy), (dx, dy)

def animate(frame):
    theta = frame * 2 * np.pi / 100
    (ax_pos, ay_pos), (bx_pos, by_pos), (cx_pos, cy_pos), (dx_pos, dy_pos) = calculate_points(theta)
    line1.set_data([base_x, ax_pos], [base_y, ay_pos])
    line2.set_data([ax_pos, bx_pos], [ay_pos, by_pos])
    line3.set_data([bx_pos, cx_pos], [by_pos, cy_pos])
    joint_points.set_data(
        [base_x, ax_pos, bx_pos, cx_pos],
        [base_y, ay_pos, by_pos, cy_pos]
    )
    return line1, line2, line3, joint_points

# ----- execution code -----

a = 38  
b = 41.5
c = 39.3
d = 40.1
e = 55.8
f = 39.4
g = 36.7
h = 65.7
base_x, base_y = 0, 0


fig, ax = plt.subplots()
ax.set_xlim(-150, 150)
ax.set_ylim(-150, 150)
line1, = ax.plot([], [], 'r-', lw=2)  
line2, = ax.plot([], [], 'g-', lw=2)  
line3, = ax.plot([], [], 'b-', lw=2)  
joint_points, = ax.plot([], [], 'ko')  

anim = FuncAnimation(fig, animate, frames=100, interval=50, blit=True)
plt.title("Strandbeest Foot Mechanism Visualization")
plt.xlabel("X position")
plt.ylabel("Y position")
plt.grid(True)
plt.show()
