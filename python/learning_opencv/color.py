import cv2
import numpy as np

IMAGE_FILEPATH = "color_wheel.jpeg"
TARGET_FILEPATH = "detected_color_output.jpeg"

image = cv2.imread(IMAGE_FILEPATH)
image = cv2.resize(image, (700, 600))

hsv_image = cv2.cvtColor(image, cv2.COLOR_BGR2HSV)

lower_red = np.array([0, 100, 100]) # lower bound of red in hsv
upper_red = np.array([10, 255, 255]) # upper bound of red in hsv

mask_red = cv2.inRange(hsv_image, lower_red, upper_red)

lower_blue = np.array([100, 150, 0]) # lower bound of blue in hsv
upper_blue = np.array([140, 255, 255]) # upper bound of blue in hsv

mask_blue = cv2.inRange(hsv_image, lower_blue, upper_blue)

lower_green = np.array([40, 100, 100]) # lower bound of green in hsv
upper_green = np.array([80, 255, 255]) # upper bound of green in hsV

mask_green = cv2.inRange(hsv_image, lower_green, upper_green)

mask_combined = mask_red | mask_blue | mask_green

detected_output = cv2.bitwise_and(image, image, mask=mask_combined)

cv2.imshow(IMAGE_FILEPATH, image)
cv2.imshow(TARGET_FILEPATH, detected_output)
cv2.imwrite(TARGET_FILEPATH, detected_output)

cv2.waitKey(0)
cv2.destroyAllWindows()