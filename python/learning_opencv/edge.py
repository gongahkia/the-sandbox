import cv2

HOLDS_FILEPATH = "holds.png"
TARGET_FILEPATH = "detected_holds_output.jpeg"

img = cv2.imread(HOLDS_FILEPATH)

edges = cv2.Canny(img, 100, 200)

cv2.imshow(HOLDS_FILEPATH, img)
cv2.imshow(TARGET_FILEPATH, edges)
cv2.imwrite(TARGET_FILEPATH, edges)

cv2.waitKey(0)
cv2.destroyAllWindows()