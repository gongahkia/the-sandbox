from PIL import Image
import pytesseract

pytesseract.pytesseract.tesseract_cmd = '/usr/bin/tesseract'
print(pytesseract.image_to_string(Image.open('test.jpg')))
print(pytesseract.image_to_string('test.jpg'))
print(pytesseract.get_languages(config=''))
print(pytesseract.image_to_string(Image.open('test-european.jpg'), lang='fra'))
print(pytesseract.image_to_string('images.txt'))
try:
    print(pytesseract.image_to_string('test.jpg', timeout=2)) # Timeout after 2 seconds
    print(pytesseract.image_to_string('test.jpg', timeout=0.5)) # Timeout after half a second
except RuntimeError as timeout_error:
    pass

print(pytesseract.image_to_boxes(Image.open('test.jpg')))
print(pytesseract.image_to_data(Image.open('test.jpg')))
print(pytesseract.image_to_osd(Image.open('test.jpg')))
hocr = pytesseract.image_to_pdf_or_hocr('test.jpg', extension='hocr')
xml = pytesseract.image_to_alto_xml('test.jpg')

text, boxes = pytesseract.run_and_get_multiple_output('test.jpg', extensions=['txt', 'box'])
