from bottle import Bottle, run, template, static_file

app = Bottle()

@app.route('/')
def home():
    return template('index.html', title='Welcome to my animal crossing new evenings', message='Hello world, Hello K.K Slider!')

@app.route('/static/<filepath:path>')
def server_static(filepath):
    return static_file(filepath, root='./static/')

if __name__ == '__main__':
    run(app, host='localhost', port=8080)
    