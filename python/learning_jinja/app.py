from flask import Flask, render_template

app = Flask(__name__)

@app.route('/')
def home():
    return render_template('index.html', title='Abang Heimmer', message='Kagurabachi Rokuhara but in real life!', image_url='images/killy.jpg')

if __name__ == '__main__':
    app.run(debug=True)