import os

from flask import Flask

def create_app(test_config=None):
    """
    factory function that creates an instance
    of a flask application whenever its called
    """
    app = Flask(__name__, instance_relative_config=True)
    app.config.from_mapping(
        SECRET_KEY='dev',
        DATABASE=os.path.join(app.instance_path, 'flaskr.sqlite'),
    )

    if test_config is None:
        app.config.from_pyfile('config.py', silent=True)
    else:
        app.config.from_mapping(test_config)

    try:
        os.makedirs(app.instance_path)
    except OSError:
        pass

    @app.route('/hello')
    def hello():

        """
        creates the hello view that can 
        be later referenced via url_for()
        """

        return 'Hello, World!'

    from . import db # calls the local python file (module)
    db.init_app(app)

    from . import auth # calls the local python file (module)
    app.register_blueprint(auth.bp) # registers the given view with the blueprint

    from . import blog
    app.register_blueprint(blog.bp) # registers the given view with the blueprint
    app.add_url_rule('/', endpoint='index') 

    return app

