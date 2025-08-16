import functools
from werkzeug.security import check_password_hash, generate_password_hash
from flaskr.db import get_db
from flask import (
    Blueprint, flash, g, redirect, render_template, request, session, url_for
) 

"""
every call of the render_template function 
is contingent on an existing html template 
being provided

running the server without creating these 
and then trying to access the url will result 
in a TemplateNotFound error
"""

bp = Blueprint('auth', __name__, url_prefix='/auth')

"""
when using a blueprint, the name of the 
blueprint is prepended to the name of the 
function
"""

# ------

@bp.route('/register', methods=('GET', 'POST'))
def register():

    if request.method == 'POST':
        username = request.form['username']
        password = request.form['password']
        db = get_db()
        error = None

        if not username:
            error = 'Username is required.'
        elif not password:
            error = 'Password is required.'

        if error is None:
            try:
                db.execute(
                    "INSERT INTO user (username, password) VALUES (?, ?)",
                    (username, generate_password_hash(password)),
                ) # try to run this command on the SQL db
                db.commit()
            except db.IntegrityError: # error hit, specify SQL generated error resulting from IntegrityError
                error = f"User {username} is already registered."
            else:

                """
                url_for() function generates the URL to 
                a view based on a name and arguments. 

                name associated with a view is also 
                called the endpoint, and by default it’s 
                the same as the name of the view function.
                """

                return redirect(url_for("auth.login"))

        flash(error)

    return render_template('auth/register.html')

# -----


"""
when we later call this endpoint we can reference 
it with 'auth.login' because you added it to 
the 'auth' blueprint 
"""
@bp.route('/login', methods=('GET', 'POST'))
def login():

    if request.method == 'POST':
        username = request.form['username']
        password = request.form['password']
        db = get_db()
        error = None
        user = db.execute(
            'SELECT * FROM user WHERE username = ?', (username,)
        ).fetchone() # fetch a single user where there is a corresponding username, anyway this is valid since there's only ever one username in a db
        
        """
        fetchone() returns one row from 
        the query. If the query returned no 
        results, it returns None. Later, 
        fetchall() will be used, which returns 
        a list of all results. 
        """

        if user is None:
            error = 'Incorrect username.'
        elif not check_password_hash(user['password'], password): # similar to hash function in php for passwords
            error = 'Incorrect password.'

        if error is None:
            session.clear() 
           
            """
            session is a superglobal that stores 
            data across requests. When validation 
            succeeds, the user’s id is stored in a 
            new session. 
            
            data is stored in a cookie that is sent 
            to the browser

            the browser then sends it back with 
            subsequent requests
            
            Flask securely signs the data so that 
            it can’t be tampered with. We've seen this 
            before in other PHP related things
            """

            session['user_id'] = user['id']
            return redirect(url_for('index')) # once again redirects to the index.html file as required

        flash(error)

    return render_template('auth/login.html')

# -----

@bp.before_app_request
def load_logged_in_user():

    """
    bp.before_app_request() registers a function 
    that runs before the view function, no matter 
    what URL is requested. load_logged_in_user checks 
    if a user id is stored in the session and gets that 
    user’s data from the database, storing it on 
    g.user, which lasts for the length of the request. 

    If there is no user id, or if the id doesn’t exist, 
    g.user will be None.
    """

    user_id = session.get('user_id') # here .get() can be used because we've already logged the user in and can determine whether their state is logged in per the existing session variable

    if user_id is None:
        g.user = None
    else:
        g.user = get_db().execute(
            'SELECT * FROM user WHERE id = ?', (user_id,)
        ).fetchone()

# -----

@bp.route('/logout') # self-explanatory
def logout():
    session.clear()
    return redirect(url_for('index'))

# -----

def login_required(view):
    @functools.wraps(view)

    def wrapped_view(**kwargs):
        if g.user is None:
            return redirect(url_for('auth.login'))
        return view(**kwargs)

    return wrapped_view
