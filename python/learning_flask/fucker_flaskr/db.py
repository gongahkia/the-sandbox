import sqlite3

import click
from flask import current_app, g

"""
g is a special object that is unique for 
each request. It is used to store data that 
might be accessed by multiple functions during 
the request. The connection is stored and reused 
instead of creating a new connection if get_db 
is called a second time in the same request.
"""

def get_db():
    if 'db' not in g:
        g.db = sqlite3.connect(
            current_app.config['DATABASE'],
            detect_types=sqlite3.PARSE_DECLTYPES
        ) # establish a connection to the db
        g.db.row_factory = sqlite3.Row # formats sql rows as python dictionaries
    return g.db

def close_db(e=None):
    db = g.pop('db', None)
    if db is not None:
        db.close()

"""
these below commands run to interact 
with the db, specifically to initialise 
and clsoe them 
"""

def init_db():
    db = get_db()
    with current_app.open_resource('schema.sql') as f:
        db.executescript(f.read().decode('utf8')) # executes the sql script schema.sql that creates the db and its internal posts

@click.command('init-db')
def init_db_command():
    """
    clear the existing data and 
    create new tables by defining 
    a CLI command called init-db 
    that calls the init_db function 
    and shows a success message to the user
    """
    init_db()
    click.echo('Initialized the database.')

def init_app(app):
    app.teardown_appcontext(close_db) # tells Flask to call that function when cleaning up after returning the response
    app.cli.add_command(init_db_command) # adds a new command that can be called with the flask command