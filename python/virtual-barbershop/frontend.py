import dash
from dash import dcc, html
from dash.dependencies import Input, Output, State
import plotly.graph_objs as go
import plotly.express as px
import pandas as pd

def create_app(shop):
    app = dash.Dash(__name__)

    app.layout = html.Div([
        html.H1("Virtual Barbershop Simulation"),
        html.Div([
            html.Div([
                dcc.Graph(id='barbers-status'),
                dcc.G
                raph(id='waiting-room'),
            ], style={'width': '50%', 'display': 'inline-block'}),
            html.Div([
                dcc.Graph(id='customer-satisfaction'),
                dcc.Graph(id='service-distribution'),
            ], style={'width': '50%', 'display': 'inline-block'}),
        ]),
        html.Div([
            html.H3("Latest Conversations"),
            html.Div(id='conversation-log'),
        ]),
        dcc.Interval(id='interval-component', interval=2000, n_intervals=0),
    ])

    @app.callback(
        [Output('barbers-status', 'figure'),
         Output('waiting-room', 'figure'),
         Output('customer-satisfaction', 'figure'),
         Output('service-distribution', 'figure'),
         Output('conversation-log', 'children')],
        [Input('interval-component', 'n_intervals')]
    )
    def update_graphs(n):
        barber_data = pd.DataFrame({
            'Barber': [b.name for b in shop.barbers],
            'Status': ['Available' if b.is_available else 'Busy' for b in shop.barbers],
            'Skill': [b.skill_level for b in shop.barbers]
        })
        barbers_fig = px.bar(barber_data, x='Barber', y='Skill', color='Status', title="Barbers Status")
        waiting_fig = go.Figure(go.Indicator(
            mode = "gauge+number",
            value = len(shop.waiting_room),
            domain = {'x': [0, 1], 'y': [0, 1]},
            title = {'text': "Waiting Customers"},
            gauge = {'axis': {'range': [0, shop.waiting_room.maxlen]}}
        ))
        if shop.total_customers > 0:
            satisfaction_rate = shop.satisfied_customers / shop.total_customers
        else:
            satisfaction_rate = 0
        satisfaction_fig = go.Figure(go.Indicator(
            mode = "gauge+number",
            value = satisfaction_rate * 100,
            domain = {'x': [0, 1], 'y': [0, 1]},
            title = {'text': "Customer Satisfaction (%)"},
            gauge = {'axis': {'range': [0, 100]}}
        ))
        service_data = pd.DataFrame([c.desired_service for c in shop.waiting_room])
        service_fig = px.pie(service_data, names=0, title="Requested Services")
        conversations = [html.P(conv) for conv in shop.conversations[-5:]]
        return barbers_fig, waiting_fig, satisfaction_fig, service_fig, conversations

    return app