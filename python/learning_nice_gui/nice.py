from nicegui import ui
import plotly.graph_objects as go
import numpy as np

def create_graph():
    x = np.linspace(0, 10, 100)
    y = np.sin(x)
    fig = go.Figure()
    fig.add_trace(go.Scatter(x=x, y=y, mode='lines', name='Sine Wave'))
    fig.update_layout(title='Sine Wave Graph', xaxis_title='X', yaxis_title='Y')
    return fig

def on_button_click():
    user_input = input_field.value  
    message_label.set_text(f'Ni hao, {user_input}!')  
    markdown_display.set_content(f'# Hello, {user_input}!\n\nWelcome to NiceGUI with Graphs and Markdown, some nerd shit for ya!')

ui.label('Welcome to the library known as NiceGUI!')  
input_field = ui.input(placeholder='Oi can you enter your name anot?')  
ui.button('Greet me lah walah :(', on_click=on_button_click)  
message_label = ui.label('')  
markdown_display = ui.markdown('**Your Markdown will appear here!** Such as `this code block` and this ***text*** and also this *too*')
graph = ui.plotly(create_graph())  
ui.run()