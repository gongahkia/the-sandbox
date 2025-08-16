import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

st.title("Interactive Data Analysis and Visualization App")

st.sidebar.title("Navigation")
options = st.sidebar.radio("Go to", ["Home", "Upload Data", "Visualize Data", "About"])

if options == "Home":
    st.subheader("Welcome to the Data Analysis App!")
    st.write("""
    This app allows you to:
    - Upload your own datasets.
    - Perform exploratory data analysis (EDA).
    - Visualize data interactively.
    """)
    st.image("https://streamlit.io/images/brand/streamlit-logo-primary-colormark-darktext.png", width=300)

elif options == "Upload Data":
    st.subheader("Upload Your Dataset")
    uploaded_file = st.file_uploader("Choose a CSV file", type=["csv"])
    if uploaded_file is not None:
        df = pd.read_csv(uploaded_file)
        st.write("Here is a preview of your data:")
        st.dataframe(df.head())
        st.session_state['data'] = df  

elif options == "Visualize Data":
    st.subheader("Visualize Your Data")
    if 'data' not in st.session_state:
        st.warning("Please upload a dataset first!")
    else:
        df = st.session_state['data']
        st.write("Select columns for plotting:")
        numeric_columns = df.select_dtypes(include=['float64', 'int64']).columns.tolist()
        if numeric_columns:
            x_axis = st.selectbox("X-axis:", numeric_columns)
            y_axis = st.selectbox("Y-axis:", numeric_columns)
            chart_type = st.radio("Chart type:", ["Scatter Plot", "Line Chart", "Bar Chart"])
            if chart_type == "Scatter Plot":
                st.write(f"Scatter Plot of {x_axis} vs {y_axis}")
                fig, ax = plt.subplots()
                sns.scatterplot(data=df, x=x_axis, y=y_axis, ax=ax)
                st.pyplot(fig)
            elif chart_type == "Line Chart":
                st.write(f"Line Chart of {x_axis} vs {y_axis}")
                fig, ax = plt.subplots()
                sns.lineplot(data=df, x=x_axis, y=y_axis, ax=ax)
                st.pyplot(fig)
            elif chart_type == "Bar Chart":
                st.write(f"Bar Chart of {x_axis} vs {y_axis}")
                fig, ax = plt.subplots()
                df_grouped = df.groupby(x_axis)[y_axis].mean().reset_index()
                sns.barplot(data=df_grouped, x=x_axis, y=y_axis, ax=ax)
                st.pyplot(fig)
        else:
            st.warning("Your dataset does not contain numeric columns for plotting.")

elif options == "About":
    st.subheader("About This App")
    st.write("""
    This app is built using [Streamlit](https://streamlit.io) to showcase data analysis and visualization features.
    Feel free to use it for quick exploratory data analysis (EDA) tasks.
    """)
    st.write("Built with ❤️ by Gongahkia ahkia ahkia ([@gongahkia on Github](https://github.com/gongahkia))")

st.sidebar.write("---")
st.sidebar.write("© 2025 Data Analysis App")