# -*- coding: utf-8 -*-
"""
Spyder Editor

"""
import pandas as pd
from newspaper import Article
import logging

# Configure logging to monitor progress
logging.basicConfig(format='%(asctime)s - %(levelname)s - %(message)s', level=logging.INFO)

# Function to download the article body
def download_article_body(url):
    try:
        article = Article(url)
        article.download()
        article.parse()
        return article.text, "Success"
    except Exception as e:
        logging.error(f"Error downloading the article: {url} | Error: {e}")
        return None, str(e)  # Returns None and the error message if something goes wrong

# Load the Excel file
df = pd.read_excel('dataset_noticias.xlsx')

# Prepare columns for the complete body and the download status
df['bodyCompleto'] = ""
df['status'] = ""

# Iterate over the rows of the DataFrame
for index, row in df.iterrows():
    body, status = download_article_body(row['link'])
    df.at[index, 'bodyCompleto'] = body
    df.at[index, 'status'] = status
    # Report progress
    logging.info(f"Processed record {index + 1}/{len(df)}: {status}")

# Save the results to a new Excel file
df.to_excel('backup_dataset_noticias.xlsx', index=False)
