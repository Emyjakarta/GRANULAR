import pandas as pd
import glob

# Get all .txt files in the current directory
txt_files = glob.glob("*.txt")

for txt_file in txt_files:
    # Replace .txt with .xlsx for the output file name
    xlsx_file = txt_file.replace(".txt", ".xlsx")

    # Read the text file into a DataFrame
    data = pd.read_csv(txt_file, delim_whitespace=True)

    # Save the DataFrame to an Excel file
    data.to_excel(xlsx_file, index=False)

    print(f"Converted {txt_file} to {xlsx_file}")

