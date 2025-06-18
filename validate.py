"""
Validation of the review table against the codebook.

Prerequisites:
1. The following command installs all necessary Python packages 
   in the environment:

pip install numpy pandas

How to use:
1. Save the script somewhere on your computer.
2. Create a folder 'data' in the same folder where the script is.
3. Run the script as shown below.

usage: python ./validate.py [-h] 
           [--coverage] [--debug] [--no-cols NO_COLS] [--no-rows NO_ROWS]
           [--only-cols ONLY_COLS] [--only-rows ONLY_ROWS] [--manual] 
           [--show SHOW] [--update]

Codebook-based validation of the review table

options:
  -h, --help            show this help message and exit
  --coverage            Check coverage of the codebook
  --debug               Enable debug output
  --no-cols NO_COLS     Hide all errors for the specified columns (multiple values 
                        should be listed as a,b,c without spaces)
  --no-rows NO_ROWS     Hide all errors for the specified rows (multiple values 
                        should be listed as a,b,c without spaces)
  --only-cols ONLY_COLS
                        Show errors only for the specified columns (multiple values 
                        should be listed as a,b,c without spaces)
  --only-rows ONLY_ROWS
                        Show errors only for the specified rows (multiple values 
                        should be listed as a,b,c without spaces)
  --manual              Validate the table with manually added papers
  --show SHOW           Maximal number of error report rows to show
  --update              Download the latest version of codebook 
                        and table before validating
"""

import argparse
import logging
import numpy as np
import pandas as pd
import re
import warnings

from dataclasses import dataclass
from functools import partial
from urllib.request import urlretrieve


# Configure logging
logger = logging.getLogger(__name__)
logging.basicConfig(format='%(levelname)s\t%(message)s',
                    level=logging.INFO)

# Disable pandas print options
pd.set_option('display.max_columns', None)
pd.set_option('display.max_rows', None)
pd.set_option('display.width', None)
pd.set_option('display.expand_frame_repr', False)
pd.set_option('max_colwidth', None)


EXPORT1_CSV_PATH = 'data/exports/20240115_pubmed_query1.csv'
EXPORT2_CSV_PATH = 'data/exports/20240805_pubmed_query2.csv'
EXPORT3_CSV_PATH = 'data/exports/20240805_pubmed_query3.csv'
TABLE_PUBMED_CSV_URL = 'https://docs.google.com/spreadsheets/d/17iQwJ36F4KCTuSRoLa7sXqZUSSO2lyHELonZgAQoxAw/export?format=csv'
TABLE_PUBMED_CSV_PATH = 'data/HEP - Pubmed Results.csv'
TABLE_MANUAL_CSV_URL = 'https://docs.google.com/spreadsheets/d/17iQwJ36F4KCTuSRoLa7sXqZUSSO2lyHELonZgAQoxAw/export?format=csv&gid=1671894873'
TABLE_MANUAL_CSV_PATH = 'data/HEP - Manual.csv'
TABLE_CODEBOOK_CSV_URL = 'https://docs.google.com/spreadsheets/d/17iQwJ36F4KCTuSRoLa7sXqZUSSO2lyHELonZgAQoxAw/export?format=csv&gid=1303473741'
TABLE_CODEBOOK_CSV_PATH = 'data/Codebook.csv'
EXCEL_OFFSET = 3  # correct line numbers to match Excel rows
                  # 2 rows were used for header
                  # 1 row difference because of 0-indexing in Python

## Command-line arguments

parser = argparse.ArgumentParser(description="Codebook-based validation of "\
                                             "the review table")
parser.add_argument('--coverage', action='store_true',
                    help="Check coverage of the codebook")
parser.add_argument('--debug', action='store_true',
                    help="Enable debug output")
parser.add_argument('--no-cols', type=str, default='',
                    help="Hide all errors for the specified columns"\
                         " (multiple values should be listed as a,b,c"\
                         " without spaces)")
parser.add_argument('--no-rows', type=str, default='',
                    help="Hide all errors for the specified rows"\
                         " (multiple values should be listed as a,b,c"\
                         " without spaces, use numbers from Excel)")
parser.add_argument('--only-cols', type=str, default='',
                    help="Show errors only for the specified columns"\
                         " (multiple values should be listed as a,b,c"\
                         " without spaces)")
parser.add_argument('--only-rows', type=str, default='',
                    help="Show errors only for the specified rows"\
                         " (multiple values should be listed as a,b,c"\
                         " without spaces, use numbers from Excel)")
parser.add_argument('--manual', action='store_true',
                    help='Validate the table with manually added papers')
parser.add_argument('--show', type=int, default=20,
                    help='Maximal number of error report rows to show')
parser.add_argument('--update', action='store_true',
                    help='Download the latest version of codebook and table'\
                         ' before validating')


## Update codebook and table

def update(table_csv_url, table_csv_path):
    """
    Download the latest version of the codebook and the table
    from Google Drive.
    """
    logger.info('Updating the codebook...')
    urlretrieve(TABLE_CODEBOOK_CSV_URL, TABLE_CODEBOOK_CSV_PATH)
    logger.info('Done')

    logger.info('Updating the table...')
    urlretrieve(table_csv_url, table_csv_path)
    logger.info('Done')


## Codebook

def load_codebook(codebook_path):
    """
    Load the codebook values from all tables in the specified .csv file
    
    Parameters
    ----------
    codebook_path: str
        Path to the .csv file of the codebook

    Returns
    -------
    codebook: pd.DataFrame
        Codebook table
    """

    df_codebook = pd.read_csv(codebook_path, header=1)
    df_codebook["run_checks"] = df_codebook["run_checks"].fillna(False)
    df_codebook["checks"] = df_codebook["checks"].fillna("")
    return df_codebook


def codebook_coverage(df, codebook):
    """
    Checks and prints if all codebook values are actually present in the table.

    Parameters
    ----------
    df: pd.DataFrame
        Table with extracted values
    codebook: pd.DataFrame
        Codebook table
    """

    logger.info('Checking for potentially unused codebook values')
    for _, row in codebook.iterrows():
        if pd.isna(row['allowed_values']):
            continue

        allowed_values = [el.lower() for el in row['allowed_values'].split(', ')]
        column_name = row['column']

        if 'list' in row['checks']:
            extracted_values = set()
            for el in df[column_name].values:
                if pd.isna(el):
                    continue

                extracted_values |= set([ch.lower() for ch in el.strip().split(', ')])
        else:
            extracted_values = [el.strip().lower()
                                for el in df[column_name].unique() 
                                if not pd.isna(el)]
        # print(column_name, extracted_values)

        if 'none' in allowed_values:
            allowed_values.remove('none')
        if 'unknown' in allowed_values:
            allowed_values.remove('unknown')
        unused_values = set(allowed_values) - set(extracted_values)
        if unused_values:
            unused_desc = ', '.join(unused_values)
            logger.info(f'Unused codebook values for {column_name}: {unused_desc}')

    logger.info('Done')


## Original export from Pubmed

def get_original_data():
    # Merge the data from all queries
    df_query1 = pd.read_csv(EXPORT1_CSV_PATH)
    df_query2 = pd.read_csv(EXPORT2_CSV_PATH)
    df_query3 = pd.read_csv(EXPORT3_CSV_PATH)

    df_original = pd.merge(
        pd.merge(df_query1, df_query2, how='outer'),
        df_query3, how='outer'
    )

    # Resolve duplicates - some entries were updated between exports
    resolved = {k: np.max(idx) for k, idx in df_original.groupby('PMID').indices.items()}
    df_original = df_original.loc[resolved.values(), :].sort_values('PMID', ascending=False)

    # Match the columns to our format
    df_original.rename(columns={'Publication Year': 'Year'}, inplace=True)
    df_original.drop(columns=['First Author', 'Journal/Book', 'Create Date', 'PMCID', 'NIHMS ID'],
                     inplace=True)
    
    return df_original


## Checks

@dataclass
class Check:
    fun: callable
    single_column: bool
    needs_codebook: bool
    needs_original: bool


## Validation rules

def is_numeric(x):
    try:
        float(x)
        return True
    except ValueError:
        return False


def should_be_0_or_1(x):
    if not is_numeric(x):
        return False
    
    return int(x) in [0, 1]


def validate_list(x, allowed, col, lower=True):
    logger.debug(f'validate_list: x={x}, col={col}, allowed={allowed}')
    if not x:
        return True

    valid = True
    chunk_lists = [el.strip() for el in x.split(';')]
    for chunk_list in chunk_lists:
        chunks = [chunk.strip() for chunk in chunk_list.split(', ')]
        if lower:
            chunks = [chunk.lower() for chunk in chunks]
            allowed = [term.lower() for term in allowed]

        # NOTE: determine whether a regex should be used for validation, a small
        # hack fine-tuned to the 'RR at least X ms' case
        use_regex = any(["\\" in el for el in allowed])
        if use_regex:
            chunks_allowed = [any([re.fullmatch(el, chunk) for el in allowed]) 
                              for chunk in chunks]
        else:
            chunks_allowed = [chunk in allowed for chunk in chunks]

        valid &= all(chunks_allowed)

    logger.debug(f'Result: {valid}')
    return valid


def validate_single(x, allowed, col, lower=True):
    logger.debug(f'validate_single: x={x}, col={col}, allowed={allowed}')
    if lower:
        allowed = [el.lower() for el in allowed]
        x = x.lower()

    # NOTE: determine whether a regex should be used for validation, a small
    # hack fine-tuned to the 'RR at least X ms' case
    use_regex = any(["\\" in el for el in allowed])
    if use_regex:
        valid = any([re.fullmatch(el, x) for el in allowed])
    else:
        valid = x in allowed

    logger.debug(f'Result: {valid}')
    return valid


def match_original(x, original, col):
    logger.debug(f'match_original: x={x}, col={col}, original={original}')
    return x == original


def number_or_range(x):
    """
    Supported formats: XX-YY, XX+-YY
    """
    if '-' in str(x) or '+-' in str(x):
        delim = '+-' if '+-' in str(x) else '-'
        chunks = x.split(delim)
        return len(chunks) == 2 and is_numeric(chunks[0]) and is_numeric(chunks[1])
    else:
        return is_numeric(x)


def validate_analyst(x):
    return x in ['Paul', 'Maria', 'Nick']


## Multi-column validation rules


def csd_as_reference(row, col, idx):
    csd_applied = any(x in str(row['Other CFA removal strategy'])
                      for x in ['CSD', 'Laplacian', 'Laplace', 'Hjorth'])
    lap_reference = row[col] == 'Laplacian reference'
    if csd_applied and not lap_reference:
        return [{
            'line': idx,
            'column': col,
            'error': "should be set to Laplacian if CSD was used", 
            'failure_case': row[col],
            'codebook': ''
        }]
    
    return []


def ica_details_for_cfa_only(row, col, idx):
    cfa_ica_removed = 'CFA' in str(row['Rejected components'])

    if cfa_ica_removed:
        return []
    
    errors = []
    if not pd.isna(row[col]) and str(row[col]):
        errors.append({
            'line': idx,
            'column': col,
            'error': "should be filled only if CFA ICs were removed", 
            'failure_case': row[col],
            'codebook': ''
        })

    return errors


def significant_info(row, col, idx):
    significant_cols = [
        "Significant / Channels",
        "Significant / Relative to",
        "Significant / Start (ms)",
        "Significant / End (ms)",
    ]
    permuted = int(row['Cluster-based Permutation'])
    try:
        test_significant = int(row['Significant test'])
    except:
        test_significant = False  # ['nan', 'none', 'unknown']
    
    cluster_expected = permuted and test_significant
    has_significant = any(not pd.isna(row[col]) for col in significant_cols)
    missing = any(pd.isna(row[col]) for col in significant_cols)
    assert col in (significant_cols + ['Cluster-based Permutation'])

    if not (cluster_expected or has_significant):
        return []

    if missing:
        return [{
            'line': idx,
            'column': col,
            'error': "should be filled for significant clusters", 
            'failure_case': row[col],
            'codebook': ''
        }]
    
    return []


layout_regex = {
    "standard": r"(?:Fp|AF|F|FC|FT|C|T|CP|TP|P|PO|O|I|A|M|Ad)(?:\d{1,2}|z)",
    "biosemi": r"(?:A|B|C|D)\d{1,2}",
    # NOTE: GSN positions are sometimes referred to by their 10-10 equivalents:
    # https://www.egi.com/images/HydroCelGSN_10-10.pdf
    "GSN": r"(?:E\d{1,3}|Cz|)|(?:Fp|AF|F|FC|FT|C|T|CP|TP|P|PO|O|I|A|M|Ad)(?:\d{1,2}|z)",
    # NOTE: easycap-M10 positions are sometimes referred to by their 10-10 equivalents:
    # https://www.easycap.de/wp-content/uploads/2018/02/Easycap-Equidistant-Layouts.pdf
    "easycap": r"(?:\d{1,2})|(?:Fp|AF|F|FC|FT|C|T|CP|TP|P|PO|O|I|A|M|Ad)(?:\d{1,2}|z)",
    "QuikCap": r"\d{1,3}",
    "Elekta": r"na",
    "KRISS": r"na"
}


def locations_belong_to_layout(row, col, idx, allow_all=False):
    test_string = row[col]

    # 'layout' should always pass
    if test_string == "layout":
        return []
    
    # For selected channels, we accept All as a valid entry
    # Also, we accept All except ...
    if allow_all and test_string == "All":
        return []
    
    all_except_prefix = "All except "
    if allow_all and all_except_prefix in test_string:
        test_string = test_string.removeprefix(all_except_prefix)

    # Channel names should be separated by comma+space
    chunks = str(test_string).split(", ")

    pattern = None
    for k, v in layout_regex.items():
        if k in row["Layout"]:
            pattern = v
    assert pattern is not None, f"Could not find regex for layout {row['Layout']}"

    # Empty pattern - no validation required
    if not pattern:
        return []

    # Report all chunks that do not match the regex
    errors = []
    matches = [re.fullmatch(pattern, ch) for ch in chunks]
    bad_chunks = [ch for ch, m in zip(chunks, matches) if m is None]
    if any(bad_chunks):
        errors.append({
            'line': idx,
            'column': col,
            'error': "should belong to the layout", 
            'failure_case': ', '.join([f"<{ch}>" for ch in bad_chunks]),
            'codebook': ''
        })

    # For 10-20 and derivatives, check T3/T7 and so on
    names_1020 = set(chunks) & set(["T3", "T4", "T5", "T6"])
    names_1010 = set(chunks) & set(["T7", "T8", "P7", "P8"])
    if row["Layout"] == "standard19" and names_1010:
        errors.append({
            'line': idx,
            'column': col,
            'error': "should not have 10-10 names", 
            'failure_case': ', '.join(names_1010),
            'codebook': ''
        })
    if row["Layout"] != "standard19" and names_1020:
        errors.append({
            'line': idx,
            'column': col,
            'error': "should not have 10-20 names", 
            'failure_case': ', '.join(names_1020),
            'codebook': ''
        })
    
    return errors


## Some tests for validation rules

assert is_numeric('123.45')
assert is_numeric('0')
assert not is_numeric('aaa')
assert not is_numeric('2-')

assert validate_single('a', ['a', 'b'], col='test')
assert validate_single('Aa', ['aA'], col='test')
assert validate_single('RR at least 500 ms', ['RR at least \\d+ ms'], col='test')
assert not validate_single('a, b', ['a', 'b'], col='test')
assert not validate_single('RR at least 500 ms', ['RR at least ms'], col='test')

assert validate_list('', ['a', 'b', 'c'], col='test')
assert validate_list('a', ['a', 'b', 'c'], col='test')
assert validate_list('a, c', ['a', 'b', 'c'], col='test')
assert validate_list('a, b, c', ['a', 'b', 'c'], col='test')
assert validate_list('A, B, C', ['a', 'B', 'c'], col='test')
assert validate_list('A, B; C', ['a', 'B', 'c'], col='test')
assert not validate_list('a, d', ['a', 'b', 'c'], col='test')

assert number_or_range('200')
assert number_or_range('200-210')
assert number_or_range('200+-20')
assert not number_or_range('200+-')
assert not number_or_range('200-')

assert should_be_0_or_1(0)
assert should_be_0_or_1('1')
assert not should_be_0_or_1(10)
assert not should_be_0_or_1('2')
assert not should_be_0_or_1('a')

assert validate_analyst('Paul')
assert validate_analyst('Maria')
assert validate_analyst('Nick')
assert not validate_analyst('XYZ')

assert not csd_as_reference({
    'Reference (offline)': 'Laplacian reference',
    'Other CFA removal strategy': 'CSD transformation'
}, 'Reference (offline)', 0)
assert not csd_as_reference({
    'Reference (offline)': 'Common average',
    'Other CFA removal strategy': ''
}, 'Reference (offline)', 0)
assert len(csd_as_reference({
    'Reference (offline)': 'Common average',
    'Other CFA removal strategy': 'CSD transformation'
}, 'Reference (offline)', 0)) == 1
assert len(csd_as_reference({
    'Reference (offline)': 'Common average',
    'Other CFA removal strategy': 'Laplacian transformation'
}, 'Reference (offline)', 0)) == 1

assert not ica_details_for_cfa_only({
    'Rejected components': 'CFA',
    'CFA Rej. Approach': 'Manual',
    'CFA Rej. Criteria': 'Topography',
    '# rejected cardiac ICs': 2.03
}, 'CFA Rej. Approach', 0)
assert not ica_details_for_cfa_only({
    'Rejected components': 'Blinks',
    'CFA Rej. Approach': pd.NA,
    'CFA Rej. Criteria': pd.NA,
    '# rejected cardiac ICs': ''
}, 'CFA Rej. Approach', 0)
assert len(ica_details_for_cfa_only({
    'Rejected components': 'Blinks',
    'CFA Rej. Approach': 'Manual',
    'CFA Rej. Criteria': 'Topography',
    '# rejected cardiac ICs': 2.03
}, '# rejected cardiac ICs', 0)) == 1

assert not locations_belong_to_layout({
    "Layout": "standard61",
    "EEG Locations": "layout"
}, "EEG Locations", 0)
assert not locations_belong_to_layout({
    "Layout": "standard32",
    "EEG Locations": "Fp1, T8, P10, FCz, Cz, Iz"
}, "EEG Locations", 0)
assert locations_belong_to_layout({
    "Layout": "standard19",
    "EEG Locations": "Fp1,T8,P10,FCz,Cz,Iz"
}, "EEG Locations", 0)
assert not locations_belong_to_layout({
    "Layout": "biosemi128",
    "EEG Locations": "A4, B8, C12, D16"
}, "EEG Locations", 0)
assert locations_belong_to_layout({
    "Layout": "biosemi128",
    "EEG Locations": "E12"
}, "EEG Locations", 0)
assert not locations_belong_to_layout({
    "Layout": "GSN-HydroCel-65",
    "EEG Locations": "Cz, E1, E16, E64"
}, "EEG Locations", 0)
assert locations_belong_to_layout({
    "Layout": "GSN-HydroCel-129",
    "EEG Locations": "F123"
}, "EEG Locations", 0)
assert not locations_belong_to_layout({
    "Layout": "easycap-M10",
    "EEG Locations": "2, 4, 8, 16"
}, "EEG Locations", 0)
assert locations_belong_to_layout({
    "Layout": "easycap-M10",
    "EEG Locations": "B12"
}, "EEG Locations", 0)
assert not locations_belong_to_layout({
    "Layout": "easycap-M10",
    "Channels selected": "All"
}, "Channels selected", 0, allow_all=True)
assert locations_belong_to_layout({
    "Layout": "easycap-M10",
    "Channels selected": "All"
}, "Channels selected", 0, allow_all=False)

assert len(significant_info({
    "Cluster-based Permutation": 1,
    "Significant test": 1,
    "Significant / Channels": pd.NA,
    "Significant / Relative to": "R-peak",
    "Significant / Start (ms)": 100,
    "Significant / End (ms)": 200
}, "Significant / Channels", 0)) == 1
assert not significant_info({
    "Cluster-based Permutation": 1,
    "Significant test": 1,
    "Significant / Channels": "Ch1, Ch2",
    "Significant / Relative to": "R-peak",
    "Significant / Start (ms)": 100,
    "Significant / End (ms)": 200
}, "Significant / Start (ms)", 0)
assert len(significant_info({
    "Cluster-based Permutation": 1,
    "Significant test": 1,
    "Significant / Channels": "Ch1, Ch2",
    "Significant / Relative to": pd.NA,
    "Significant / Start (ms)": pd.NA,
    "Significant / End (ms)": pd.NA
}, "Significant / End (ms)", 0)) == 1
assert not significant_info({
    "Cluster-based Permutation": 1,
    "Significant test": 0,
    "Significant / Channels": pd.NA,
    "Significant / Relative to": pd.NA,
    "Significant / Start (ms)": pd.NA,
    "Significant / End (ms)": pd.NA
}, "Significant / Channels", 0)
assert len(significant_info({
    "Cluster-based Permutation": 1,
    "Significant test": 1,
    "Significant / Channels": pd.NA,
    "Significant / Relative to": pd.NA,
    "Significant / Start (ms)": pd.NA,
    "Significant / End (ms)": pd.NA
}, "Significant / Channels", 0)) == 1


## Mapping of validation functions to the codebook names

CHECK_MAPPING = {
    # Single-column
    'should be one of the codebook options': Check(
        fun=validate_single, 
        single_column=True,
        needs_codebook=True,
        needs_original=False
    ),
    'should be a list of codebook options': Check(
        fun=validate_list, 
        single_column=True,
        needs_codebook=True,
        needs_original=False
    ),
    'should be a number or a range': Check(
        fun=number_or_range, 
        single_column=True,
        needs_codebook=False,
        needs_original=False
    ),
    'analyst': Check(
        fun=validate_analyst, 
        single_column=True,
        needs_codebook=False,
        needs_original=False
    ),
    'should be 0 or 1': Check(
        fun=should_be_0_or_1, 
        single_column=True,
        needs_codebook=False,
        needs_original=False
    ),
    'should match the Pubmed export': Check(
        fun=match_original,
        single_column=True,
        needs_codebook=False,
        needs_original=True
    ),
    # Multi-column
    'should be filled only if CFA ICs were removed': Check(
        fun=ica_details_for_cfa_only,
        single_column=False,
        needs_codebook=False,
        needs_original=False
    ),
    'should be set to Laplacian if CSD was used': Check(
        fun=csd_as_reference,
        single_column=False,
        needs_codebook=False,
        needs_original=False
    ),
    'should belong to the layout (used)': Check(
        fun=locations_belong_to_layout,
        single_column=False,
        needs_codebook=False,
        needs_original=False
    ),
    'should belong to the layout (selected)': Check(
        fun=partial(locations_belong_to_layout, allow_all=True),
        single_column=False,
        needs_codebook=False,
        needs_original=False
    ),
    'should belong to the layout (significant)': Check(
        fun=locations_belong_to_layout,
        single_column=False,
        needs_codebook=False,
        needs_original=False
    ),
    'should be filled for significant clusters': Check(
        fun=significant_info,
        single_column=False,
        needs_codebook=False,
        needs_original=False
    )
}


def validate_row(row, idx, ignore_cols, missing_cols, codebook, original):
    errors = []
    unknown_checks = set()

    for col, value in row.items():
        # Ignore columns during validation as well
        # Helps if some columns were added but not defined in the script
        if col in ignore_cols or col in missing_cols:
            continue
        codebook_col = codebook[codebook.column == col]
        assert len(codebook_col) == 1, f"Could not find column {col} in the codebook"
        codebook_col = codebook_col.squeeze()

        # Check if the value allowed to be empty
        if pd.isna(value):
            if not codebook_col.allow_empty:
                logger.debug(f'Should not be empty: col={col} | value={value}')
                errors.append({
                    'line': idx,
                    'column': col,
                    'error': 'should not be empty', 
                    'failure_case': value,
                    'codebook': ''
                })
            else:
                logger.debug(f'Skipping checks: col={col} | value={value}')
            continue

        # Check if the value allowed to be none
        if value == "none":
            if not codebook_col.allow_none:
                logger.debug(f'Should not be none: col={col} | value={value}')
                errors.append({
                    'line': idx,
                    'column': col,
                    'error': 'should not be none', 
                    'failure_case': value,
                    'codebook': ''
                })
            else:
                logger.debug(f'Skipping checks: col={col} | value={value}')
            continue

        # Check if the value allowed to be unknown
        if value == "unknown":
            if not codebook_col.allow_unknown:
                logger.debug(f'Should not be unknown: col={col} | value={value}')
                errors.append({
                    'line': idx,
                    'column': col,
                    'error': 'should not be unknown', 
                    'failure_case': value,
                    'codebook': ''
                })
            else:
                logger.debug(f'Skipping checks: col={col} | value={value}')
            continue

        # Accept everything in square brackets if allowed
        value_str = str(value)
        has_brackets = value_str and value_str[0] == '[' and str(value)[-1] == ']'
        if codebook_col.allow_brackets and has_brackets:
            logger.debug(f'Skipping checks: col={col} | value={value}')
            continue

        # Check data type
        if codebook_col.data_type == "numeric" and not is_numeric(value):
            errors.append({
                'line': idx,
                'column': col,
                'error': 'wrong data type', 
                'failure_case': value,
                'codebook': codebook_col.data_type
            })

        checks = [el for el in str(codebook_col.checks).split(', ') if el.strip()]
        for check_name in checks:
            if check_name not in CHECK_MAPPING:
                unknown_checks.add(check_name)
                continue

            check = CHECK_MAPPING[check_name]

            # Provide the whole row to multi-column checks
            if not check.single_column:
                errors.extend(check.fun(row, col, idx))
                continue

            if check.needs_codebook:
                allowed_values = [el for el in str(codebook_col.allowed_values).split(', ') if el.strip()]
                if not check.fun(value, allowed_values, col=col):
                    errors.append({
                        'line': idx,
                        'column': col,
                        'error': check_name, 
                        'failure_case': value,
                        'codebook': allowed_values
                    })
            elif check.needs_original:
                if original is not None:
                    original_value = original[col].values[0]
                    if not check.fun(value, original_value, col=col):
                        errors.append({
                            'line': idx,
                            'column': col,
                            'error': check_name, 
                            'failure_case': value,
                            'codebook': original_value
                        })
            else:
                if not check.fun(value):
                    errors.append({
                        'line': idx,
                        'column': col,
                        'error': check_name, 
                        'failure_case': value,
                        'codebook': ''
                    })  

    return errors, unknown_checks      


def validate(df, df_original, codebook, ignore_cols, manual, included=True):
    # Focus only on columns that need to be validated and match the `included`
    # argument
    codebook = codebook[codebook.run_checks]
    codebook = codebook[codebook.included == included]
    checked_columns = list(codebook.column.values)

    desc = "included" if included else "all"
    col_desc = ", ".join(checked_columns)
    logger.info(f"Validating the following columns for {desc} papers: {col_desc}")

    # Check if any columns are not validated
    missing_cols = set(df.columns) - set(checked_columns)
    if missing_cols:
        missing_desc = ', '.join(missing_cols)
        if included:
            logger.warning(
                f"Some columns will be ignored during validation: {missing_desc}"
            )
        else:
            logger.warning(
                "Some columns will be ignored during validation "
                "(use --debug to see the full list)"
            )
            logger.debug(f"Ignored columns: {missing_desc}")

    # Validate the data frame row by row
    errors = []
    unknown_checks = set()
    for idx, row in df.iterrows():
        row_dict = row.to_dict()
        original = None
        if not manual:
            original = df_original[df_original.PMID.astype(int) == row['PMID']]            
            if len(original) != 1:
                errors.append({
                    'line': idx,
                    'column': "PMID",
                    'error': "should match the Pubmed export", 
                    'failure_case': row['PMID'],
                    'codebook': ''
                })
                original = None
        col_errors, col_unknown_checks = validate_row(row_dict, idx, ignore_cols, 
                                                      missing_cols, codebook, original)
        errors.extend(col_errors)
        unknown_checks |= col_unknown_checks

    if unknown_checks:
        warnings.warn(f'There were unknown checks in the schema: {unknown_checks}')

    errors_df = pd.DataFrame(errors)
    return errors_df


## Dataset

def load_data(data_path):
    """
    Load and preprocess the table
    """
    dtype = {}
    dtype['# rejected cardiac ICs'] = str
    dtype['#Channels'] = str
    dtype['Multiple Comparisons'] = str
    df = pd.read_csv(data_path, header=1, dtype=dtype)

    # Fill in the columns that need to be validated with multiple options
    df['#Channels'] = df['#Channels'].fillna('')
    df['Channels selected'] = df['Channels selected'].fillna('')
    df.Topic = df.Topic.fillna('')
    df['Rejected components'] = df['Rejected components'].fillna('')
    df['CFA Rej. Criteria'] = df['CFA Rej. Criteria'].fillna('')
    df.Controls = df.Controls.fillna('')

    # Check that every paper has either 0 or 1 in Include column but not both 
    # (ignoring empty cells for now)
    include_check = df.loc[df.Include.notna(), ['PMID', 'Include']]\
            .value_counts()\
            .reset_index()
    bad_id = include_check['count'].argmax() # only makes sense if max > 1
    assert include_check['count'].max() == 1, f"A paper was both included "\
        f"and not included: {include_check.loc[bad_id, 'PMID']}"
    
    # Check that Include is set for all papers
    include_missing = set(df.PMID.values) - set(include_check.PMID.values)
    if include_missing:
        logger.error(f"The following PMIDs were not screened: {include_missing}")

    # Print all combinations of Include and Comment values
    # (weird ones should be fixed)
    include_comment = df.loc[df.Include.notna(), ['Include', 'Comment']]\
                        .value_counts()\
                        .reset_index()
    logger.info('Unique combinations of Include and Comment in the table:')
    print(include_comment)
    print(f"{include_comment['count'].sum()} / {len(set(df.PMID.values))}")

    return df


def filter_rows(df_full, included=True):
    """
    Filter rows to be validated by conditions:
     - paper was included in the further analysis (Include == 1)
    """
    df = df_full.copy()

    # Pick only the included papers
    if included:
        pmids_included = list(df.PMID[df.Include == 1])
        df = df[df.PMID.isin(pmids_included)]

    return df


def validate_own(df, df_all, df_original, codebook, 
                 ignore_rows=[], ignore_cols=[], 
                 only_rows=[], only_cols=[],
                 n=20, manual=False, coverage=False, debug=False):
    logger.info('Validating the table...')
    errors_df_included = validate(df, df_original, codebook, ignore_cols, manual=manual, included=True)
    errors_df_all = validate(df_all, df_original, codebook, ignore_cols, manual=manual, included=False)
    errors_df = pd.concat([errors_df_all, errors_df_included])
    if not len(errors_df):
        logger.info('Nothing to complain about')
        if coverage:
            codebook_coverage(df, codebook)
        return
    
    report = pd.merge(df.reset_index(names='line'), 
                      errors_df, 
                      how='right', on='line')
    if debug:
        report['failure_case'] = report['failure_case']\
                                    .apply(lambda x: f'<{x}>')
    report['line'] += EXCEL_OFFSET
    errors_total = len(report)

    # Print number of errors per column
    col_stats = report.column.value_counts()
    print(col_stats)

    # Prepare to print: 
    #  - remove errors for ignored columns
    #  - sort by line in Excel
    #  - reorder columns
    #  - use first n rows
    report_disp = report.copy()
    if only_cols:
        report_disp = report_disp[report_disp.column.isin(only_cols)]
    if only_rows:
        report_disp = report_disp[report_disp.line.isin(only_rows)]
    report_disp = report_disp[np.logical_and(~report_disp.line.isin(ignore_rows),
                                             ~report_disp.column.isin(ignore_cols))]\
                        .sort_values('line')
    errors_non_ignored = len(report_disp)
    report_disp = report_disp[['line', 'PMID',
                               'column', 'failure_case',
                               'error', 'codebook']]\
                        .head(n=n)
    
    logger.info(f'{errors_total} errors total ({errors_non_ignored} for non-ignored columns)')
    if n < errors_non_ignored:
        logger.info(f'Showing the first {n} errors below:')
    for _, row in report_disp.iterrows():
        print()
        print(row.to_string())


def main(args):
    table_csv_url = TABLE_PUBMED_CSV_URL
    table_csv_path = TABLE_PUBMED_CSV_PATH
    if args.manual:
        table_csv_url = TABLE_MANUAL_CSV_URL
        table_csv_path = TABLE_MANUAL_CSV_PATH

    if args.update:
        update(table_csv_url, table_csv_path)

    codebook = load_codebook(TABLE_CODEBOOK_CSV_PATH)
    df_full = load_data(table_csv_path)

    df_original = get_original_data()
    if not args.manual:
        assert len(df_original) == len(df_full.PMID.unique()), \
            "The number of unique PMIDs in the table and in the original data " \
            "does not match"

    df = filter_rows(df_full, included=True)
    df_all = filter_rows(df_full, included=False)

    ignore_cols = [col.strip() for col in args.no_cols.split(',') if col.strip()]
    ignore_cols_exist = [col in df.columns for col in ignore_cols if col]
    if not all(ignore_cols_exist):
        raise ValueError(f'Some of the provided ignore columns are '
                         f'not present in the dataset: {ignore_cols_exist}')

    ignore_rows = args.no_rows.split(',')
    try:
        ignore_rows = [int(r.strip()) for r in ignore_rows if r.strip()]
    except ValueError:
        raise ValueError('Failed to process provided rows to be ignored')

    logger.info(f'Columns to be ignored: {ignore_cols}')
    logger.info(f'Rows to be ignored: {ignore_rows}')

    only_cols = [col.strip() for col in args.only_cols.split(',') if col.strip()]
    only_cols_exist = [col in df.columns for col in only_cols if col]
    if not all(only_cols_exist):
        raise ValueError(f'Some of the provided columns to be shown are '
                         f'not present in the dataset: {only_cols_exist}')

    only_rows = args.only_rows.split(',')
    try:
        only_rows = [int(r.strip()) for r in only_rows if r.strip()]
    except ValueError:
        raise ValueError('Failed to process provided rows to be shown')
    
    if only_cols:
        ignore_cols = []
        logger.info(f'Columns to be shown: {only_cols}')
    if only_rows:
        ignore_rows = []
        logger.info(f'Rows to be shown: {only_rows}')

    validate_own(df, df_all, df_original, codebook, ignore_rows,
                 ignore_cols, only_rows, only_cols,
                 args.show, args.manual, args.coverage, args.debug)


if __name__ == "__main__":
    args = parser.parse_args()
    if args.debug:
        logger.setLevel(logging.DEBUG)    

    main(args)
