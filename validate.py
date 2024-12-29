"""
Validation of the review table against the codebook.

Prerequisites:
1. The following command installs all necessary Python packages 
   in the environment:

pip install numpy pandas pandera python-docx

How to use:
1. Save the script somewhere on your computer.
2. Create a folder 'data' in the same folder where the script is.
3. Download the codebook (HEP Literature - Normalization.docx) from
   Google drive and save it to the 'data' folder.
4. Download the review table (HEP - Pubmed Results.csv) from Google
   drive and save it to the 'data' folder.
5. Run the script as shown below.

usage: python ./validate.py [-h] [--analyst {Paul,Maria,Nick}] [--debug]
                   [--export EXPORT] [--no-cols NO_COLS] [--no-rows NO_ROWS]
                   [--show SHOW] [--update]

Codebook-based validation of the review table

options:
  -h, --help            show this help message and exit
  --analyst {Paul,Maria,Nick}
                        Filter table rows by the name of the analyst
  --debug               Enable debug output
  --export EXPORT       Filename to export the schema to
  --no-cols NO_COLS     Hide all errors for the specified columns (multiple
                        values should be listed as a,b,c without spaces)
  --no-rows NO_ROWS     Hide all errors for the specified rows (multiple
                        values should be listed as a,b,c without spaces, use
                        numbers from Excel)
  --show SHOW           Maximal number of error report rows to show
  --update              Download the latest version of codebook and
                        tablebefore validating

Example - show max. 50 errors for papers that Nick analyzed, while ignoring
columns 'Topic' and 'ECG Locations' and rows 20 and 30 in Excel, update
the codebook and the table before validating:

python ./validate.py --update --analyst Nick --no-cols "Topic,ECG Locations" --no-rows "20,30" --show 50
"""

import argparse
import logging
import numpy as np
import pandas as pd
import pandera as pa
import warnings

from docx import Document
from pandera import Check, Column
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


CODEBOOK_DOCX_URL = 'https://docs.google.com/document/d/1g77QJYJ7pyCoy_7PcrHwYomr2ysh0_UfWHCoockUN8E/export?format=docx'
CODEBOOK_DOCX_PATH = 'data/HEP Literature - Normalization.docx'
TABLE_PUBMED_CSV_URL = 'https://docs.google.com/spreadsheets/d/17iQwJ36F4KCTuSRoLa7sXqZUSSO2lyHELonZgAQoxAw/export?format=csv'
TABLE_PUBMED_CSV_PATH = 'data/HEP - Pubmed Results.csv'
TABLE_MANUAL_CSV_URL = 'https://docs.google.com/spreadsheets/d/17iQwJ36F4KCTuSRoLa7sXqZUSSO2lyHELonZgAQoxAw/export?format=csv&gid=1671894873'
TABLE_MANUAL_CSV_PATH = 'data/HEP - Manual.csv'
EXCEL_OFFSET = 3  # correct line numbers to match Excel rows
                  # 2 rows were used for header
                  # 1 row difference because of 0-indexing in Python

## Command-line arguments

parser = argparse.ArgumentParser(description="Codebook-based validation of "\
                                             "the review table")
parser.add_argument('--analyst', type=str,
                    choices=['Paul', 'Maria', 'Nick'],
                    help="Filter table rows by the name of the analyst")
parser.add_argument('--debug', action='store_true',
                    help="Enable debug output")
parser.add_argument('--export', type=str,
                    help="Filename to export the schema to")
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
    logging.info('Updating the codebook...')
    urlretrieve(CODEBOOK_DOCX_URL, CODEBOOK_DOCX_PATH)
    logging.info('Done')

    logging.info('Updating the table...')
    urlretrieve(table_csv_url, table_csv_path)
    logging.info('Done')


## Codebook

def load_codebook(codebook_path):
    """
    Load the codebook values from all tables in the specified
    .docx file. 
    
    Parameter:
     - codebook_path - path to the .docx file
    
    Expected structure of the tables:
    
    -------------------------------------
    | Name of the column in Excel table |
    -------------------------------------
    | Value  | (Synonyms)  | Comments   |
    -------------------------------------
    | value1 | ...         | ...        |
    -------------------------------------
    | value2 | ...         | ...        |
    -------------------------------------  
    
    Result:
     - codebook: dict
       keys = names of the columns in Excel table,
       values = (value1, value2, ...) as extracted from .docx
    """

    doc_codebook = Document(codebook_path)
    codebook = {}
    non_unique = []

    logger.debug('load_codebook: Values imported from the .docx codebook:')
    for i, table in enumerate(doc_codebook.tables):
        if table.rows[1].cells[0].text != "Value":
            warnings.warn(f'Skipping table {i}, unexpected format')
            continue

        key = table.rows[0].cells[0].text.strip()
        values = [r.cells[0].text.strip() for r in table.rows[2:]]
        codebook[key] = values
        logger.debug(f'load_codebook: Table {i} - {key}: {values}')

        # Check if tables contain duplicates
        if len(values) != len(set(values)):
            non_unique.append(key)

    # Warn if duplicates 
    if non_unique:
        warnings.warn(f"The following tables contain duplicate values: " \
                      f"{', '.join(non_unique)}")
        
    return codebook


## Validation rules

def is_number(x):
    try:
        float(x)
        return True
    except ValueError:
        return False


def should_be_0_or_1(x):
    return x in [0, 1]


def validate_list(x, allowed, col, lower=True):
    logger.debug(f'validate_list: x={x}, col={col}, allowed={allowed}')
    if not x:
        return True

    chunks = [chunk.strip() for chunk in x.strip().split(', ')]
    if lower:
        chunks = [chunk.lower() for chunk in chunks]
        allowed = [term.lower() for term in allowed]
    chunks_allowed = [chunk in allowed for chunk in chunks]

    logger.debug(f'Result: {all(chunks_allowed)}')
    return all(chunks_allowed)


def validate_single(x, allowed, col, lower=True):
    logger.debug(f'validate_single: x={x}, col={col}, allowed={allowed}')
    if lower:
        allowed = [el.lower() for el in allowed]
        x = x.lower()
    
    logger.debug(f'Result: {x in allowed}')
    return x in allowed


def number_or_range(x):
    if '-' in str(x):
        chunks = x.split('-')
        return len(chunks) == 2 and is_number(chunks[0]) and is_number(chunks[1])
    else:
        return is_number(x)


def number_if_known(x):
    """
    Accepts None, unknown or a number
    """
    if x.lower() in ['none', 'unknown']:
        return True
    
    return is_number(x)


def number_plus_minus_sd(x):
    chunks = x.split('+-') if '+-' in x else [x]
    return all([is_number(el) for el in chunks])


def validate_analyst(x):
    return x in ['Paul', 'Maria', 'Nick']


## Multi-column validation rules


def csd_as_reference(row, idx):
    csd_applied = any(x in str(row['Other CFA removal strategy'])
                      for x in ['CSD', 'Laplacian', 'Laplace', 'Hjorth'])
    lap_reference = row['Reference (offline)'] == 'Laplacian reference'
    if csd_applied and not lap_reference:
        return [{
            'line': idx,
            'column': 'Reference (offline)',
            'check': "should be set to 'Laplacian reference' if CSD was used", 
            'failure_case': row['Reference (offline)'],
            'codebook': ''
        }]
    
    return []


def ica_details_for_cfa_only(row, idx):
    cfa_ica_removed = 'CFA' in str(row['Rejected components'])
    cfa_detail_columns = [
        '# rejected cardiac ICs',
        'CFA Rej. Approach',
        'CFA Rej. Criteria'
    ]

    if cfa_ica_removed:
        return []
    
    errors = []
    for col in cfa_detail_columns:
        if not pd.isna(row[col]) and str(row[col]):
            errors.append({
                'line': idx,
                'column': col,
                'check': "should be empty if no CFA-related ICs were removed", 
                'failure_case': row[col],
                'codebook': ''
            })

    return errors


def channels_eeg_meg(row, idx):
    channels_col = '#Channels'
    if row[channels_col] == 'unknown':
        return []

    # For EEG, a single number is expected
    if row['Modality'] == 'EEG':
        if is_number(row[channels_col]):
            return []
        
        return [{
            'line': idx,
            'column': channels_col,
            'check': "should be a single number for EEG", 
            'failure_case': row[channels_col],
            'codebook': ''
        }]
    
    # Otherwise, mag/grad should be specified for MEG
    errors = [{
        'line': idx,
        'column': channels_col,
        'check': "should be a list of numbers with magnetometers/gradiometers for MEG", 
        'failure_case': row[channels_col],
        'codebook': ''
    }]
    chunks = [ch.strip() for ch in row[channels_col].split(',')]
    for ch in chunks:
        if len(ch.split(' ')) != 2:
            return errors
        
        num, desc = ch.split(' ')
        if not is_number(num) or desc.lower() not in ['magnetometers', 'gradiometers']:
            return errors
        
    return []


def cluster_based_permutations(row, idx):
    comp = row['Multiple Comparisons']
    perm = row['Cluster-based Permutation']

    if 'Cluster-based' in comp and str(perm) != '1':
        return [{
            'line': idx,
            'column': 'Cluster-based permutation',
            'check': "should be set to 1 if cluster-based permutations were used", 
            'failure_case': perm,
            'codebook': ''
        }]
    
    if 'Cluster-based' not in comp and str(perm) == '1':
        return [{
            'line': idx,
            'column': 'Cluster-based permutation',
            'check': "should NOT be set to 1 if cluster-based permutations were NOT used", 
            'failure_case': perm,
            'codebook': ''
        }]
    
    return []


## Some tests for validation rules

assert is_number('123.45')
assert is_number('0')
assert not is_number('aaa')
assert not is_number('2-')

assert validate_single('a', ['a', 'b'], col='test')
assert validate_single('Aa', ['aA'], col='test')
assert not validate_single('a, b', ['a', 'b'], col='test')

assert validate_list('', ['a', 'b', 'c'], col='test')
assert validate_list('a', ['a', 'b', 'c'], col='test')
assert validate_list('a, c', ['a', 'b', 'c'], col='test')
assert validate_list('a, b, c', ['a', 'b', 'c'], col='test')
assert validate_list('A, B, C', ['a', 'B', 'c'], col='test')
assert not validate_list('a, d', ['a', 'b', 'c'], col='test')

assert number_if_known('1.234')
assert number_if_known('123')
assert number_if_known('none')
assert number_if_known('Unknown')
assert not number_if_known('aaa')

assert number_plus_minus_sd('200')
assert number_plus_minus_sd('200+-20')
assert not number_plus_minus_sd('200+-')

assert should_be_0_or_1(0)
assert should_be_0_or_1(1)
assert not should_be_0_or_1(10)

assert validate_analyst('Paul')
assert validate_analyst('Maria')
assert validate_analyst('Nick')
assert not validate_analyst('XYZ')

assert not csd_as_reference({
    'Reference (offline)': 'Laplacian reference',
    'Other CFA removal strategy': 'CSD transformation'
}, 0)
assert not csd_as_reference({
    'Reference (offline)': 'Common average',
    'Other CFA removal strategy': ''
}, 0)
assert len(csd_as_reference({
    'Reference (offline)': 'Common average',
    'Other CFA removal strategy': 'CSD transformation'
}, 0)) == 1
assert len(csd_as_reference({
    'Reference (offline)': 'Common average',
    'Other CFA removal strategy': 'Laplacian transformation'
}, 0)) == 1

assert not ica_details_for_cfa_only({
    'Rejected components': 'CFA',
    'CFA Rej. Approach': 'Manual',
    'CFA Rej. Criteria': 'Topography',
    '# rejected cardiac ICs': 2.03
}, 0)
assert not ica_details_for_cfa_only({
    'Rejected components': 'Blinks',
    'CFA Rej. Approach': pd.NA,
    'CFA Rej. Criteria': pd.NA,
    '# rejected cardiac ICs': ''
}, 0)
assert len(ica_details_for_cfa_only({
    'Rejected components': 'Blinks',
    'CFA Rej. Approach': 'Manual',
    'CFA Rej. Criteria': 'Topography',
    '# rejected cardiac ICs': 2.03
}, 0)) == 3

assert not channels_eeg_meg({
    'Modality': 'EEG',
    '#Channels': '60'
}, 0)
assert channels_eeg_meg({
    'Modality': 'MEG',
    '#Channels': '60'
}, 0)
assert not channels_eeg_meg({
    'Modality': 'MEG',
    '#Channels': '60 Magnetometers'
}, 0)
assert not channels_eeg_meg({
    'Modality': 'MEG',
    '#Channels': '60 Gradiometers'
}, 0)
assert not channels_eeg_meg({
    'Modality': 'MEG',
    '#Channels': '60 magnetometers, 60 gradiometers'
}, 0)


assert not cluster_based_permutations({
    'Multiple Comparisons': 'Cluster-based',
    'Cluster-based Permutation': 1
}, 0)
assert not cluster_based_permutations({
    'Multiple Comparisons': 'Bonferroni',
    'Cluster-based Permutation': 0
}, 0)
assert len(cluster_based_permutations({
    'Multiple Comparisons': 'Cluster-based',
    'Cluster-based Permutation': 0
}, 0)) == 1
assert len(cluster_based_permutations({
    'Multiple Comparisons': 'Bonferroni',
    'Cluster-based Permutation': 1
}, 0)) == 1


## Own validation function since pandera one did not work

CHECK_FN_MAPPING = {
    'cb_single': (validate_single, True),
    'cb_multiple': (validate_list, True),
    'number_plus_minus_sd': (number_plus_minus_sd, False),
    'number_or_range': (number_or_range, False),
    'analyst': (validate_analyst, False),
    'number_none_or_unknown': (number_if_known, False),
    '0_or_1': (should_be_0_or_1, False)
}
MULTICOLUMN_CHECKS = [
    csd_as_reference,
    ica_details_for_cfa_only,
    channels_eeg_meg,
    cluster_based_permutations
]

def my_validate(df, schema, codebook, ignore_cols):
    # Check if any columns are not validated
    missing_cols = set(df.columns) - set(schema.columns)
    if missing_cols:
        logging.warning(
            f"The following columns are not defined in the schema "
            f"and will thus be ignored during validation: {', '.join(missing_cols)}"
        )

    errors = []
    unknown_checks = set()
    for idx, row in df.iterrows():
        row_dict = row.to_dict()
        for col, value in row_dict.items():
            # Ignore columns during validation as well
            # Helps if some columns were added but not defined in the script
            if col in ignore_cols or col in missing_cols:
                continue
            schema_col = schema.columns[col]

            # Check nullable if needed
            if not schema_col.nullable and pd.isna(value):
                logger.debug(f'Should not be empty: col={col} | value={value}')
                errors.append({
                    'line': idx,
                    'column': col,
                    'check': 'should not be empty', 
                    'failure_case': value,
                    'codebook': ''
                })
                continue

            # Skip checks for NaN values
            if schema_col.nullable and pd.isna(value):
                logger.debug(f'Skipping checks: col={col} | value={value}')
                continue

            # Accept none, unknown or everything in square brackets
            if value in ['none', 'unknown']:
                logger.debug(f'Skipping checks: col={col} | value={value}')
                continue
            value_str = str(value)
            if value_str and value_str[0] == '[' and str(value)[-1] == ']':
                logger.debug(f'Skipping checks: col={col} | value={value}')
                continue

            # Coerce dtype
            try:
                type_before = type(value)
                value = schema_col.dtype.coerce_value(value)
                type_after = type(value)
                logger.debug(f'coerce worked: col={col} | value={value} | {schema_col.dtype} | {type_before} -> {type_after}')
            except ValueError as e:
                logger.debug(f'coerce failed: col={col} | value={value} | {schema_col.dtype} | {type(value)} -> x')
                errors.append({
                    'line': idx,
                    'column': col,
                    'check': 'wrong data type', 
                    'failure_case': value,
                    'codebook': schema_col.dtype
                })

            checks = schema_col.checks
            for check in checks:
                if check.name not in CHECK_FN_MAPPING:
                    unknown_checks.add(check.name)
                    continue

                check_fn, needs_codebook = CHECK_FN_MAPPING[check.name]
                if needs_codebook:
                    if not check_fn(value, codebook[col], col=col):
                        errors.append({
                            'line': idx,
                            'column': col,
                            'check': check.error, 
                            'failure_case': value,
                            'codebook': codebook[col]
                        })
                else:
                    if not check_fn(value):
                        errors.append({
                            'line': idx,
                            'column': col,
                            'check': check.error, 
                            'failure_case': value,
                            'codebook': ''
                        })        

        for check_fn in MULTICOLUMN_CHECKS:
            errors.extend(check_fn(row, idx))

    if unknown_checks:
        warnings.warn(f'There were unknown checks in the schema: {unknown_checks}')

    errors_df = pd.DataFrame(errors)
    return errors_df


## Schema
# columns are grouped by data type and checks
# format: (name of column, can be empty or not)

float_columns = [
    ('PMID', False), 
    ('Year', False), 
    ('Sample size', False), 
    ('#ECG Channels', False),  
    ('Length (min)', True),
    ('High-pass', False), 
    ('Low-pass', False),
    ('HEP / Start (ms)', False), 
    ('HEP / End (ms)', False),
    ('Baseline / Start (ms)', False),
    ('Baseline / End (ms)', False),
    ('#Permutations', True), 
    ('Significant / Start (ms)', True),
    ('Significant / End (ms)', True)
]
int_01_columns = [
    ('rsHEP', False), 
    ('Include', True), 
    ('ICA', True), 
    ('ICA on epochs', True),
    ('Averaging (channels)', False), 
    ('Averaging (time)', False),
    ('Cluster-based Permutation', False)
]
str_columns = [
    ('DOI', False), 
    ('Link', False), 
    ('Title', False), 
    ('Authors', False), 
    ('Citation', False), 
    ('Comment', True),
    ('#Channels', False),
    ('ECG Description', False),
    ('ECG Ground', True),
    ('Other CFA removal strategy', True), 
    ('Other cleaning strategy', True),
    ('Channels selected', False),   # could be reconsidered
    ('Significant / Channels', True), 
    ('Other notes (unclassified)', True),
    ('Motivation', True)
]
str_cb_single_columns = [
    ('HEP / Window Type', False), 
    ('HEP / Relative to', False), 
    ('Modality', False),
    ('Layout', False), 
    ('ECG Lead', False), 
    ('Reference (online)', False),
    ('Reference (offline)', False), 
    ('CFA Rej. Approach', True), 
    ('Hypothesis', False),
    ('Value', False),  
    ('Significant / Relative to', True)
]
str_cb_multiple_columns = [
    ('Topic', False), 
    ('Rejected components', True),
    ('CFA Rej. Criteria', True), 
    ('Controls', True),
    ('ECG Locations', False),
    ('Statistics', False),
]
special_columns = {
    "Analyst": Column(str, 
                      Check.str_matches('Paul|Maria|Nick',
                                        name='analyst',
                                        error='unknown analyst')),
    "# rejected cardiac ICs": Column(str, 
                                     Check(lambda x: number_or_range(x),
                                           name='number_or_range',
                                           error="should be a number or a range",
                                           element_wise=True),
                                     nullable=True),
    '#Trials(+-SD)': Column(str,
                            Check(lambda x: number_plus_minus_sd(x),
                                  name='number_plus_minus_sd',
                                  error="should be number or number+-sd",
                                  element_wise=True),
                            nullable=True),
    '#Channels': Column(str,
                        Check(lambda x: channels_eeg_meg(x),
                              name='channels_eeg_meg',
                              error="should be a number (EEG) or 'N mag'/'N grad'/'N mag, M grad' for MEG"),
                        nullable=False)
}

def construct_schema(codebook):
    """
    Combine all previously defined columns with values from the
    codebook and create a pandera schema.
    """
    all_columns = special_columns.copy()

    # Allow none and unknown if all float columns
    all_columns.update({
        col: Column(str,
                    Check(lambda x: number_if_known(x),
                          name='number_none_or_unknown',
                          error="should be None, Unknown, or a number",
                          element_wise=True), 
                    nullable=is_nullable) 
        for col, is_nullable in float_columns
    })
    all_columns.update({
        col: Column('Int64', 
                    Check.isin([0, 1],
                               name='0_or_1',
                               error='should be 0 or 1',
                               element_wise=True), 
                    nullable=is_nullable) 
        for col, is_nullable in int_01_columns
    })
    all_columns.update({
        col: Column(str, 
                    nullable=is_nullable) 
        for col, is_nullable in str_columns
    })
    all_columns.update({
        col: Column(str, 
                    Check(lambda x: validate_single(x, codebook[col].copy(), col=col),
                          name='cb_single',
                          error='should be one of the codebook options',
                          element_wise=True),
                    nullable=is_nullable)
        for col, is_nullable in str_cb_single_columns
    })
    all_columns.update({
        col: Column(str, 
                    Check(lambda x: validate_list(x, codebook[col].copy(), col=col),
                          name='cb_multiple',
                          error='should be a list of codebook options', 
                          element_wise=True),
                    nullable=is_nullable)
        for col, is_nullable in str_cb_multiple_columns
    })

    schema = pa.DataFrameSchema(all_columns, coerce=True)

    return schema


def export_schema_wrt_df(schema, df):
    """
    Export the schema for all columns of the provided dataframe.
    Exported information for each column:
     - column name
     - data type
     - whether the column can be empty
     - name of applied checks
    """
    data = []
    for col in df.columns:
        col_data = {
            'Column': col, 
            'Data type': pd.NA, 
            'Can be empty?': pd.NA, 
            'Checks': []
        }
        if col in schema.columns:
            col_data['Data type'] = schema.columns[col].dtype
            col_data['Can be empty?'] = schema.columns[col].nullable
            col_data['Checks'] = [c.error for c in schema.columns[col].checks]
        data.append(col_data)

    return pd.DataFrame(data)


## Dataset

def load_data(data_path):
    """
    Load and preprocess the table
    """
    dtype = {col: 'Int64' for col in int_01_columns}
    dtype['# rejected cardiac ICs'] = str
    dtype['#Channels'] = str
    dtype['Multiple Comparisons'] = str
    df = pd.read_csv(data_path, header=1, dtype=dtype)

    # Fill in the columns that need to be validated with multiple options
    df['#Channels'] = df['#Channels'].fillna('')
    df['Channels selected'] = df['Channels selected'].fillna('')
    df.Topic = df.Topic.fillna('')
    df['Rejected components'] = df['Rejected components'].fillna('')
    df['Multiple Comparisons'] = df['Multiple Comparisons'].fillna('')
    df['CFA Rej. Criteria'] = df['CFA Rej. Criteria'].fillna('')
    df.Controls = df.Controls.fillna('')

    # Fix weird dash signs
    df['# rejected cardiac ICs'] = df['# rejected cardiac ICs'].str.replace('–', '-')
    df['Layout'] = df['Layout'].str.replace('–', '-')

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
        logging.error(f"The following PMIDs were not screened: {include_missing}")

    # Print all combinations of Include and Comment values
    # (weird ones should be fixed)
    include_comment = df.loc[df.Include.notna(), ['Include', 'Comment']]\
                        .value_counts()\
                        .reset_index()
    logging.info('Unique combinations of Include and Comment in the table:')
    print(include_comment)
    print(f"{include_comment['count'].sum()} / {len(set(df.PMID.values))}")

    return df


def filter_rows(df_full, analyst=None):
    """
    Filter rows to be validated by conditions:
     - paper was included in the further analysis (Include == 1)
     - if analyst is provided, the paper was analyzed by them
    """
    df = df_full.copy()

    # Pick only the included papers
    pmids_included = list(df.PMID[df.Include == 1])
    df = df[df.PMID.isin(pmids_included)]

    # Filter rows by the analyst
    if analyst:
        df = df[df.Analyst == analyst]

    return df


def validate_own(df, schema, codebook, 
                 ignore_rows=[], ignore_cols=[], 
                 only_rows=[], only_cols=[],
                 n=20, debug=False):
    logger.info('Validating the table...')
    errors_df = my_validate(df, schema, codebook, ignore_cols)
    if not len(errors_df):
        logger.info('Nothing to complain about')
        return
    
    report = pd.merge(df.reset_index(names='line'), 
                      errors_df, 
                      how='right', on='line')
    if debug:
        report['failure_case'] = report['failure_case']\
                                    .apply(lambda x: f'<{x}>')
    report['line'] += EXCEL_OFFSET
    errors_total = len(report)

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
                               'check', 'codebook']]\
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

    codebook = load_codebook(CODEBOOK_DOCX_PATH)
    schema = construct_schema(codebook)
    df_full = load_data(table_csv_path)

    if args.export:
        schema_df = export_schema_wrt_df(schema, df_full)
        schema_df.to_csv(args.export, index=False)
        return

    df = filter_rows(df_full, analyst=args.analyst)

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

    validate_own(df, schema, codebook, ignore_rows,
                 ignore_cols, only_rows, only_cols,
                 args.show, args.debug)


if __name__ == "__main__":
    args = parser.parse_args()
    if args.debug:
        logger.setLevel(logging.DEBUG)    

    main(args)
