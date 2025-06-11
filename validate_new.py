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
import warnings

from dataclasses import dataclass

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
parser.add_argument('--analyst', type=str,
                    choices=['Paul', 'Maria', 'Nick'],
                    help="Filter table rows by the name of the analyst")
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
    
    Parameter:
     - codebook_path - path to the .csv file

    Result:
     - codebook: dataframe
    """

    df_codebook = pd.read_csv(codebook_path, header=1)
    df_codebook["run_checks"] = df_codebook["run_checks"].fillna(False)
    df_codebook["checks"] = df_codebook["checks"].fillna("")
    return df_codebook


## Checks

@dataclass
class Check:
    fun: callable
    single_column: bool
    needs_codebook: bool


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


## Some tests for validation rules

assert is_numeric('123.45')
assert is_numeric('0')
assert not is_numeric('aaa')
assert not is_numeric('2-')

assert validate_single('a', ['a', 'b'], col='test')
assert validate_single('Aa', ['aA'], col='test')
assert not validate_single('a, b', ['a', 'b'], col='test')

assert validate_list('', ['a', 'b', 'c'], col='test')
assert validate_list('a', ['a', 'b', 'c'], col='test')
assert validate_list('a, c', ['a', 'b', 'c'], col='test')
assert validate_list('a, b, c', ['a', 'b', 'c'], col='test')
assert validate_list('A, B, C', ['a', 'B', 'c'], col='test')
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

## Mapping of validation functions to the codebook names

CHECK_MAPPING = {
    # Single-column
    'should be one of the codebook options': Check(
        fun=validate_single, 
        single_column=True,
        needs_codebook=True),
    'should be a list of codebook options': Check(
        fun=validate_list, 
        single_column=True,
        needs_codebook=True),
    'should be a number or a range': Check(
        fun=number_or_range, 
        single_column=True,
        needs_codebook=False),
    'analyst': Check(
        fun=validate_analyst, 
        single_column=True,
        needs_codebook=False),
    'should be 0 or 1': Check(
        fun=should_be_0_or_1, 
        single_column=True,
        needs_codebook=False),
    # Multi-column
    'should be filled only if CFA ICs were removed': Check(
        fun=ica_details_for_cfa_only,
        single_column=False,
        needs_codebook=False
    ),
    'should be set to Laplacian if CSD was used': Check(
        fun=csd_as_reference,
        single_column=False,
        needs_codebook=False
    )
}


def validate_row(row, idx, ignore_cols, missing_cols, codebook):
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


def validate(df, codebook, ignore_cols, included=True):
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
        logger.warning(
            "Some columns will be ignored during validation "
            "(use --debug to see the full list)"
        )
        logger.debug(f"Ignored columns: {', '.join(missing_cols)}")

    # Validate the data frame row by row
    errors = []
    unknown_checks = set()
    for idx, row in df.iterrows():
        row_dict = row.to_dict()
        col_errors, col_unknown_checks = validate_row(row_dict, idx, ignore_cols, 
                                                      missing_cols, codebook)
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


def filter_rows(df_full, analyst=None, included=True):
    """
    Filter rows to be validated by conditions:
     - paper was included in the further analysis (Include == 1)
     - if analyst is provided, the paper was analyzed by them
    """
    df = df_full.copy()

    # Pick only the included papers
    if included:
        pmids_included = list(df.PMID[df.Include == 1])
        df = df[df.PMID.isin(pmids_included)]

    # Filter rows by the analyst
    if analyst:
        df = df[df.Analyst == analyst]

    return df


def validate_own(df, df_all, codebook, 
                 ignore_rows=[], ignore_cols=[], 
                 only_rows=[], only_cols=[],
                 n=20, debug=False):
    logger.info('Validating the table...')
    errors_df_included = validate(df, codebook, ignore_cols, included=True)
    errors_df_all = validate(df_all, codebook, ignore_cols, included=False)
    errors_df = pd.concat([errors_df_all, errors_df_included])
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

    df = filter_rows(df_full, analyst=args.analyst, included=True)
    df_all = filter_rows(df_full, analyst=args.analyst, included=False)

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

    validate_own(df, df_all, codebook, ignore_rows,
                 ignore_cols, only_rows, only_cols,
                 args.show, args.debug)


if __name__ == "__main__":
    args = parser.parse_args()
    if args.debug:
        logger.setLevel(logging.DEBUG)    

    main(args)
