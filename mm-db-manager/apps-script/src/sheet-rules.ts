// Generic helpers for defining + applying Google Sheets data validation rules
// by zero-indexed column indices.
//
// Apps Script Range.setDataValidation copies the DV rule across the range.
// For formula DV: we build a formula referencing the top-left cell (e.g. C2).
// Sheets will adjust the references as it fills down.

import { DB_SHEET, MUSEUM_LIST_SHEET } from "./config";

export type SheetRule = {
    /** Zero-indexed column index (0 = A). */
    col: number;
    /** Static validation rule for the whole column range. */
    rule?: GoogleAppsScript.Spreadsheet.DataValidation;
    /** validation rule factory needed when formula references cell being validated. */
    ruleFactory?: (ctx: {
	sheet: GoogleAppsScript.Spreadsheet.Sheet;
	col: number;
	startRow: number; // 1-indexed
	endRow: number; // 1-indexed
	a1TopLeft: string; // e.g. "C2"
    }) => GoogleAppsScript.Spreadsheet.DataValidation;
    /** Clear any existing validation rules on that column range first. */
    clearFirst?: boolean;
};

const DEFAULT_DATA_ROWS = 1000;

export function colToA1(col0: number): string {
    if (!Number.isInteger(col0) || col0 < 0) {
	throw new Error(`Invalid col index: ${col0}`);
    }
    let n = col0 + 1;
    let s = "";
    while (n > 0) {
	const rem = (n - 1) % 26;
	s = String.fromCharCode(65 + rem) + s;
	n = Math.floor((n - 1) / 26);
    }
    return s;
}

export function cellA1(col0: number, row1: number): string {
    return `${colToA1(col0)}${row1}`;
}

export function ensureSheetHasRows(
    sheet: GoogleAppsScript.Spreadsheet.Sheet,
    minRows: number
): void {
    const maxRows = sheet.getMaxRows();
    if (maxRows >= minRows) return;
    sheet.insertRowsAfter(maxRows, minRows - maxRows);
}

export function applySheetRules(
    sheet: GoogleAppsScript.Spreadsheet.Sheet,
    rules: SheetRule[],
    // headerRow must be 1-indexed to match sheet layout
    headerRow: number
): void {
    const startRow = headerRow + 1;
    const endRow = Math.max(sheet.getMaxRows(), headerRow + DEFAULT_DATA_ROWS);
    // Ensure at least one data row exists and enough rows to cover the DV range
    ensureSheetHasRows(sheet, Math.max(endRow, startRow));
    if (sheet.getMaxRows() < startRow) {
	return;
    }
    const effectiveEndRow = Math.min(endRow, sheet.getMaxRows());
    for (const r of rules) {
	if (!r.rule && !r.ruleFactory) {
	    throw new Error(`Rule for col ${r.col} must provide rule or ruleFactory`);
	}
	const range = sheet.getRange(
	    startRow,
	    r.col + 1,
	    effectiveEndRow - startRow + 1,
	    1
	);
	if (r.clearFirst) {
	    range.clearDataValidations();
	}
	const topLeft = cellA1(r.col, startRow);
	const dv =
	    r.rule ??
	    r.ruleFactory!({
		sheet,
		col: r.col,
		startRow: startRow,
		endRow: effectiveEndRow,
		a1TopLeft: topLeft,
	    });
	range.setDataValidation(dv);
    }
}

/* -----------------------------------------
 * Data Validation Rule/RuleFactory builders
 * -------------------------------------- */

export function dvCheckbox(
    allowInvalid = false
): GoogleAppsScript.Spreadsheet.DataValidation {
    return SpreadsheetApp.newDataValidation()
	.requireCheckbox()
	.setAllowInvalid(allowInvalid)
	.build();
}

export function dvRequiredNonBlankText(
    a1TopLeft: string,
    helpText?: string
): GoogleAppsScript.Spreadsheet.DataValidation {
    const formula = `=LEN(TRIM(${a1TopLeft}))>0`;
    const b = SpreadsheetApp.newDataValidation()
	.requireFormulaSatisfied(formula)
	.setAllowInvalid(false);
    if (helpText) {
	b.setHelpText(helpText);
    }
    return b.build();
}

export function dvOptionalRegex(
    a1TopLeft: string,
    regex: string,
    helpText?: string
): GoogleAppsScript.Spreadsheet.DataValidation {
    const formula = `=OR(LEN(TRIM(${a1TopLeft}))=0, REGEXMATCH(TRIM(${a1TopLeft}), "${regex}"))`;
    const b = SpreadsheetApp.newDataValidation()
	.requireFormulaSatisfied(formula)
	.setAllowInvalid(false);
    if (helpText) {
	b.setHelpText(helpText);
    }
    return b.build();
}

export function dvPostcode(
  a1TopLeft: string,
  helpText = "Optional: UK postcode with a space (e.g. SW1A 1AA)"
): GoogleAppsScript.Spreadsheet.DataValidation {
  // Uppercase + trim before validation
  // Format: 1–2 letters, digit, optional letter/digit, space, digit, 2 letters
  const formula =
    `=OR(` +
    `LEN(TRIM(${a1TopLeft}))=0, ` +
    `REGEXMATCH(` +
      `UPPER(TRIM(${a1TopLeft})), ` +
      `"^[A-Z]{1,2}[0-9][A-Z0-9]?\\s[0-9][A-Z]{2}$"` +
    `)` +
    `)`;

  const b = SpreadsheetApp.newDataValidation()
    .requireFormulaSatisfied(formula)
    .setAllowInvalid(false)
    .setHelpText(helpText);

  return b.build();
}

export function dvWikidataId(
  a1TopLeft: string,
  helpText = "Optional: Wikidata ID like Q12345"
): GoogleAppsScript.Spreadsheet.DataValidation {
  // Allow blank OR Q + digits
  const formula =
    `=OR(` +
    `LEN(TRIM(${a1TopLeft}))=0, ` +
    `REGEXMATCH(TRIM(${a1TopLeft}), "^Q[0-9]+$")` +
    `)`;

  const b = SpreadsheetApp.newDataValidation()
    .requireFormulaSatisfied(formula)
    .setAllowInvalid(false)
    .setHelpText(helpText);

  return b.build();
}


export function dvOptionalYearOrYearRangeFormat(
    a1TopLeft: string,
    helpText?: string
): GoogleAppsScript.Spreadsheet.DataValidation {
    // Format only: YYYY or YYYY/YYYY
    const regex = "^\\d{4}(?:\\/\\d{4})?$";
    return dvOptionalRegex(a1TopLeft, regex, helpText ?? "Optional: YYYY or YYYY/YYYY");
}

export function dvOptionalDate(
    helpText?: string
): GoogleAppsScript.Spreadsheet.DataValidation {
    const b = SpreadsheetApp.newDataValidation()
	.requireDate()
	.setAllowInvalid(false);
    if (helpText) b.setHelpText(helpText);
    return b.build();
}

export function dvDropdown(
    values: ReadonlySet<string> | readonly string[] | string[],
    options?: { allowEmpty?: boolean; helpText?: string; allowInvalid?: boolean }
): GoogleAppsScript.Spreadsheet.DataValidation {
    const arr = Array.isArray(values) ? [...values] : Array.from(values);
    if (arr.length === 0) {
	throw new Error("dvDropdown: values must be non-empty");
    }
    const allowEmpty = options?.allowEmpty ?? false;
    const allowInvalid = options?.allowInvalid ?? false;
    const helpText = options?.helpText;
    const finalValues = allowEmpty ? [...arr, ""] : arr;
    const b = SpreadsheetApp.newDataValidation()
	.requireValueInList(finalValues, true)
	.setAllowInvalid(allowInvalid);
    if (helpText) {
	b.setHelpText(helpText);
    }
    return b.build();
}

/**
 * Data validation rule for a "Museum" picker dropdown.
 * Requires that refreshMuseumListSheet() has been run (e.g. onOpen).
 *
 * - Uses requireValueInRange (scales well)
 * - Dropdown shows "id - name"
 * - Empty allowed (first entry is blank)
 */
export function dvMuseumPicker(): GoogleAppsScript.Spreadsheet.DataValidation {
    const ss = SpreadsheetApp.getActive();
    const list = ss.getSheetByName(MUSEUM_LIST_SHEET.NAME);
    if (!list) {
	throw new Error(`Missing sheet "${MUSEUM_LIST_SHEET.NAME}". Run refreshMuseumListSheet().`);
    }
    const headerRow = MUSEUM_LIST_SHEET.HEADER_ROW + 1;
    const startRow = headerRow + 1;
    const col = MUSEUM_LIST_SHEET.VALUE + 1;
    const lastRow = list.getLastRow();
    const endRow = Math.max(lastRow, startRow);
    const range = list.getRange(startRow, col, endRow - startRow + 1, 1);
    return SpreadsheetApp.newDataValidation()
	.requireValueInRange(range, true)
	.setAllowInvalid(false)
	.build();
}

/**
 * Rebuild the Museum List sheet from the Database sheet as:
 *   "id - museum_name"
 *
 * - Sorted alphabetically by museum_name (case-insensitive)
 *
 * Safe to call repeatedly.
 */
export function refreshMuseumListSheet(): void {
    const ss = SpreadsheetApp.getActive();
    const db = ss.getSheetByName(DB_SHEET.NAME);
    if (!db) {
	throw new Error(`Missing sheet "${DB_SHEET.NAME}"`);
    }
    let list = ss.getSheetByName(MUSEUM_LIST_SHEET.NAME);
    if (!list) {
	list = ss.insertSheet(MUSEUM_LIST_SHEET.NAME);
    }
    const dbHeaderRow = DB_SHEET.HEADER_ROW + 1;
    const firstDataRow = dbHeaderRow + 1;
    const lastRow = db.getLastRow();
    const width = Math.max(DB_SHEET.ID, DB_SHEET.MUSEUM_NAME) + 1;
    const items: Array<{ id: string; name: string }> = [];
    if (lastRow >= firstDataRow) {
	const numRows = lastRow - firstDataRow + 1;
	const values = db.getRange(firstDataRow, 1, numRows, width).getValues();
	for (const row of values) {
	    const id = String(row[DB_SHEET.ID] ?? "").trim();
	    const name = String(row[DB_SHEET.MUSEUM_NAME] ?? "").trim();
	    if (!id || !name) continue;
	    items.push({ id, name });
	}
    }
    items.sort(
	(a, b) =>
	    a.name.localeCompare(b.name, undefined, { sensitivity: "base" })
    );
    let out: string[][] = items.map((x) => [`${x.id} - ${x.name}`]);
    if (out.length === 0) {
	out = [[""]];
    }
    const listHeaderRow = MUSEUM_LIST_SHEET.HEADER_ROW + 1;
    const listStartRow = listHeaderRow + 1;
    const outputCol = MUSEUM_LIST_SHEET.VALUE + 1;
    const neededRows = listStartRow + out.length - 1;
    if (list.getMaxRows() < neededRows) {
	list.insertRowsAfter(list.getMaxRows(), neededRows - list.getMaxRows());
    }
    // Clear a generous range, not based on getLastRow() (safe on new sheets)
    list
	.getRange(listStartRow, outputCol, Math.max(2000, out.length + 10), 1)
	.clearContent();
    // Write output
    list.getRange(listStartRow, outputCol, out.length, 1).setValues(out);
    try {
	list.hideSheet();
    } catch {
	// ignore
    }
}
