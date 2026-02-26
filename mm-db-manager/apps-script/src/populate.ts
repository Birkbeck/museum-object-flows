import { DB_SHEET, EDIT_SHEET, DELETE_SHEET } from "./config";
import { asTrimmedString, parseMuseumId } from "./normalizers";
import { insertDatabaseToForm } from "./insert";

/**
 * Simple trigger / installable trigger handler.
 *
 * Populates Edit/Delete sheet rows when the user changes the "MUSEUM" cell ("id - name").
 */
export function onEdit(e: GoogleAppsScript.Events.SheetsOnEdit): void {
    if (!e) {
	return;
    }
    const range = e.range;
    const sheet = range.getSheet();
    const sheetName = sheet.getName();
    // Only react to single-cell edits.
    if (range.getNumRows() !== 1 || range.getNumColumns() !== 1) {
	return;
    }
    if (sheetName === EDIT_SHEET.NAME) {
	maybePopulateFromMuseumCell({
	    formSheet: sheet,
	    formRowNumber: range.getRow(),
	    editedCol1: range.getColumn(),
	    formMapping: EDIT_SHEET,
	    museumCol1: (EDIT_SHEET.MUSEUM as number) + 1,
	});
	return;
    }
    if (sheetName === DELETE_SHEET.NAME) {
	maybePopulateFromMuseumCell({
	    formSheet: sheet,
	    formRowNumber: range.getRow(),
	    editedCol1: range.getColumn(),
	    formMapping: DELETE_SHEET,
	    museumCol1: (DELETE_SHEET.MUSEUM as number) + 1,
	});
	return;
    }
}

function maybePopulateFromMuseumCell(args: {
    formSheet: GoogleAppsScript.Spreadsheet.Sheet;
    formRowNumber: number;
    editedCol1: number; // 1-indexed column edited
    museumCol1: number; // 1-indexed museum column
    formMapping: Record<string, number | string>;
}): void {
    const { formSheet, formRowNumber, editedCol1, museumCol1, formMapping } = args;
    // Ignore header row and edits to other columns
    const headerRow1 = (formMapping.HEADER_ROW as number) + 1;
    if (formRowNumber <= headerRow1) {
	return;
    }
    if (editedCol1 !== museumCol1) {
	return;
    }
    const museumCell = formSheet.getRange(formRowNumber, editedCol1).getValue();
    const museumId = parseMuseumId(museumCell);
    if (!museumId) {
	clearFormRowExceptReady(formSheet, formRowNumber, formMapping);
	return;
    }
    const ss = SpreadsheetApp.getActive();
    const dbSheet = ss.getSheetByName(DB_SHEET.NAME);
    if (!dbSheet) {
	throw new Error(`Missing sheet: ${DB_SHEET.NAME}`);
    }
    const idToRow = buildDbIdRowMap(dbSheet);
    const dbRowNumber = idToRow.get(museumId);
    if (!dbRowNumber) {
	// Invalid ID: clear row to avoid misleading data, but preserve ready checkbox.
	clearFormRowExceptReady(formSheet, formRowNumber, formMapping);
	SpreadsheetApp.getUi().alert(`Museum ID "${museumId}" not found in ${DB_SHEET.NAME}.`);
	return;
    }
    const dbLastCol = dbSheet.getLastColumn();
    const dbRowValues = dbSheet.getRange(dbRowNumber, 1, 1, dbLastCol).getValues()[0];
    try {
	insertDatabaseToForm({
	    formSheet,
	    formRowNumber,
	    formMapping,
	    dbRowValues,
	    preserveReadyColumn: true,
	});
    } catch (err) {
	const msg =
	    `Couldn't populate this row because some database values don't match the ` +
	    `${formSheet.getName()} sheet's validation.\n\n` +
	    `Museum ID: ${museumId}\n` +
	    `Database row: ${dbRowNumber}\n\n`;
	SpreadsheetApp.getUi().alert(msg);
	console.error("Populate failed", { museumId, dbRowNumber, err });
	return;
    }
}

function clearFormRowExceptReady(
    formSheet: GoogleAppsScript.Spreadsheet.Sheet,
    formRowNumber: number,
    formMapping: Record<string, number | string>
): void {
    const lastCol = formSheet.getLastColumn();
    const existing = formSheet.getRange(formRowNumber, 1, 1, lastCol).getValues()[0] ?? [];
    const outRow: unknown[] = new Array(lastCol).fill("");
    if (typeof formMapping.READY_TO_COMMIT === "number") {
	outRow[formMapping.READY_TO_COMMIT] = existing[formMapping.READY_TO_COMMIT];
    }
    if (typeof formMapping.READY_TO_DELETE === "number") {
	outRow[formMapping.READY_TO_DELETE] = existing[formMapping.READY_TO_DELETE];
    }
    outRow[formMapping.MUSEUM] = existing[formMapping.MUSEUM];
    formSheet.getRange(formRowNumber, 1, 1, lastCol).setValues([outRow]);
}

function buildDbIdRowMap(dbSheet: GoogleAppsScript.Spreadsheet.Sheet): Map<string, number> {
    const map = new Map<string, number>();
    const lastRow1 = dbSheet.getLastRow();
    const headerRow1 = DB_SHEET.HEADER_ROW + 1;
    if (lastRow1 <= headerRow1) {
	return map;
    }
    const numDataRows = lastRow1 - headerRow1;
    const idColIndex = DB_SHEET.ID + 1;
    const idValues = dbSheet.getRange(headerRow1 + 1, idColIndex, numDataRows, 1).getValues();
    for (let i = 0; i < idValues.length; i++) {
	const id = asTrimmedString(idValues[i][0]);
	if (!id) {
	    continue;
	}
	const sheetRowNumber = headerRow1 + 1 + i;
	map.set(id, sheetRowNumber);
    }
    return map;
}
