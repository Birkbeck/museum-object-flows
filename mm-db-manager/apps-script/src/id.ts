import { DB_SHEET, TRASH_SHEET, NEW_ID_SHEET } from "./config";
import { withDocumentLock } from "./lock";

/**
 * Allocates a new museum ID of the form:
 *   mm.new.<n>
 *
 * Uses NEW_ID_SHEET.LAST_ID as the single source of truth.
 *
 * If the ID sheet doesn't exist yet, it will be created and seeded with the
 * largest existing numeric suffix found anywhere in the DB + trash sheets.
 */
export function allocateNextMuseumId(
    ss: GoogleAppsScript.Spreadsheet.Spreadsheet
): string {
    return withDocumentLock(() => {
	const idSheet = ensureIdSheetSeeded(ss);
	const cell = idSheet.getRange(NEW_ID_SHEET.LAST_ID);
	const raw = cell.getValue();
	const lastIssued = parseCounterCell(raw);
	const next = lastIssued + 1;
	cell.setValue(next);
	return `mm.new.${next}`;
    });
}

/**
 * Ensure NEW_ID_SHEET exists. If it doesn't, create it and seed LAST_ID with the
 * maximum mm.new.<n> found in DB + trash.
 */
function ensureIdSheetSeeded(
    ss: GoogleAppsScript.Spreadsheet.Spreadsheet
): GoogleAppsScript.Spreadsheet.Sheet {
    let idSheet = ss.getSheetByName(NEW_ID_SHEET.NAME);
    if (idSheet) {
	return idSheet;
    }
    idSheet = ss.insertSheet(NEW_ID_SHEET.NAME);
    const maxExisting = findMaxMuseumIdSuffixInDbAndTrash(ss);
    idSheet.getRange(NEW_ID_SHEET.LAST_ID).setValue(maxExisting);
    try {
	idSheet.hideSheet();
    } catch {
	// ignore
    }
    return idSheet;
}

function findMaxMuseumIdSuffixInDbAndTrash(
    ss: GoogleAppsScript.Spreadsheet.Spreadsheet
): number {
    const sheetNames = [DB_SHEET.NAME, TRASH_SHEET.NAME];
    let max = 0;
    for (const name of sheetNames) {
	const sheet = ss.getSheetByName(name);
	if (!sheet) {
	    continue;
	}
	max = Math.max(max, findMaxMuseumIdSuffixInSheet(sheet));
    }
    return max;
}

function findMaxMuseumIdSuffixInSheet(
    sheet: GoogleAppsScript.Spreadsheet.Sheet
): number {
    const range = sheet.getDataRange();
    const values = range.getValues();
    let max = 0;
    for (let r = 0; r < values.length; r++) {
	for (let c = 0; c < values[r].length; c++) {
	    const v = values[r][c];
	    if (v == null) {
		continue;
	    }
	    const s = String(v);
	    const m = s.match(/\bmm\.new\.(\d+)\b/);
	    if (!m) {
		continue;
	    }
	    const n = Number(m[1]);
	    if (Number.isFinite(n) && Number.isInteger(n) && n >= 0) {
		if (n > max) {
		    max = n;
		}
	    }
	}
    }
    return max;
}

function parseCounterCell(value: unknown): number {
    if (
	typeof value === "number" &&
	    Number.isFinite(value) &&
	    Number.isInteger(value) &&
	    value >= 0
    ) {
	return value;
    }
    if (typeof value === "string") {
	const s = value.trim();
	if (s === "") {
	    return 0;
	}
	const n = Number(s);
	if (Number.isFinite(n) && Number.isInteger(n) && n >= 0) {
	    return n;
	}
    }
    if (value === null || value === undefined || value === "") {
	return 0;
    }
    throw new Error(
	`Invalid NEW_ID_SHEET counter value: "${String(
value
)}" (expected non-negative integer)`
    );
}
