import { INSTRUCTIONS_SHEET } from "./config";

/**
 * Writes the current date/time to the Instructions sheet.
 *
 * Intended to be called after every successful database mutation
 * (add / edit / delete / restore / permanent delete).
 */
export function logDatabaseChangeDate(): void {
  const ss = SpreadsheetApp.getActive();
  const sheet = ss.getSheetByName(INSTRUCTIONS_SHEET.NAME);
  if (!sheet) {
    throw new Error(`Missing sheet "${INSTRUCTIONS_SHEET.NAME}"`);
  }
  const row = INSTRUCTIONS_SHEET.DATE_ROW + 1;
  const col = INSTRUCTIONS_SHEET.DATE_COL + 1;
  sheet.getRange(row, col).setValue(new Date());
}
