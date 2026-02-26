/**
 * Helpers for removing rows from form-style sheets (Add, Edit, etc.).
 *
 * This module contains no locking logic.
 * Callers are responsible for acquiring any required document lock
 * around multi-step operations.
 */

/**
 * Deletes a single row from a form sheet.
 *
 * Important:
 * - When deleting multiple rows, callers should delete bottom-up
 *   to avoid shifting row indices.
 *
 * @param sheet The sheet to delete from
 * @param sheetRowNumber 1-indexed row number to delete
 */
export function deleteFormRow(args: {
    ss: GoogleAppsScript.Spreadsheet.Spreadsheet;
    sheetName: string;
    sheetRowNumber: number; // 1-indexed
}): void {
    const { ss, sheetName, sheetRowNumber } = args;
    const sheet = ss.getSheetByName(sheetName);
    if (!sheet) throw new Error(`Missing sheet: ${sheetName}`);
    sheet.deleteRow(sheetRowNumber);
}
