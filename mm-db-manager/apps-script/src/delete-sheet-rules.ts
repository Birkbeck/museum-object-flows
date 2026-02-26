import { DELETE_SHEET } from "./config";
import {
    SheetRule,
    applySheetRules,
    dvCheckbox,
    dvMuseumPicker,
} from "./sheet-rules";

export function buildDeleteSheetRules(): SheetRule[] {
    return [
	{
	    col: DELETE_SHEET.READY_TO_DELETE,
	    rule: dvCheckbox(false),
	    clearFirst: true
	},
	{
	    col: DELETE_SHEET.MUSEUM,
	    rule: dvMuseumPicker(),
	    clearFirst: true,
	},
    ];
}

export function setupDeleteSheetValidations(): void {
    const sheet = SpreadsheetApp.getActive().getSheetByName(DELETE_SHEET.NAME);
    if (!sheet) {
	throw new Error(`Missing sheet "${DELETE_SHEET.NAME}"`);
    }
    applySheetRules(
	sheet,
	buildDeleteSheetRules(),
	DELETE_SHEET.HEADER_ROW + 1,
    );
}
