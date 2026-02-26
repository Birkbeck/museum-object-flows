import { TRASH_SHEET } from "./config";
import {
    SheetRule,
    applySheetRules,
    dvCheckbox,
} from "./sheet-rules";

export function buildTrashSheetRules(): SheetRule[] {
    return [
	{
	    col: TRASH_SHEET.PERMANENTLY_DELETE,
	    rule: dvCheckbox(false),
	    clearFirst: true
	},
	{
	    col: TRASH_SHEET.RESTORE,
	    rule: dvCheckbox(false),
	    clearFirst: true
	},
    ];
}

export function setupTrashSheetValidations(): void {
    const sheet = SpreadsheetApp.getActive().getSheetByName(TRASH_SHEET.NAME);
    if (!sheet) {
	throw new Error(`Missing sheet "${TRASH_SHEET.NAME}"`);
    }
    applySheetRules(
	sheet,
	buildTrashSheetRules(),
	TRASH_SHEET.HEADER_ROW + 1,
    );
}
