import { EDIT_SHEET } from "./config";
import {
    ACCREDITATION_VALUES,
    GOVERNANCE_VALUES,
    SIZE_VALUES,
    SUBJECT_VALUES,
} from "./allowed-values";
import {
    SheetRule,
    applySheetRules,
    dvCheckbox,
    dvDropdown,
    dvRequiredNonBlankText,
    dvMuseumPicker,
    dvOptionalDate,
    dvOptionalYearOrYearRangeFormat,
    dvPostcode,
    dvWikidataId
} from "./sheet-rules";

export function buildEditSheetRules(): SheetRule[] {
    return [
	{
	    col: EDIT_SHEET.READY_TO_COMMIT,
	    rule: dvCheckbox(false),
	    clearFirst: true
	},
	{
	    col: EDIT_SHEET.MUSEUM,
	    rule: dvMuseumPicker(),
	    clearFirst: true,
	},
	{
	    col: EDIT_SHEET.WIKIDATA_ID,
	    ruleFactory: ({ a1TopLeft }) => dvWikidataId(a1TopLeft),
	    clearFirst: true,
	},
	{
	    col: EDIT_SHEET.POSTCODE,
	    ruleFactory: ({ a1TopLeft }) => dvPostcode(a1TopLeft),
	    clearFirst: true,
	},
	{
	    col: EDIT_SHEET.ACCREDITATION,
	    rule: dvDropdown(ACCREDITATION_VALUES),
	    clearFirst: true,
	},
	{
	    col: EDIT_SHEET.ACCREDITATION_CHANGE_DATE,
	    rule: dvOptionalDate("Optional: enter a date"),
	    clearFirst: true,
	},
	{
	    col: EDIT_SHEET.GOVERNANCE,
	    rule: dvDropdown(GOVERNANCE_VALUES),
	    clearFirst: true,
	},
	{
	    col: EDIT_SHEET.PREVIOUS_GOVERNANCE,
	    rule: dvDropdown(GOVERNANCE_VALUES, { allowEmpty: true }),
	    clearFirst: true,
	},
	{
	    col: EDIT_SHEET.PREVIOUS_GOVERNANCE_START,
	    ruleFactory: ({ a1TopLeft }) =>
		dvOptionalYearOrYearRangeFormat(a1TopLeft),
	    clearFirst: true,
	},
	{
	    col: EDIT_SHEET.PREVIOUS_GOVERNANCE_END,
	    ruleFactory: ({ a1TopLeft }) =>
		dvOptionalYearOrYearRangeFormat(a1TopLeft),
	    clearFirst: true,
	},
	{
	    col: EDIT_SHEET.SIZE,
	    rule: dvDropdown(SIZE_VALUES),
	    clearFirst: true,
	},
	{
	    col: EDIT_SHEET.SUBJECT,
	    rule: dvDropdown(SUBJECT_VALUES),
	    clearFirst: true,
	},
	{
	    col: EDIT_SHEET.YEAR_OPENED,
	    ruleFactory: ({ a1TopLeft }) =>
		dvOptionalYearOrYearRangeFormat(a1TopLeft),
	    clearFirst: true,
	},
	{
	    col: EDIT_SHEET.YEAR_CLOSED,
	    ruleFactory: ({ a1TopLeft }) =>
		dvOptionalYearOrYearRangeFormat(a1TopLeft),
	    clearFirst: true,
	},
    ];
}

export function setupEditSheetValidations(): void {
    const sheet = SpreadsheetApp.getActive().getSheetByName(EDIT_SHEET.NAME);
    if (!sheet) {
	throw new Error(`Missing sheet "${EDIT_SHEET.NAME}"`);
    }
    applySheetRules(
	sheet,
	buildEditSheetRules(),
	EDIT_SHEET.HEADER_ROW + 1,
    );
}
