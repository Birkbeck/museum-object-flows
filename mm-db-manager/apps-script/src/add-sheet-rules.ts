import { ADD_SHEET } from "./config";
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
    dvOptionalDate,
    dvOptionalYearOrYearRangeFormat,
    dvPostcode,
    dvWikidataId
} from "./sheet-rules";

export function buildAddSheetRules(): SheetRule[] {
    return [
	{
	    col: ADD_SHEET.READY_TO_COMMIT,
	    rule: dvCheckbox(false),
	    clearFirst: true,
	},
	{
	    col: ADD_SHEET.WIKIDATA_ID,
	    ruleFactory: ({ a1TopLeft }) => dvWikidataId(a1TopLeft),
	    clearFirst: true,
	},
	{
	    col: ADD_SHEET.POSTCODE,
	    ruleFactory: ({ a1TopLeft }) => dvPostcode(a1TopLeft),
	    clearFirst: true,
	},
	{
	    col: ADD_SHEET.ACCREDITATION,
	    rule: dvDropdown(ACCREDITATION_VALUES),
	    clearFirst: true,
	},
	{
	    col: ADD_SHEET.ACCREDITATION_CHANGE_DATE,
	    rule: dvOptionalDate("Optional: enter a date"),
	    clearFirst: true,
	},
	{
	    col: ADD_SHEET.GOVERNANCE,
	    rule: dvDropdown(GOVERNANCE_VALUES),
	    clearFirst: true,
	},
	{
	    col: ADD_SHEET.PREVIOUS_GOVERNANCE,
	    rule: dvDropdown(GOVERNANCE_VALUES, { allowEmpty: true }),
	    clearFirst: true,
	},
	{
	    col: ADD_SHEET.PREVIOUS_GOVERNANCE_START,
	    ruleFactory: ({ a1TopLeft }) =>
		dvOptionalYearOrYearRangeFormat(a1TopLeft),
	    clearFirst: true,
	},
	{
	    col: ADD_SHEET.PREVIOUS_GOVERNANCE_END,
	    ruleFactory: ({ a1TopLeft }) =>
		dvOptionalYearOrYearRangeFormat(a1TopLeft),
	    clearFirst: true,
	},
	{
	    col: ADD_SHEET.SIZE,
	    rule: dvDropdown(SIZE_VALUES),
	    clearFirst: true,
	},
	{
	    col: ADD_SHEET.SUBJECT,
	    rule: dvDropdown(SUBJECT_VALUES),
	    clearFirst: true,
	},
	{
	    col: ADD_SHEET.YEAR_OPENED,
	    ruleFactory: ({ a1TopLeft }) =>
		dvOptionalYearOrYearRangeFormat(a1TopLeft),
	    clearFirst: true,
	},
	{
	    col: ADD_SHEET.YEAR_CLOSED,
	    ruleFactory: ({ a1TopLeft }) =>
		dvOptionalYearOrYearRangeFormat(a1TopLeft),
	    clearFirst: true,
	},
    ];
}

export function setupAddSheetValidations(): void {
    const sheet = SpreadsheetApp.getActive().getSheetByName(ADD_SHEET.NAME);
    if (!sheet) {
	throw new Error(`Missing sheet "${ADD_SHEET.NAME}"`);
    }
    applySheetRules(
	sheet,
	buildAddSheetRules(),
	ADD_SHEET.HEADER_ROW + 1,
    );
}
