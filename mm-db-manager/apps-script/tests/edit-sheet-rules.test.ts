// Integration-ish test for edit-sheet-rules with SpreadsheetApp mocked.

import { setupEditSheetValidations } from "../src/edit-sheet-rules";
import { EDIT_SHEET } from "../src/config";

jest.mock("../src/sheet-rules", () => {
    const actual = jest.requireActual("../src/sheet-rules");
    return {
	...actual,
	dvMuseumPicker: jest.fn(() => ({ kind: "range", allowInvalid: false, showDropdown: true })),
    };
});

type DV =
    | { kind: "checkbox"; allowInvalid: boolean; helpText?: string }
    | { kind: "date"; allowInvalid: boolean; helpText?: string }
    | { kind: "list"; values: string[]; showDropdown: boolean; allowInvalid: boolean; helpText?: string }
    | { kind: "formula"; formula: string; allowInvalid: boolean; helpText?: string }
    | { kind: "range"; showDropdown: boolean; allowInvalid?: boolean; helpText?: string };

class FakeDataValidationBuilder {
    private dv: Partial<DV> = {};
    requireCheckbox() {
	this.dv = { kind: "checkbox" };
	return this;
    }
    requireDate() {
	this.dv = { kind: "date" };
	return this;
    }
    requireValueInList(values: string[], showDropdown: boolean) {
	this.dv = { kind: "list", values, showDropdown };
	return this;
    }
    requireFormulaSatisfied(formula: string) {
	this.dv = { kind: "formula", formula };
	return this;
    }
    requireValueInRange(_range: any, showDropdown: boolean) {
	this.dv = { kind: "range", showDropdown };
	return this;
    }
    setAllowInvalid(allowInvalid: boolean) {
	(this.dv as any).allowInvalid = allowInvalid;
	return this;
    }
    setHelpText(helpText: string) {
	(this.dv as any).helpText = helpText;
	return this;
    }
    build() {
	if ((this.dv as any).allowInvalid === undefined) {
	    (this.dv as any).allowInvalid = false;
	}
	return this.dv as DV;
    }
}

class FakeRange {
    constructor(
	public row: number,
	public col: number,
	public numRows: number,
	public numCols: number
    ) {}
    clearDataValidations = jest.fn();
    setDataValidation = jest.fn();
    clearContent = jest.fn();
    setValues = jest.fn();
}

class FakeSheet {
    private maxRows: number;
    public ranges: FakeRange[] = [];
    public insertCalls: Array<{ after: number; howMany: number }> = [];
    
    constructor(public name: string, initialMaxRows: number) {
	this.maxRows = initialMaxRows;
    }
    
    getMaxRows() {
	return this.maxRows;
    }
    
    getLastRow() {
	return this.maxRows;
    }
    
    insertRowsAfter(afterPosition: number, howMany: number) {
	this.insertCalls.push({ after: afterPosition, howMany });
	this.maxRows += howMany;
    }
    
    getRange(row: number, col: number, numRows = 1, numCols = 1) {
	const r = new FakeRange(row, col, numRows, numCols);
	this.ranges.push(r);
	return r as any;
    }
    
    hideSheet() {
	// no-op
    }
}

class FakeSpreadsheet {
    constructor(private sheets: Record<string, FakeSheet>) {}
    getSheetByName(name: string) {
	return this.sheets[name] ?? null;
    }
    insertSheet(name: string) {
	const sh = new FakeSheet(name, 1);
	this.sheets[name] = sh;
	return sh;
    }
}

describe("setupEditSheetValidations (integration-ish)", () => {
    beforeEach(() => {
	(globalThis as any).SpreadsheetApp = {
	    getActive: jest.fn(),
	    newDataValidation: jest.fn(() => new FakeDataValidationBuilder()),
	};
    });
    
    test("expands the Edit sheet and applies validations from row 2 down", () => {
	// Start with just header row
	const editSheet = new FakeSheet(EDIT_SHEET.NAME, 1);
	const ss = new FakeSpreadsheet({ [EDIT_SHEET.NAME]: editSheet });
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	setupEditSheetValidations();
	
	// With DEFAULT_DATA_ROWS=1000, headerRow1=1, endRow1=1001 -> insert 1000 rows
	expect(editSheet.insertCalls.length).toBe(1);
	expect(editSheet.insertCalls[0]).toEqual({ after: 1, howMany: 1000 });
	expect(editSheet.getMaxRows()).toBe(1001);
	
	// Each rule should apply to rows 2..1001 (1000 rows), 1 column wide
	expect(editSheet.ranges.length).toBeGreaterThan(0);
	for (const r of editSheet.ranges) {
	    expect(r.row).toBe(2);
	    expect(r.numRows).toBe(1000);
	    expect(r.numCols).toBe(1);
	    
	    expect(r.clearDataValidations).toHaveBeenCalledTimes(1);
	    expect(r.setDataValidation).toHaveBeenCalledTimes(1);
	}
    });
    
    test("applies museum dropdown to the MUSEUM column (range-based DV from dvMuseumPicker)", () => {
	const editSheet = new FakeSheet(EDIT_SHEET.NAME, 1);
	const ss = new FakeSpreadsheet({ [EDIT_SHEET.NAME]: editSheet });
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	setupEditSheetValidations();
	
	const museumCol1 = EDIT_SHEET.MUSEUM + 1;
	const museumRanges = editSheet.ranges.filter((r) => r.col === museumCol1);
	expect(museumRanges.length).toBe(1);
	
	const dvArg = museumRanges[0].setDataValidation.mock.calls[0][0] as DV;
	expect(dvArg.kind).toBe("range");
    });
    
    test("injects correct A1 top-left into formula DVs (Wikidata uses E2; Postcode uses J2)", () => {
	const editSheet = new FakeSheet(EDIT_SHEET.NAME, 1);
	const ss = new FakeSpreadsheet({ [EDIT_SHEET.NAME]: editSheet });
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	setupEditSheetValidations();
	
	const wikidataCol1 = EDIT_SHEET.WIKIDATA_ID + 1;
	const wikidataRange = editSheet.ranges.find((r) => r.col === wikidataCol1);
	expect(wikidataRange).toBeTruthy();
	const wikidataDv = wikidataRange!.setDataValidation.mock.calls[0][0] as DV;
	expect(wikidataDv.kind).toBe("formula");
	if (wikidataDv.kind === "formula") {
	    expect(wikidataDv.formula).toContain("E2");
	    expect(wikidataDv.formula).toMatch(/REGEXMATCH/i);
	}
	
	const postcodeCol1 = EDIT_SHEET.POSTCODE + 1;
	const postcodeRange = editSheet.ranges.find((r) => r.col === postcodeCol1);
	expect(postcodeRange).toBeTruthy();
	const postcodeDv = postcodeRange!.setDataValidation.mock.calls[0][0] as DV;
	expect(postcodeDv.kind).toBe("formula");
	if (postcodeDv.kind === "formula") {
	    expect(postcodeDv.formula).toContain("J2");
	    expect(postcodeDv.formula).toMatch(/REGEXMATCH/i);
	}
    });
    
    test("throws a clear error if the Edit sheet is missing", () => {
	const ss = new FakeSpreadsheet({});
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	expect(() => setupEditSheetValidations()).toThrow(
	    `Missing sheet "${EDIT_SHEET.NAME}"`
	);
    });
});
