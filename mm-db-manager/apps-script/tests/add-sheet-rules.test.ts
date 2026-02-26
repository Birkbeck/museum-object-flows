// Integration-ish test for add-sheet-rules with SpreadsheetApp mocked.

import { setupAddSheetValidations } from "../src/add-sheet-rules";
import { ADD_SHEET } from "../src/config";

type DV =
    | { kind: "checkbox"; allowInvalid: boolean; helpText?: string }
    | { kind: "date"; allowInvalid: boolean; helpText?: string }
    | { kind: "list"; values: string[]; showDropdown: boolean; allowInvalid: boolean; helpText?: string }
    | { kind: "formula"; formula: string; allowInvalid: boolean; helpText?: string }
    | { kind: "range"; showDropdown: boolean; allowInvalid: boolean; helpText?: string };

class FakeDataValidationBuilder {
    private dv: Partial<DV> = {};
    requireCheckbox() {
	this.dv = { kind: "checkbox" } as Partial<DV>;
	return this;
    }
    requireDate() {
	this.dv = { kind: "date" } as Partial<DV>;
	return this;
    }
    requireValueInList(values: string[], showDropdown: boolean) {
	this.dv = { kind: "list", values, showDropdown } as Partial<DV>;
	return this;
    }
    requireFormulaSatisfied(formula: string) {
	this.dv = { kind: "formula", formula } as Partial<DV>;
	return this;
    }
    requireValueInRange(_range: any, showDropdown: boolean) {
	// Not used by Add sheet rules, but included for completeness.
	this.dv = { kind: "range", showDropdown } as Partial<DV>;
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
	// default allowInvalid if not set
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
    // Present if your code ever calls it; harmless otherwise
    clearContent = jest.fn();
    setValues = jest.fn();
    getRow() {
	return this.row;
    }
    getColumn() {
	return this.col;
    }
}

class FakeSheet {
    private maxRows: number;
    public ranges: FakeRange[] = [];
    public insertCalls: Array<{ after: number; howMany: number }> = [];
    
    constructor(public name: string, initialMaxRows: number) {
	this.maxRows = initialMaxRows;
    }
    
    getName() {
	return this.name;
    }
    
    getMaxRows() {
	return this.maxRows;
    }
    
    // Some code paths use getLastRow; for our purposes, equal to maxRows
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

describe("setupAddSheetValidations (integration-ish)", () => {
    beforeEach(() => {
	// Provide global SpreadsheetApp mock used by your rule builders.
	(globalThis as any).SpreadsheetApp = {
	    getActive: jest.fn(),
	    newDataValidation: jest.fn(() => new FakeDataValidationBuilder()),
	};
    });
    
    test("expands sheet to cover DEFAULT_DATA_ROWS and applies validations from row 2 down", () => {
	// Add sheet starts with only header row present.
	const addSheet = new FakeSheet(ADD_SHEET.NAME, 1);
	const ss = new FakeSpreadsheet({ [ADD_SHEET.NAME]: addSheet });
	
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	setupAddSheetValidations();
	
	expect(addSheet.insertCalls.length).toBe(1);
	expect(addSheet.insertCalls[0]).toEqual({ after: 1, howMany: 1000 });
	expect(addSheet.getMaxRows()).toBe(1001);
	
	expect(addSheet.ranges.length).toBeGreaterThan(0);
	for (const r of addSheet.ranges) {
	    expect(r.row).toBe(2);
	    expect(r.numRows).toBe(1000);
	    expect(r.numCols).toBe(1);
	    expect(r.clearDataValidations).toHaveBeenCalledTimes(1);
	    expect(r.setDataValidation).toHaveBeenCalledTimes(1);
	}
    });
    
    test("injects correct A1 top-left into formula-based DV (e.g. Wikidata uses D2)", () => {
	const addSheet = new FakeSheet(ADD_SHEET.NAME, 1);
	const ss = new FakeSpreadsheet({ [ADD_SHEET.NAME]: addSheet });
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	setupAddSheetValidations();
	
	const wikidataCol1 = ADD_SHEET.WIKIDATA_ID + 1;
	
	const wikidataRanges = addSheet.ranges.filter((r) => r.col === wikidataCol1);
	expect(wikidataRanges.length).toBe(1);
	
	const dvArg = wikidataRanges[0].setDataValidation.mock.calls[0][0] as DV;
	expect(dvArg.kind).toBe("formula");
	
	if (dvArg.kind === "formula") {
	    expect(dvArg.formula).toContain("D2");
	    expect(dvArg.formula).toMatch(/REGEXMATCH/i);
	}
    });
    
    test("throws a clear error if the Add sheet is missing", () => {
	const ss = new FakeSpreadsheet({});
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	expect(() => setupAddSheetValidations()).toThrow(
	    `Missing sheet "${ADD_SHEET.NAME}"`
	);
    });
});
