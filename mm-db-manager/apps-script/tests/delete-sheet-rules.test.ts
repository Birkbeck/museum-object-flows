// Integration-ish test for delete-sheet-rules with SpreadsheetApp mocked.

import { setupDeleteSheetValidations } from "../src/delete-sheet-rules";
import { DELETE_SHEET } from "../src/config";

jest.mock("../src/sheet-rules", () => {
    const actual = jest.requireActual("../src/sheet-rules");
    return {
	...actual,
	dvMuseumPicker: jest.fn(() => ({ kind: "range", allowInvalid: false, showDropdown: true })),
    };
});

type DV =
    | { kind: "checkbox"; allowInvalid: boolean; helpText?: string }
    | { kind: "formula"; formula: string; allowInvalid: boolean; helpText?: string }
    | { kind: "list"; values: string[]; showDropdown: boolean; allowInvalid: boolean; helpText?: string }
    | { kind: "date"; allowInvalid: boolean; helpText?: string }
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
}

describe("setupDeleteSheetValidations (integration-ish)", () => {
    beforeEach(() => {
	(globalThis as any).SpreadsheetApp = {
	    getActive: jest.fn(),
	    newDataValidation: jest.fn(() => new FakeDataValidationBuilder()),
	};
    });
    
    test("expands the Delete sheet and applies validations from row 2 down", () => {
	const deleteSheet = new FakeSheet(DELETE_SHEET.NAME, 1); // header row only
	const ss = new FakeSpreadsheet({ [DELETE_SHEET.NAME]: deleteSheet });
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	setupDeleteSheetValidations();
	
	// With DEFAULT_DATA_ROWS=1000 and headerRow1=1 => endRow1=1001, insert 1000 rows.
	expect(deleteSheet.insertCalls.length).toBe(1);
	expect(deleteSheet.insertCalls[0]).toEqual({ after: 1, howMany: 1000 });
	expect(deleteSheet.getMaxRows()).toBe(1001);
	
	// Two rules -> two ranges, both should start at row 2 and span 1000 rows.
	expect(deleteSheet.ranges.length).toBe(2);
	
	for (const r of deleteSheet.ranges) {
	    expect(r.row).toBe(2);
	    expect(r.numRows).toBe(1000);
	    expect(r.numCols).toBe(1);
	    expect(r.clearDataValidations).toHaveBeenCalledTimes(1);
	    expect(r.setDataValidation).toHaveBeenCalledTimes(1);
	}
    });
    
    test("applies checkbox DV to READY_TO_DELETE column", () => {
	const deleteSheet = new FakeSheet(DELETE_SHEET.NAME, 1);
	const ss = new FakeSpreadsheet({ [DELETE_SHEET.NAME]: deleteSheet });
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	setupDeleteSheetValidations();
	
	const readyCol1 = DELETE_SHEET.READY_TO_DELETE + 1;
	const readyRange = deleteSheet.ranges.find((r) => r.col === readyCol1);
	expect(readyRange).toBeTruthy();
	
	const dv = readyRange!.setDataValidation.mock.calls[0][0] as DV;
	expect(dv.kind).toBe("checkbox");
    });
    
    test("applies museum picker dropdown DV to MUSEUM column", () => {
	const deleteSheet = new FakeSheet(DELETE_SHEET.NAME, 1);
	const ss = new FakeSpreadsheet({ [DELETE_SHEET.NAME]: deleteSheet });
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	setupDeleteSheetValidations();
	
	const museumCol1 = DELETE_SHEET.MUSEUM + 1;
	const museumRange = deleteSheet.ranges.find((r) => r.col === museumCol1);
	expect(museumRange).toBeTruthy();
	
	const dv = museumRange!.setDataValidation.mock.calls[0][0] as DV;
	expect(dv.kind).toBe("range");
    });
    
    test("throws a clear error if the Delete sheet is missing", () => {
	const ss = new FakeSpreadsheet({});
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	expect(() => setupDeleteSheetValidations()).toThrow(
	    `Missing sheet "${DELETE_SHEET.NAME}"`
	);
    });
});
