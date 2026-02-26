// Integration-ish test for trash-sheet-rules with SpreadsheetApp mocked.

import { setupTrashSheetValidations } from "../src/trash-sheet-rules";
import { TRASH_SHEET } from "../src/config";

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

describe("setupTrashSheetValidations (integration-ish)", () => {
    beforeEach(() => {
	(globalThis as any).SpreadsheetApp = {
	    getActive: jest.fn(),
	    newDataValidation: jest.fn(() => new FakeDataValidationBuilder()),
	};
    });
    
    test("expands the Trash sheet and applies validations from row 2 down", () => {
	// Start with only header row
	const trashSheet = new FakeSheet(TRASH_SHEET.NAME, 1);
	const ss = new FakeSpreadsheet({ [TRASH_SHEET.NAME]: trashSheet });
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	setupTrashSheetValidations();
	
	// DEFAULT_DATA_ROWS=1000, headerRow1=1 -> endRow1=1001 -> inserts 1000 rows
	expect(trashSheet.insertCalls.length).toBe(1);
	expect(trashSheet.insertCalls[0]).toEqual({ after: 1, howMany: 1000 });
	expect(trashSheet.getMaxRows()).toBe(1001);
	
	// Two rules -> two ranges
	expect(trashSheet.ranges.length).toBe(2);
	
	for (const r of trashSheet.ranges) {
	    expect(r.row).toBe(2);
	    expect(r.numRows).toBe(1000);
	    expect(r.numCols).toBe(1);
	    
	    // clearFirst: true in both rules
	    expect(r.clearDataValidations).toHaveBeenCalledTimes(1);
	    expect(r.setDataValidation).toHaveBeenCalledTimes(1);
	}
    });
    
    test("applies checkbox DV to PERMANENTLY_DELETE column", () => {
	const trashSheet = new FakeSheet(TRASH_SHEET.NAME, 1);
	const ss = new FakeSpreadsheet({ [TRASH_SHEET.NAME]: trashSheet });
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	setupTrashSheetValidations();
	
	const col1 = TRASH_SHEET.PERMANENTLY_DELETE + 1;
	const range = trashSheet.ranges.find((r) => r.col === col1);
	expect(range).toBeTruthy();
	
	const dv = range!.setDataValidation.mock.calls[0][0] as DV;
	expect(dv.kind).toBe("checkbox");
    });
    
    test("applies checkbox DV to RESTORE column", () => {
	const trashSheet = new FakeSheet(TRASH_SHEET.NAME, 1);
	const ss = new FakeSpreadsheet({ [TRASH_SHEET.NAME]: trashSheet });
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	setupTrashSheetValidations();
	
	const col1 = TRASH_SHEET.RESTORE + 1;
	const range = trashSheet.ranges.find((r) => r.col === col1);
	expect(range).toBeTruthy();
	
	const dv = range!.setDataValidation.mock.calls[0][0] as DV;
	expect(dv.kind).toBe("checkbox");
    });
    
    test("throws a clear error if the Trash sheet is missing", () => {
	const ss = new FakeSpreadsheet({});
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	expect(() => setupTrashSheetValidations()).toThrow(
	    `Missing sheet "${TRASH_SHEET.NAME}"`
	);
    });
});
