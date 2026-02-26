// Integration-ish tests for onEdit() with SpreadsheetApp mocked

import { onEdit } from "../src/populate";
import { DB_SHEET, EDIT_SHEET, DELETE_SHEET } from "../src/config";

type MockRange = {
    getValue?: jest.Mock;
    getValues?: jest.Mock;
    setValues?: jest.Mock;
    getRow?: jest.Mock;
    getColumn?: jest.Mock;
    getNumRows?: jest.Mock;
    getNumColumns?: jest.Mock;
    getSheet?: jest.Mock;
};

type MockSheet = {
    getName: jest.Mock;
    getLastColumn: jest.Mock;
    getRange: jest.Mock;
};

type MockDbSheet = {
    getLastRow: jest.Mock;
    getLastColumn: jest.Mock;
    getRange: jest.Mock;
};

type MockSpreadsheet = {
    getSheetByName: jest.Mock;
};

function makeUi() {
    return { alert: jest.fn() };
}

function buildRow(lastCol: number, values: Record<number, unknown>): unknown[] {
    const row = new Array(lastCol).fill("");
    for (const [k, v] of Object.entries(values)) row[Number(k)] = v;
    return row;
}

function makeEvent(args: {
    sheet: MockSheet;
    row: number;
    col: number;
    numRows?: number;
    numCols?: number;
}): any {
    const range: MockRange = {
	getSheet: jest.fn(() => args.sheet as any),
	getRow: jest.fn(() => args.row),
	getColumn: jest.fn(() => args.col),
	getNumRows: jest.fn(() => args.numRows ?? 1),
	getNumColumns: jest.fn(() => args.numCols ?? 1),
    };
    return { range } as any;
}

describe("onEdit populate (SpreadsheetApp mocked; real insertDatabaseToForm)", () => {
    beforeEach(() => {
	jest.resetAllMocks();
	const ui = makeUi();
	(globalThis as any).SpreadsheetApp = {
	    getActive: jest.fn(),
	    getUi: jest.fn(() => ui),
	};
    });
    
    test("ignores edits on other sheets", () => {
	const otherSheet: MockSheet = {
	    getName: jest.fn(() => "Some Other Sheet"),
	    getLastColumn: jest.fn(() => 10),
	    getRange: jest.fn(),
	};
	const ss: MockSpreadsheet = { getSheetByName: jest.fn() };
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	onEdit(makeEvent({ sheet: otherSheet, row: 2, col: 2 }));
	
	expect(otherSheet.getRange).not.toHaveBeenCalled();
	const ui = (globalThis as any).SpreadsheetApp.getUi();
	expect(ui.alert).not.toHaveBeenCalled();
    });
    
    test("ignores multi-cell edits", () => {
	const editSheet: MockSheet = {
	    getName: jest.fn(() => EDIT_SHEET.NAME),
	    getLastColumn: jest.fn(() => EDIT_SHEET.NOTES + 1),
	    getRange: jest.fn(),
	};
	const ss: MockSpreadsheet = { getSheetByName: jest.fn() };
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	onEdit(makeEvent({ sheet: editSheet, row: 2, col: 2, numRows: 2, numCols: 1 }));
	
	expect(editSheet.getRange).not.toHaveBeenCalled();
    });
    
    test("ignores edits not in the MUSEUM column", () => {
	const lastCol = EDIT_SHEET.NOTES + 1;
	const editSheet: MockSheet = {
	    getName: jest.fn(() => EDIT_SHEET.NAME),
	    getLastColumn: jest.fn(() => lastCol),
	    getRange: jest.fn(),
	};
	
	const ss: MockSpreadsheet = { getSheetByName: jest.fn() };
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	onEdit(makeEvent({ sheet: editSheet, row: 2, col: EDIT_SHEET.MUSEUM_NAME + 1 }));
	
	expect(editSheet.getRange).not.toHaveBeenCalled();
    });
    
    test("Edit sheet: invalid museum value clears row except READY_TO_COMMIT and MUSEUM", () => {
	const lastCol = EDIT_SHEET.NOTES + 1;
	const existingRow = buildRow(lastCol, {
	    [EDIT_SHEET.READY_TO_COMMIT]: true,
	    [EDIT_SHEET.MUSEUM]: "whatever user typed",
	    [EDIT_SHEET.MUSEUM_NAME]: "Should be cleared",
	    [EDIT_SHEET.POSTCODE]: "SW1A 1AA",
	});
	const museumCellRange: MockRange = { getValue: jest.fn(() => "invalid") };
	const fullRowRange: MockRange = {
	    getValues: jest.fn(() => [existingRow]),
	    setValues: jest.fn(),
	};
	const editSheet: MockSheet = {
	    getName: jest.fn(() => EDIT_SHEET.NAME),
	    getLastColumn: jest.fn(() => lastCol),
	    getRange: jest.fn((row: number, col: number, numRows?: number, numCols?: number) => {
		if (row === 2 && col === EDIT_SHEET.MUSEUM + 1 && (numRows ?? 1) === 1 && (numCols ?? 1) === 1) {
		    return museumCellRange as any;
		}
		if (row === 2 && col === 1 && numRows === 1 && numCols === lastCol) {
		    return fullRowRange as any;
		}
		return { getValue: jest.fn(), getValues: jest.fn(), setValues: jest.fn() } as any;
	    }),
	};
	const ss: MockSpreadsheet = { getSheetByName: jest.fn() };
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	onEdit(makeEvent({ sheet: editSheet, row: 2, col: EDIT_SHEET.MUSEUM + 1 }));
	
	expect(fullRowRange.setValues).toHaveBeenCalledTimes(1);
	const [[written2d]] = fullRowRange.setValues!.mock.calls as unknown[][];
	const writtenRow = (written2d as any[][])[0];
	expect(writtenRow[EDIT_SHEET.READY_TO_COMMIT]).toBe(true);
	expect(writtenRow[EDIT_SHEET.MUSEUM]).toBe("whatever user typed");
	expect(writtenRow[EDIT_SHEET.MUSEUM_NAME]).toBe("");
	expect(writtenRow[EDIT_SHEET.POSTCODE]).toBe("");
    });
    
    test("Edit sheet: valid museum id populates row from DB", () => {
	const formLastCol = EDIT_SHEET.NOTES + 1;
	const dbLastCol = DB_SHEET.NOTES + 1;
	const existingFormRow = buildRow(formLastCol, {
	    [EDIT_SHEET.READY_TO_COMMIT]: true,
	    [EDIT_SHEET.MUSEUM]: "mm.new.7 - Whatever",
	    [EDIT_SHEET.MUSEUM_NAME]: "stale",
	});
	const dbRow = new Array(dbLastCol).fill("");
	dbRow[DB_SHEET.ID] = "mm.new.7";
	dbRow[DB_SHEET.MUSEUM_NAME] = "Museum A";
	dbRow[DB_SHEET.POSTCODE] = "SW1A 1AA";
	dbRow[DB_SHEET.YEAR_OPENED_1] = "1999";
	dbRow[DB_SHEET.YEAR_OPENED_2] = "2003";
	dbRow[DB_SHEET.YEAR_CLOSED_1] = "2010";
	dbRow[DB_SHEET.YEAR_CLOSED_2] = "2010";
	dbRow[DB_SHEET.GOVERNANCE] = "local authority";
	dbRow[DB_SHEET.SUBJECT] = "art: painting";
	dbRow[DB_SHEET.NOTES] = "hello";
	const museumCellRange: MockRange = { getValue: jest.fn(() => "mm.new.7 - Museum A") };
	const fullRowRange: MockRange = {
	    getValues: jest.fn(() => [existingFormRow]),
	    setValues: jest.fn(),
	};
	const editSheet: MockSheet = {
	    getName: jest.fn(() => EDIT_SHEET.NAME),
	    getLastColumn: jest.fn(() => formLastCol),
	    getRange: jest.fn((row: number, col: number, numRows?: number, numCols?: number) => {
		if (row === 2 && col === EDIT_SHEET.MUSEUM + 1 && (numRows ?? 1) === 1 && (numCols ?? 1) === 1) {
		    return museumCellRange as any;
		}
		if (row === 2 && col === 1 && numRows === 1 && numCols === formLastCol) {
		    return fullRowRange as any;
		}
		return { getValue: jest.fn(), getValues: jest.fn(), setValues: jest.fn() } as any;
	    }),
	};
	
	const dbIdScanRange: MockRange = { getValues: jest.fn(() => [["mm.new.7"]]) };
	const dbReadRange: MockRange = { getValues: jest.fn(() => [dbRow]) };
	const dbSheet: MockDbSheet = {
	    getLastRow: jest.fn(() => DB_SHEET.HEADER_ROW + 1 + 1),
	    getLastColumn: jest.fn(() => dbLastCol),
	    getRange: jest.fn((row: number, col: number, numRows: number, numCols: number) => {
		if (numCols === 1 && col === DB_SHEET.ID + 1) return dbIdScanRange as any;
		if (col === 1 && numRows === 1 && numCols === dbLastCol) return dbReadRange as any;
		return { getValues: jest.fn() } as any;
	    }),
	};
	const ss: MockSpreadsheet = {
	    getSheetByName: jest.fn((name: string) => {
		if (name === DB_SHEET.NAME) return dbSheet as any;
		return null;
	    }),
	};
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	onEdit(makeEvent({ sheet: editSheet, row: 2, col: EDIT_SHEET.MUSEUM + 1 }));
	
	expect(fullRowRange.setValues).toHaveBeenCalledTimes(1);
	const [[written2d]] = fullRowRange.setValues!.mock.calls as unknown[][];
	const writtenRow = (written2d as any[][])[0];
	expect(writtenRow[EDIT_SHEET.READY_TO_COMMIT]).toBe("");
	expect(writtenRow[EDIT_SHEET.MUSEUM]).toBe("mm.new.7 - Museum A");
	expect(writtenRow[EDIT_SHEET.MUSEUM_NAME]).toBe("Museum A");
	expect(writtenRow[EDIT_SHEET.POSTCODE]).toBe("SW1A 1AA");
	expect(writtenRow[EDIT_SHEET.GOVERNANCE]).toBe("local authority");
	expect(writtenRow[EDIT_SHEET.SUBJECT]).toBe("art: painting");
	expect(writtenRow[EDIT_SHEET.NOTES]).toBe("hello");
	expect(writtenRow[EDIT_SHEET.YEAR_OPENED]).toBe("1999/2003");
	expect(writtenRow[EDIT_SHEET.YEAR_CLOSED]).toBe("2010");
    });
    
    test("Delete sheet: valid museum id populates row from DB (READY_TO_DELETE preserved)", () => {
	const formLastCol = DELETE_SHEET.NOTES + 1;
	const dbLastCol = DB_SHEET.NOTES + 1;
	const existingFormRow = buildRow(formLastCol, {
	    [DELETE_SHEET.READY_TO_DELETE]: true,
	    [DELETE_SHEET.MUSEUM]: "mm.new.7 - Whatever",
	    [DELETE_SHEET.MUSEUM_NAME]: "stale",
	});
	const dbRow = new Array(dbLastCol).fill("");
	dbRow[DB_SHEET.ID] = "mm.new.7";
	dbRow[DB_SHEET.MUSEUM_NAME] = "Museum A";
	dbRow[DB_SHEET.POSTCODE] = "SW1A 1AA";
	const museumCellRange: MockRange = { getValue: jest.fn(() => "mm.new.7 - Museum A") };
	const fullRowRange: MockRange = {
	    getValues: jest.fn(() => [existingFormRow]),
	    setValues: jest.fn(),
	};
	const deleteSheet: MockSheet = {
	    getName: jest.fn(() => DELETE_SHEET.NAME),
	    getLastColumn: jest.fn(() => formLastCol),
	    getRange: jest.fn((row: number, col: number, numRows?: number, numCols?: number) => {
		if (row === 2 && col === DELETE_SHEET.MUSEUM + 1 && (numRows ?? 1) === 1 && (numCols ?? 1) === 1) {
		    return museumCellRange as any;
		}
		if (row === 2 && col === 1 && numRows === 1 && numCols === formLastCol) {
		    return fullRowRange as any;
		}
		return { getValue: jest.fn(), getValues: jest.fn(), setValues: jest.fn() } as any;
	    }),
	};
	const dbIdScanRange: MockRange = { getValues: jest.fn(() => [["mm.new.7"]]) };
	const dbReadRange: MockRange = { getValues: jest.fn(() => [dbRow]) };
	const dbSheet: MockDbSheet = {
	    getLastRow: jest.fn(() => DB_SHEET.HEADER_ROW + 1 + 1),
	    getLastColumn: jest.fn(() => dbLastCol),
	    getRange: jest.fn((row: number, col: number, numRows: number, numCols: number) => {
		if (numCols === 1 && col === DB_SHEET.ID + 1) return dbIdScanRange as any;
		if (col === 1 && numRows === 1 && numCols === dbLastCol) return dbReadRange as any;
		return { getValues: jest.fn() } as any;
	    }),
	};
	const ss: MockSpreadsheet = {
	    getSheetByName: jest.fn((name: string) => {
		if (name === DB_SHEET.NAME) return dbSheet as any;
		return null;
	    }),
	};
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	onEdit(makeEvent({ sheet: deleteSheet, row: 2, col: DELETE_SHEET.MUSEUM + 1 }));
	
	expect(fullRowRange.setValues).toHaveBeenCalledTimes(1);
	const [[written2d]] = fullRowRange.setValues!.mock.calls as unknown[][];
	const writtenRow = (written2d as any[][])[0];
	expect(writtenRow[DELETE_SHEET.READY_TO_DELETE]).toBe("");
	expect(writtenRow[DELETE_SHEET.MUSEUM]).toBe("mm.new.7 - Museum A");
	expect(writtenRow[DELETE_SHEET.MUSEUM_NAME]).toBe("Museum A");
	expect(writtenRow[DELETE_SHEET.POSTCODE]).toBe("SW1A 1AA");
    });
    
    test("validly formatted museum id but not found in DB: alerts and clears row except ready + museum", () => {
	const formLastCol = EDIT_SHEET.NOTES + 1;
	const dbLastCol = DB_SHEET.NOTES + 1;
	const existingFormRow = buildRow(formLastCol, {
	    [EDIT_SHEET.READY_TO_COMMIT]: true,
	    [EDIT_SHEET.MUSEUM]: "mm.new.999 - Missing",
	    [EDIT_SHEET.MUSEUM_NAME]: "stale",
	});
	const museumCellRange: MockRange = { getValue: jest.fn(() => "mm.new.999 - Missing") };
	const fullRowRange: MockRange = {
	    getValues: jest.fn(() => [existingFormRow]),
	    setValues: jest.fn(),
	};
	const editSheet: MockSheet = {
	    getName: jest.fn(() => EDIT_SHEET.NAME),
	    getLastColumn: jest.fn(() => formLastCol),
	    getRange: jest.fn((row: number, col: number, numRows?: number, numCols?: number) => {
		if (row === 2 && col === EDIT_SHEET.MUSEUM + 1 && (numRows ?? 1) === 1 && (numCols ?? 1) === 1) {
		    return museumCellRange as any;
		}
		if (row === 2 && col === 1 && numRows === 1 && numCols === formLastCol) {
		    return fullRowRange as any;
		}
		return { getValue: jest.fn(), getValues: jest.fn(), setValues: jest.fn() } as any;
	    }),
	};
	const dbIdScanRange: MockRange = { getValues: jest.fn(() => [["mm.new.1"], ["mm.new.2"]]) };
	const dbSheet: MockDbSheet = {
	    getLastRow: jest.fn(() => DB_SHEET.HEADER_ROW + 1 + 2),
	    getLastColumn: jest.fn(() => dbLastCol),
	    getRange: jest.fn((row: number, col: number, numRows: number, numCols: number) => {
		if (numCols === 1 && col === DB_SHEET.ID + 1) return dbIdScanRange as any;
		return { getValues: jest.fn() } as any;
	    }),
	};
	const ss: MockSpreadsheet = {
	    getSheetByName: jest.fn((name: string) => {
		if (name === DB_SHEET.NAME) return dbSheet as any;
		return null;
	    }),
	};
	
	(globalThis as any).SpreadsheetApp.getActive.mockReturnValue(ss);
	
	onEdit(makeEvent({ sheet: editSheet, row: 2, col: EDIT_SHEET.MUSEUM + 1 }));
	
	expect(fullRowRange.setValues).toHaveBeenCalledTimes(1);
	const [[written2d]] = fullRowRange.setValues!.mock.calls as unknown[][];
	const writtenRow = (written2d as any[][])[0];
	expect(writtenRow[EDIT_SHEET.READY_TO_COMMIT]).toBe(true);
	expect(writtenRow[EDIT_SHEET.MUSEUM]).toBe("mm.new.999 - Missing");
	expect(writtenRow[EDIT_SHEET.MUSEUM_NAME]).toBe("");
	const ui = (globalThis as any).SpreadsheetApp.getUi();
	expect(ui.alert).toHaveBeenCalledTimes(1);
	expect(ui.alert.mock.calls[0][0]).toContain("not found");
    });
});
