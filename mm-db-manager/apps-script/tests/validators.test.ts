import {
    isEmpty,
    isNotEmpty,
    isBoolean,
    isValidWikidataId,
    isValidPostcode,
    isValidAccreditation,
    isValidAccreditationNumber,
    isValidYearRange,
    isValidGovernance,
    isValidSize,
    isValidSubject
} from "../src/validators";
import {
    ACCREDITATION_VALUES,
    GOVERNANCE_VALUES,
    SIZE_VALUES,
    SUBJECT_VALUES
} from "../src/allowed-values";

function firstOf<T>(set: ReadonlySet<T>): T {
    const v = set.values().next().value as T | undefined;
    if (v === undefined) {
	throw new Error("Test requires the Set to contain at least one value.");
    }
    return v;
}

describe("isEmpty / isNotEmpty", () => {
    test.each([
	[null],
	[undefined],
	[""],
	["   "],
	["\n\t "],
    ])("isEmpty(%p) is true", (v) => {
	expect(isEmpty(v)).toBe(true);
    });
    
    test.each([
	["a"],
	["  a  "],
	[0],
	[123],
	[false],
	[true],
	[new Date()],
	[["x"]],
	[{ a: 1 }],
    ])("isEmpty(%p) is false", (v) => {
	expect(isEmpty(v)).toBe(false);
    });
    
    test("isNotEmpty is the negation of isEmpty", () => {
	const samples: unknown[] = [null, undefined, "", " ", "x", 0, false, true];
	for (const s of samples) {
	    expect(isNotEmpty(s)).toBe(!isEmpty(s));
	}
    });
});

describe("isBoolean", () => {
    test.each([[true], [false]])("isBoolean(%p) is true", (v) => {
	expect(isBoolean(v)).toBe(true);
    });
    
    test.each([
	[""],
	["TRUE"],
	["FALSE"],
	[0],
	[1],
	[null],
	[undefined],
	[{}],
	[new Date()],
    ])("isBoolean(%p) is false", (v) => {
	expect(isBoolean(v)).toBe(false);
    });
});

describe("isValidWikidataId", () => {
    test.each([
	["Q1"],
	["Q42"],
	["Q42 "],
	["Q1234567890"],
	["  Q123  "],
    ])("accepts %p", (v) => {
	expect(isValidWikidataId(v)).toBe(true);
    });
    
    test.each([
	["Q"],
	["q42"],
	["Q-1"],
	["Q 42"],
	["Q42a"],
	[42],
	[null],
	[undefined],
    ])("rejects %p", (v) => {
	expect(isValidWikidataId(v)).toBe(false);
    });
});

describe("isValidPostcode (UK, requires space)", () => {
    test.each([
	["SW1A 1AA"],
	["sw1a 1aa"],
	["M1 1AE"],
	["EC1V 9LB"],
	["W1A 0AX"],
	["B33 8TH"],
	["  B33 8TH  "],
    ])("accepts %p", (v) => {
	expect(isValidPostcode(v)).toBe(true);
    });
    
    test.each([
	["SW1A1AA"],
	["EC1V9LB"],
	["B338TH"],
	["INVALID"],
	["123 456"],
	[""],
	["   "],
	[null],
	[undefined],
	[123],
    ])("rejects %p", (v) => {
	expect(isValidPostcode(v)).toBe(false);
    });
});

describe("isValidAccreditationNumber", () => {
    test.each([
	[1],
	[123],
	["1"],
	["00123"],
	[" 42 "],
    ])("accepts %p", (v) => {
	expect(isValidAccreditationNumber(v)).toBe(true);
    });
    
    test.each([
	[0],
	[-1],
	["0"],
	["-2"],
	["12.3"],
	[12.3],
	["12A"],
	[""],
	["   "],
	[null],
	[undefined],
	[true],
	[false],
    ])("rejects %p", (v) => {
	expect(isValidAccreditationNumber(v)).toBe(false);
    });
});

describe("isValidYearRange", () => {
    test.each([
	[1999],
	["1999"],
	["0000"],
	["1999/2000"],
	["2020/2020"],
	[" 2001/2002 "],
    ])("accepts %p", (v) => {
	expect(isValidYearRange(v)).toBe(true);
    });
    
    test.each([
	["1999/1998"],
	["199"],
	[199],
	["19999"],
	["1999/20"],
	["1999 / 2000"],
	["1999-2000"],
	["1999:2000"],
	["abcd"],
	[null],
	[undefined],
    ])("rejects %p", (v) => {
	expect(isValidYearRange(v)).toBe(false);
    });
});

describe("Set-based validators (accreditation/governance/size/subject)", () => {
    test("isValidAccreditation accepts a known allowed value", () => {
	const allowed = firstOf(ACCREDITATION_VALUES);
	expect(isValidAccreditation(allowed)).toBe(true);
    });
    
    test("isValidGovernance accepts a known allowed value", () => {
	const allowed = firstOf(GOVERNANCE_VALUES);
	expect(isValidGovernance(allowed)).toBe(true);
    });
    
    test("isValidSize accepts a known allowed value", () => {
	const allowed = firstOf(SIZE_VALUES);
	expect(isValidSize(allowed)).toBe(true);
    });
    
    test("isValidSubject accepts a known allowed value", () => {
	const allowed = firstOf(SUBJECT_VALUES);
	expect(isValidSubject(allowed)).toBe(true);
    });
    
    test.each([
	["DefinitelyNotARealAllowedValue"],
	[""],
	["   "],
	[null],
	[undefined],
	[123],
	[true],
    ])("all Set-based validators reject %p", (v) => {
	expect(isValidAccreditation(v)).toBe(false);
	expect(isValidGovernance(v)).toBe(false);
	expect(isValidSize(v)).toBe(false);
	expect(isValidSubject(v)).toBe(false);
    });
});
