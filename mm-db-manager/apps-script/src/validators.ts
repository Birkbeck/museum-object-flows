import {
  ACCREDITATION_VALUES,
  GOVERNANCE_VALUES,
  SIZE_VALUES,
  SUBJECT_VALUES
} from "./allowed-values";

export function validateRow(values: unknown[], columns: Record<string, number|string>): string[] {
    const errors: string[] = [];
    if (isEmpty(values[columns.MUSEUM_NAME])) {
	errors.push("Museum must have a name.")
    }
    if (
	isNotEmpty(values[columns.WIKIDATA_ID])
	    && !isValidWikidataId(values[columns.WIKIDATA_ID])
    ) {
	errors.push(
	    `Wikidata ID ${values[columns.WIKIDATA_ID]} is not a valid Wikidata ID.`
	)
    }
    if (!isValidPostcode(values[columns.POSTCODE])) {
	errors.push(
	    `Postcode ${values[columns.POSTCODE]} is not a correctly formatted UK postcode.`
	)
    }
    if (!isValidAccreditation(values[columns.ACCREDITATION])) {
	errors.push(
	    `Accreditation ${values[columns.ACCREDITATION]} is not a valid accreditation status.`
	)
    }
    if (
	isNotEmpty(values[columns.ACCREDITATION_NUMBER])
	    && !isValidAccreditationNumber(values[columns.ACCREDITATION_NUMBER])
    ) {
	errors.push(
	    `Accreditation number ${values[columns.ACCREDITATION_NUMBER]} is not a valid accreditation number.`
	)
    }
    if (
	isNotEmpty(values[columns.ACCREDITATION_CHANGE_DATE])
	    && !isValidYearRange(values[columns.ACCREDITATION_CHANGE_DATE])
    ) {
	errors.push(
	    `Date accreditation status changed ${values[columns.ACCREDITATION_CHANGE_DATE]} is not a valid year range.`
	)
    }
    if (!isValidGovernance(values[columns.GOVERNANCE])) {
	errors.push(
	    `Governance ${values[columns.GOVERNANCE]} is not a valid governance type.`
	)
    }
    if (
	isNotEmpty(values[columns.PREVIOUS_GOVERNANCE])
	    && !isValidGovernance(values[columns.PREVIOUS_GOVERNANCE])
    ) {
	errors.push(
	    `Previous governance ${values[columns.PREVIOUS_GOVERNANCE]} is not a valid governance type.`
	)
    }
    if (
	isNotEmpty(values[columns.PREVIOUS_GOVERNANCE_START])
	    && !isValidYearRange(values[columns.PREVIOUS_GOVERNANCE_START])
    ) {
	errors.push(
	    `Start date of previous governance ${values[columns.PREVIOUS_GOVERNANCE_START]} is not a valid year range.`
	)
    }
    if (
	isNotEmpty(values[columns.PREVIOUS_GOVERNANCE_END])
	    && !isValidYearRange(values[columns.PREVIOUS_GOVERNANCE_END])
    ) {
	errors.push(
	    `End date of previous governance ${values[columns.PREVIOUS_GOVERNANCE_END]} is not a valid year range.`
	)
    }
    if (!isValidSize(values[columns.SIZE])) {
	errors.push(
	    `Size ${values[columns.SIZE]} is not a valid museum size.`
	)
    }
    if (!isValidSubject(values[columns.SUBJECT])) {
	errors.push(
	    `Subject ${values[columns.SUBJECT]} is not a valid museum subject matter.`
	)
    }
    if (!isValidYearRange(values[columns.YEAR_OPENED])) {
	errors.push(
	    `Year opened ${values[columns.YEAR_OPENED]} is not a valid year range.`
	)
    }
    if (!isValidYearRange(values[columns.YEAR_CLOSED])) {
	errors.push(
	    `Year closed ${values[columns.YEAR_CLOSED]} is not a valid year range.`
	)
    }
    return errors
}

export function isEmpty(value: unknown): boolean {
    return value === null || value === undefined || String(value).trim() === "";
}

export function isNotEmpty(value: unknown): boolean {
    return !isEmpty(value);
}

export function isBoolean(value: unknown): value is boolean {
  return typeof value === "boolean";
}

export function isValidWikidataId(value: unknown): boolean {
    if (typeof value !== "string") return false;
    return /^Q\d+$/.test(value.trim());
}

export function isValidPostcode(value: unknown): boolean {
    if (typeof value !== "string") return false;
    return /^[A-Z]{1,2}\d[A-Z\d]?\s+\d[A-Z]{2}$/i.test(value.trim());
}

export function isValidAccreditation(value: unknown): boolean {
    if (typeof value !== "string") return false;
    return ACCREDITATION_VALUES.has(value);
}

export function isValidAccreditationNumber(value: unknown): boolean {
    if (value === null || value === undefined || value === "") {
	return false;
    }
    const num =
	typeof value === "number"
	? value
	: typeof value === "string"
        ? Number(value)
        : NaN;
    return Number.isInteger(num) && num > 0;
}

export function isValidYearRange(value: unknown): boolean {
    if (typeof value !== "string" && typeof value !== "number") {
        return false;
    }
    const trimmed = String(value).trim();
    // Must be YYYY or YYYY/YYYY
    if (!/^\d{4}(?:\/\d{4})?$/.test(trimmed)) {
        return false;
    }
    const [startStr, endStr] = trimmed.split("/");
    const start = Number(startStr);
    const end = endStr ? Number(endStr) : start;
    return start <= end;
}

export function isValidGovernance(value: unknown): boolean {
    if (typeof value !== "string") return false;
    return GOVERNANCE_VALUES.has(value);
}

export function isValidSize(value: unknown): boolean {
    if (typeof value !== "string") return false;
    return SIZE_VALUES.has(value);
}

export function isValidSubject(value: unknown): boolean {
    if (typeof value !== "string") return false;
    return SUBJECT_VALUES.has(value);
}
