export function asTrimmedString(v: unknown): string {
    return typeof v === "string" ? v.trim() : String(v ?? "").trim();
}

export function getBroadType(value: unknown): string {
    if (typeof value !== "string") return "";
    const trimmed = value.trim();
    if (!trimmed) return "";
    const [broad] = trimmed.split(":");
    return broad.trim();
}

export function joinYearRange(start: unknown, end: unknown): string {
    const s1 = asTrimmedString(start);
    const s2 = asTrimmedString(end);
    if (!s1 && !s2) {
	return "";
    }
    if (!s2 || s2 === s1) {
	return s1;
    }
    return `${s1}/${s2}`;
}

export function splitYearRange(value: unknown): [string, string] {
    if (typeof value !== "string" && typeof value !== "number") {
        return ["", ""];
    }
    const trimmed = String(value).trim();
    if (!trimmed) {
        return ["", ""];
    }
    const [start, end] = trimmed.split("/");
    return [start, end ?? start];
}

export function parseMuseumId(value: unknown): string | null {
    if (typeof value !== "string") {
	return null;
    }
    const s = value.trim();
    if (!s) {
	return null;
    }
    const m = /^(.+?)\s*-\s*.+$/.exec(s);
    if (!m) {
	return null;
    }
    const id = m[1].trim();
    if (!id) {
	return null;
    }
    return id;
}
