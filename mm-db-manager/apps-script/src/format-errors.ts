export function formatErrors(
    errorsByRow: Array<{ row: number; errors: string[] }>,
    addedCount: number
): string {
    const lines: string[] = [];
    if (addedCount > 0) {
	lines.push(
	    addedCount === 1 ? `Added ${addedCount} museum.` : `Added ${addedCount} museums.`
	);
	lines.push("");
    }
    lines.push("Some rows could not be added:");
    for (const { row, errors } of errorsByRow) {
	lines.push(`Row ${row}:`);
	for (const e of errors) lines.push(`  - ${e}`);
    }
    return lines.join("\n");
}
