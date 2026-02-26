import { callCloudEndpoint, formatCloudError } from "./cloud-api";
import { formatErrors } from "./format-errors";

type RowError = { row: number; errors: string[] };

type AddMuseumsResponse = {
    ok: boolean;
    addedCount: number;
    errorsByRow: RowError[];
    skippedNotReady: number;
    message: string;
};

export function addMuseums(): void {
    try {
        const resp = callCloudEndpoint<AddMuseumsResponse>("addMuseums");
        const errors = resp.errorsByRow ?? [];
        if (errors.length > 0) {
            SpreadsheetApp.getUi().alert(formatErrors(errors, resp.addedCount ?? 0));
            return;
        }
        SpreadsheetApp.getUi().alert(resp.message || "Add completed.");
    } catch (err) {
        SpreadsheetApp.getUi().alert(formatCloudError("Add", err));
    }
}
