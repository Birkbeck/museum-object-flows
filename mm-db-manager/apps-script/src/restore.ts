import { callCloudEndpoint, formatCloudError } from "./cloud-api";
import { formatErrors } from "./format-errors";

type RowError = { row: number; errors: string[] };

type RestoreMuseumsResponse = {
    ok: boolean;
    restoredCount: number;
    errorsByRow: RowError[];
    skippedNotReady: number;
    message: string;
};

export function restoreMuseums(): void {
    try {
        const resp = callCloudEndpoint<RestoreMuseumsResponse>("restoreMuseums");
        const errors = resp.errorsByRow ?? [];
        if (errors.length > 0) {
            SpreadsheetApp.getUi().alert(formatErrors(errors, resp.restoredCount ?? 0));
            return;
        }
        SpreadsheetApp.getUi().alert(resp.message || "Restore completed.");
    } catch (err) {
        SpreadsheetApp.getUi().alert(formatCloudError("Restore", err));
    }
}
