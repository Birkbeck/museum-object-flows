import { callCloudEndpoint, formatCloudError } from "./cloud-api";
import { formatErrors } from "./format-errors";

type RowError = { row: number; errors: string[] };

type PermanentlyDeleteMuseumsResponse = {
    ok: boolean;
    deletedCount: number;
    errorsByRow: RowError[];
    skippedNotReady?: number;
    skippedNotMarked?: number;
    message: string;
};

export function permanentlyDeleteMuseums(): void {
    try {
        const resp = callCloudEndpoint<PermanentlyDeleteMuseumsResponse>("permanentlyDeleteMuseums");
        const errors = resp.errorsByRow ?? [];
        if (errors.length > 0) {
            SpreadsheetApp.getUi().alert(formatErrors(errors, resp.deletedCount ?? 0));
            return;
        }
        SpreadsheetApp.getUi().alert(resp.message || "Permanent delete completed.");
    } catch (err) {
        SpreadsheetApp.getUi().alert(formatCloudError("Permanent delete", err));
    }
}
