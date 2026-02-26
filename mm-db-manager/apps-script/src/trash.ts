import { callCloudEndpoint, formatCloudError } from "./cloud-api";
import { formatErrors } from "./format-errors";

type RowError = { row: number; errors: string[] };

type TrashMuseumsResponse = {
    ok: boolean;
    trashedCount: number;
    errorsByRow: RowError[];
    skippedNotReady: number;
    message: string;
};

export function trashMuseums(): void {
    try {
        const resp = callCloudEndpoint<TrashMuseumsResponse>("trashMuseums");
        const errors = resp.errorsByRow ?? [];
        if (errors.length > 0) {
            SpreadsheetApp.getUi().alert(formatErrors(errors, resp.trashedCount ?? 0));
            return;
        }
        SpreadsheetApp.getUi().alert(resp.message || "Trash completed.");
    } catch (err) {
        SpreadsheetApp.getUi().alert(formatCloudError("Trash", err));
    }
}
