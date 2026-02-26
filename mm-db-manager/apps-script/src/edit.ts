import { callCloudEndpoint, formatCloudError } from "./cloud-api";
import { formatErrors } from "./format-errors";

type RowError = { row: number; errors: string[] };

type EditMuseumsResponse = {
    ok: boolean;
    editedCount: number;
    errorsByRow: RowError[];
    skippedNotReady: number;
    message: string;
};

export function editMuseums(): void {
    try {
        const resp = callCloudEndpoint<EditMuseumsResponse>("editMuseums");
        const errors = resp.errorsByRow ?? [];
        if (errors.length > 0) {
            SpreadsheetApp.getUi().alert(formatErrors(errors, resp.editedCount ?? 0));
            return;
        }
        SpreadsheetApp.getUi().alert(resp.message || "Edit completed.");
    } catch (err) {
        SpreadsheetApp.getUi().alert(formatCloudError("Edit", err));
    }
}
