import { setupAddSheetValidations } from "./add-sheet-rules";
import { setupEditSheetValidations } from "./edit-sheet-rules";
import { setupDeleteSheetValidations } from "./delete-sheet-rules";
import { setupTrashSheetValidations } from "./trash-sheet-rules";

/**
 * Re-applies all sheet validation rules.
 * Safe to call repeatedly.
 */
export function setupAllSheetValidations(): void {
  setupAddSheetValidations();
  setupEditSheetValidations();
  setupDeleteSheetValidations();
  setupTrashSheetValidations();
}
