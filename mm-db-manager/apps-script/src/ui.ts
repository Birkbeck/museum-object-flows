export function onOpenMenu(): void {
  SpreadsheetApp.getUi()
    .createMenu("Mapping Museums Database")
    .addItem("Add selected museums in Add", "addMuseums")
    .addItem("Commit selected edits in Edit", "editMuseums")
    .addItem("Trash selected museums in Delete", "trashMuseums")
    .addItem("Permanently delete selected museums in Trash", "permanentlyDeleteMuseums")
    .addItem("Restore selected museums in Trash", "restoreMuseums")
    .addItem("Publish database", "publishDatabase")
    .addToUi();
}
