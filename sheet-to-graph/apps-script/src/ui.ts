export function onOpenMenu(): void {
  SpreadsheetApp.getUi()
    .createMenu("Dispersal Database")
    .addItem("Update Database", "translateDatabase")
    .addToUi();
}
