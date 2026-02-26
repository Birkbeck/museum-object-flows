import { onOpenMenu } from "./ui";
import { addMuseums as addMuseums_ } from "./add";
import { editMuseums as editMuseums_ } from "./edit";
import { trashMuseums as trashMuseums_ } from "./trash";
import { restoreMuseums as restoreMuseums_ } from "./restore";
import { permanentlyDeleteMuseums as permanentlyDeleteMuseums_ } from "./permanently-delete";
import { publishDatabase as publishDatabase_ } from "./publish";
import { refreshMuseumListSheet } from "./sheet-rules";
import { setupAllSheetValidations } from "./setup-sheet-validations";
import { onEdit as onEdit_ } from "./populate";

function onOpenScript(e?: GoogleAppsScript.Events.SheetsOnOpen): void {
    refreshMuseumListSheet();
    setupAllSheetValidations();
    onOpenMenu();
}
function onEditScript(e: GoogleAppsScript.Events.SheetsOnEdit): void {
    onEdit_(e);
}
function addMuseums(): void { addMuseums_(); }
function editMuseums(): void { editMuseums_(); }
function trashMuseums(): void { trashMuseums_(); }
function restoreMuseums(): void { restoreMuseums_(); }
function permanentlyDeleteMuseums(): void { permanentlyDeleteMuseums_(); }
function publishDatabase(): void { publishDatabase_(); }

Object.assign(globalThis as any, {
    __mm_onOpen: onOpenScript,
    __mm_onEdit: onEditScript,
    __mm_addMuseums: addMuseums,
    __mm_editMuseums: editMuseums,
    __mm_trashMuseums: trashMuseums,
    __mm_restoreMuseums: restoreMuseums,
    __mm_permanentlyDeleteMuseums: permanentlyDeleteMuseums,
    __mm_publishDatabase: publishDatabase,
});
