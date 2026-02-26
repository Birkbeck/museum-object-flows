import { onOpenMenu } from "./ui";
import { translateDatabase as translateDatabase_ } from "./translate-database";

function onOpenScript(e?: GoogleAppsScript.Events.SheetsOnOpen): void {
    onOpenMenu();
}
function translateDatabase(): void { translateDatabase_(); }

Object.assign(globalThis as any, {
    __mm_onOpen: onOpenScript,
    __mm_translateDatabase: translateDatabase,
});
