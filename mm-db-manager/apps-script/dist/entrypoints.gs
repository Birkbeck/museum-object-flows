/**
 * Top-level Apps Script entrypoints.
 * Delegates to implementations published by the TS bundle under __mm_* names
 * to avoid recursion with these shims.
 */

function onOpen(e) {
  if (!globalThis.__mm_onOpen) {
    throw new Error("__mm_onOpen implementation not found on globalThis");
  }
  return globalThis.__mm_onOpen(e);
}

function onEdit(e) {
  if (!globalThis.__mm_onEdit) {
    throw new Error("__mm_onEdit implementation not found on globalThis");
  }
  return globalThis.__mm_onEdit(e);
}

function addMuseums() {
  if (!globalThis.__mm_addMuseums) {
    throw new Error("__mm_addMuseums implementation not found on globalThis");
  }
  return globalThis.__mm_addMuseums();
}

function editMuseums() {
  if (!globalThis.__mm_editMuseums) {
    throw new Error("__mm_editMuseums implementation not found on globalThis");
  }
  return globalThis.__mm_editMuseums();
}

function trashMuseums() {
  if (!globalThis.__mm_trashMuseums) {
    throw new Error("__mm_trashMuseums implementation not found on globalThis");
  }
  return globalThis.__mm_trashMuseums();
}

function restoreMuseums() {
  if (!globalThis.__mm_restoreMuseums) {
    throw new Error("__mm_restoreMuseums implementation not found on globalThis");
  }
  return globalThis.__mm_restoreMuseums();
}

function permanentlyDeleteMuseums() {
  if (!globalThis.__mm_permanentlyDeleteMuseums) {
    throw new Error("__mm_permanentlyDeleteMuseums implementation not found on globalThis");
  }
  return globalThis.__mm_permanentlyDeleteMuseums();
}

function publishDatabase() {
  if (!globalThis.__mm_publishDatabase) {
    throw new Error("__mm_publishDatabase implementation not found on globalThis");
  }
  return globalThis.__mm_publishDatabase();
}
