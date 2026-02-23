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

function translateDatabase() {
  if (!globalThis.__mm_translateDatabase) {
    throw new Error("__mm_translateDatabase implementation not found on globalThis");
  }
  return globalThis.__mm_translateDatabase();
}
