/**
 * Run a callback under the document lock.
 * Use this around any logic that must be atomic (ID generation + writes, etc.).
 */
export function withDocumentLock<T>(fn: () => T): T {
    const lock = LockService.getDocumentLock();
    lock.waitLock(30_000);
    try {
	return fn();
    } finally {
	lock.releaseLock();
    }
}
