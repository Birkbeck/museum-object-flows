type PublishResponse = {
    status: "success" | "error";
    seconds?: number;
    rows?: number;
    message?: string;
};

function getPublishBaseUrl(): string {
    const url = PropertiesService.getScriptProperties().getProperty("PUBLISH_API_BASE_URL");
    if (!url) throw new Error("Missing Script Property PUBLISH_API_BASE_URL");
    return url.replace(/\/+$/, ""); // trim trailing slash
}

function getPublishToken(): string {
    const token = PropertiesService.getScriptProperties().getProperty("PUBLISH_TOKEN");
    if (!token) throw new Error("Missing Script Property PUBLISH_TOKEN");
    return token;
}

export function publishDatabase(): void {
    const ui = SpreadsheetApp.getUi();
    try {
	const choice = ui.alert(
	    "Publish database snapshot?",
	    "This will update the public-facing data.",
	    ui.ButtonSet.OK_CANCEL
	);
	if (choice !== ui.Button.OK) {
	    return;
	}
	const url = getPublishBaseUrl();
	const token = getPublishToken();
	const resp = UrlFetchApp.fetch(url, {
	    method: "post",
	    contentType: "application/json",
	    payload: JSON.stringify({}),
	    headers: { "X-Publish-Token": token },
	    muteHttpExceptions: true,
	});
	const status = resp.getResponseCode();
	Logger.log("STATUS: %s", status);
	const text = resp.getContentText();
	Logger.log("RAW (first 500 chars): %s", text.slice(0, 500));
	if (status < 200 || status >= 300) {
	    throw new Error(`Publish failed (HTTP ${status}). Body: ${text.slice(0, 300)}`);
	}
	let data;
	try {
	    data = JSON.parse(text);
	    if (status >= 400 || data.status !== "success") {
		ui.alert(`Publish failed.\n\n${data.message ?? `HTTP ${status}`}`);
		return;
	    }
	} catch (e) {
	    ui.alert(`Publish returned non-JSON. Body: ${text.slice(0, 300)}`);
	}
	ui.alert(
	    `Publish complete ✅\n\nRows: ${data.rows ?? "?"}\nSeconds: ${(
data.seconds ?? 0
).toFixed(2)}`
	);
    } catch (err) {
	const msg = err instanceof Error ? err.message : String(err);
	ui.alert(`Publish failed. ${msg}`);
    }
}
