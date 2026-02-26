type TranslateResponse = {
  status: "success" | "error";
  seconds?: number;
  rows?: number;
  message?: string;
};

function getTranslateBaseUrl(): string {
  const url = PropertiesService.getScriptProperties().getProperty(
    "TRANSLATE_API_BASE_URL"
  );
  if (!url) throw new Error("Missing Script Property TRANSLATE_API_BASE_URL");
  return url.replace(/\/+$/, ""); // trim trailing slash
}

function getTranslateToken(): string {
  const token = PropertiesService.getScriptProperties().getProperty(
    "TRANSLATE_TOKEN"
  );
  if (!token) throw new Error("Missing Script Property TRANSLATE_TOKEN");
  return token;
}

export function translateDatabase(): void {
  const ui = SpreadsheetApp.getUi();

  try {
    const choice = ui.alert(
      "Run translation?",
      "This will anonymize and regenerate the graph data.",
      ui.ButtonSet.OK_CANCEL
    );
    if (choice !== ui.Button.OK) {
      return;
    }

    const url = getTranslateBaseUrl();
    const token = getTranslateToken();

    const resp = UrlFetchApp.fetch(url, {
      method: "post",
      contentType: "application/json",
      payload: JSON.stringify({}),
      headers: { "X-Translate-Token": token },
      muteHttpExceptions: true,
    });

    const status = resp.getResponseCode();
    const text = resp.getContentText();

    Logger.log("STATUS: %s", status);
    Logger.log("RAW (first 500 chars): %s", text.slice(0, 500));

    if (status < 200 || status >= 300) {
      throw new Error(
        `Translate failed (HTTP ${status}). Body: ${text.slice(0, 300)}`
      );
    }

    let data: TranslateResponse;

    try {
      data = JSON.parse(text);

      if (data.status !== "success") {
        ui.alert(`Translate failed.\n\n${data.message ?? "Unknown error"}`);
        return;
      }
    } catch (e) {
      ui.alert(`Translate returned non-JSON. Body: ${text.slice(0, 300)}`);
      return;
    }

    ui.alert(
      `Translation complete ✅\n\nSeconds: ${(
        data.seconds ?? 0
      ).toFixed(2)}`
    );
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    ui.alert(`Translate failed. ${msg}`);
  }
}
