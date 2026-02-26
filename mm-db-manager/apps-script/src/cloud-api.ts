type CloudResponseBase = {
    ok?: boolean;
    message?: string;
    error?: string;
    details?: unknown;
};

function getCloudApiBaseUrl(): string {
    const url = PropertiesService.getScriptProperties().getProperty('API_BASE_URL');
    if (!url) throw new Error('Missing Script Property API_BASE_URL');
    return url;
}

function getCloudApiHmacSecret(): string | null {
    const props = PropertiesService.getScriptProperties();
    return props.getProperty("API_HMAC_SECRET");
}

function signPayload(payload: string, secret: string): string {
    const sig = Utilities.computeHmacSha256Signature(payload, secret);
    return Utilities.base64Encode(sig);
}

function buildUrl(endpoint: string): string {
    const base = getCloudApiBaseUrl();
    const cleanEndpoint = endpoint.replace(/^\/+/, "");
    return `${base}/${cleanEndpoint}`;
}

export function callCloudEndpoint<T extends CloudResponseBase>(
    endpoint: string,
    body: Record<string, unknown> = {}
): T {
    const url = buildUrl(endpoint);
    const payload = JSON.stringify(body ?? {});
    const headers: Record<string, string> = {};

    const secret = getCloudApiHmacSecret();
    if (secret) {
        headers["X-Signature"] = signPayload(payload, secret);
    }

    const resp = UrlFetchApp.fetch(url, {
        method: "post",
        contentType: "application/json",
        payload,
        headers,
        muteHttpExceptions: true,
    });

    const status = resp.getResponseCode();
    const text = resp.getContentText();
    let data: CloudResponseBase = {};
    if (text) {
        try {
            data = JSON.parse(text) as CloudResponseBase;
        } catch (err) {
            throw new Error(`Invalid JSON from cloud API (${status}).`);
        }
    }

    if (status >= 400) {
        const errorMsg = data.error || data.message || `HTTP ${status} from cloud API.`;
        const details = data.details ? ` Details: ${JSON.stringify(data.details)}` : "";
        throw new Error(`${errorMsg}${details}`);
    }

    return data as T;
}

export function formatCloudError(action: string, err: unknown): string {
    const message = err instanceof Error ? err.message : String(err);
    return `${action} failed. ${message}`;
}
