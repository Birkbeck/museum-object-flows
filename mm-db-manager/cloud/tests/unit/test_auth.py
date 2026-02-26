import base64
import hashlib
import hmac

import pytest
from flask import Flask, request as flask_request

import mm_db_cloud.services.auth as auth
from mm_db_cloud.models.errors import AuthError


def _sign(secret: str, body: bytes) -> str:
    mac = hmac.new(secret.encode("utf-8"), body, hashlib.sha256).digest()
    return base64.b64encode(mac).decode("utf-8")


def test_verify_request_raises_error_when_secret_unset(monkeypatch):
    monkeypatch.setattr(auth, "_HMAC_SECRET", "")

    app = Flask(__name__)

    @app.post("/x")
    def x():
        auth.verify_request(flask_request)
        return "ok"

    client = app.test_client()
    r = client.post("/x", data=b'{"a":1}', content_type="application/json")
    assert r.status_code == 500


def test_verify_request_rejects_missing_signature(monkeypatch):
    monkeypatch.setattr(auth, "_HMAC_SECRET", "s3cr3t")
    app = Flask(__name__)

    @app.post("/x")
    def x():
        auth.verify_request(flask_request)
        return "ok"

    client = app.test_client()
    r = client.post("/x", data=b'{"a":1}', content_type="application/json")
    assert r.status_code == 500  # unhandled in this tiny app route
    # Better: call verify_request directly with a request context:
    # but we show the realistic behavior below.

    with app.test_request_context(
        "/x", method="POST", data=b"{}", content_type="application/json"
    ):
        with pytest.raises(AuthError):
            auth.verify_request(flask_request)


def test_verify_request_accepts_valid_signature(monkeypatch):
    monkeypatch.setattr(auth, "_HMAC_SECRET", "s3cr3t")
    app = Flask(__name__)
    body = b'{"spreadsheetId":"abc","ops":[]}'

    sig = _sign("s3cr3t", body)

    with app.test_request_context(
        "/x",
        method="POST",
        data=body,
        headers={"X-Signature": sig},
        content_type="application/json",
    ):
        auth.verify_request(flask_request)  # should not raise


def test_verify_request_rejects_invalid_signature(monkeypatch):
    monkeypatch.setattr(auth, "_HMAC_SECRET", "s3cr3t")
    app = Flask(__name__)
    body = b'{"spreadsheetId":"abc","ops":[]}'

    with app.test_request_context(
        "/x",
        method="POST",
        data=body,
        headers={"X-Signature": "invalid"},
        content_type="application/json",
    ):
        with pytest.raises(AuthError):
            auth.verify_request(flask_request)
