from __future__ import annotations

import base64
import hashlib
import hmac
import os

from flask import Request

from mm_db_cloud.models.errors import AuthError


_HMAC_SECRET = os.environ.get("HMAC_SECRET", "")


def verify_request(request: Request) -> None:
    """
    Require header:
      X-Signature: base64(hmac_sha256(secret, raw_body))
    """
    if not _HMAC_SECRET:
        raise RuntimeError("HMAC_SECRET environment variable is not set")

    sig = request.headers.get("X-Signature", "")
    if not sig:
        raise AuthError("Missing X-Signature header")

    raw = request.get_data() or b""
    mac = hmac.new(_HMAC_SECRET.encode("utf-8"), raw, hashlib.sha256).digest()
    expected = base64.b64encode(mac).decode("utf-8")

    if not hmac.compare_digest(expected, sig):
        raise AuthError("Invalid signature")
