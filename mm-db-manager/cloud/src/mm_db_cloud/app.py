from __future__ import annotations

import logging
import os
import sys
import traceback
from typing import Any, Dict

from flask import Flask, jsonify, request

from mm_db_cloud.handlers.add_museums import bp as add_museums_bp
from mm_db_cloud.handlers.edit_museums import bp as edit_museums_bp
from mm_db_cloud.handlers.trash_museums import bp as trash_museums_bp
from mm_db_cloud.handlers.restore_museums import bp as restore_museums_bp
from mm_db_cloud.handlers.permanently_delete_museums import (
    bp as permanently_delete_museums_bp,
)
from mm_db_cloud.models.errors import AuthError, RequestValidationError


def _configure_logging() -> None:
    """
    Cloud Run/Functions capture stdout/stderr. Ensure our logs are emitted and include exceptions.
    """
    level_name = os.getenv("LOG_LEVEL", "INFO").upper()
    level = getattr(logging, level_name, logging.INFO)
    root = logging.getLogger()
    if not root.handlers:
        logging.basicConfig(
            level=level,
            format="%(asctime)s %(levelname)s %(name)s %(message)s",
            stream=sys.stdout,
        )
    else:
        root.setLevel(level)


def _safe_json() -> Dict[str, Any] | None:
    """
    Best-effort JSON parse for logging. Never raises.
    """
    try:
        return request.get_json(silent=True)
    except Exception:
        return None


def create_app() -> Flask:
    _configure_logging()
    log = logging.getLogger("mm_db_cloud")

    app = Flask(__name__)

    # --- Blueprints ---
    app.register_blueprint(add_museums_bp)
    app.register_blueprint(edit_museums_bp)
    app.register_blueprint(trash_museums_bp)
    app.register_blueprint(restore_museums_bp)
    app.register_blueprint(permanently_delete_museums_bp)

    @app.before_request
    def _log_request() -> None:
        # Don't log raw bodies by default (could include secrets). Log shape instead.
        log.info(
            "request start method=%s path=%s content_type=%s content_length=%s",
            request.method,
            request.path,
            request.content_type,
            request.content_length,
        )

    @app.after_request
    def _log_response(resp):
        log.info(
            "request end method=%s path=%s status=%s",
            request.method,
            request.path,
            resp.status_code,
        )
        return resp

    @app.errorhandler(AuthError)
    def handle_auth_error(e: AuthError):
        log.warning(
            "auth error method=%s path=%s error=%s",
            request.method,
            request.path,
            str(e),
        )
        return jsonify({"ok": False, "error": str(e)}), 403

    @app.errorhandler(RequestValidationError)
    def handle_validation_error(e: RequestValidationError):
        payload = _safe_json()
        log.warning(
            "validation error method=%s path=%s error=%s payload=%s",
            request.method,
            request.path,
            str(e),
            payload,
        )
        return jsonify({"ok": False, "error": str(e)}), 400

    @app.errorhandler(Exception)
    def handle_unexpected_error(e: Exception):
        log.exception(
            "unhandled error method=%s path=%s payload=%s",
            request.method,
            request.path,
            _safe_json(),
        )

        return jsonify({"ok": False, "error": "Internal server error"}), 500

    return app
