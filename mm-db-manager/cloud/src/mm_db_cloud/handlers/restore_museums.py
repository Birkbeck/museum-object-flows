from __future__ import annotations

import logging

from flask import Blueprint, jsonify, request

from mm_db_cloud.handlers._pinned_sheet import get_pinned_spreadsheet_id
from mm_db_cloud.models.restore_museums import RestoreMuseumsRequest
from mm_db_cloud.services.restore_museums_service import RestoreMuseumsService
from mm_db_cloud.services.auth import verify_request
from mm_db_cloud.services.sheets_service import SheetsService

bp = Blueprint("restore_museums_domain", __name__)
log = logging.getLogger(__name__)


@bp.post("/restoreMuseums")
def restore_museums():
    auth_resp = verify_request(request)
    if auth_resp is not None:
        return auth_resp

    spreadsheet_id = get_pinned_spreadsheet_id()

    sheets = SheetsService()
    svc = RestoreMuseumsService(sheets)

    try:
        resp = svc.run(RestoreMuseumsRequest(), spreadsheet_id=spreadsheet_id)
        return jsonify(resp.to_dict()), 200
    except Exception:
        log.exception("restoreMuseums handler crashed")
        raise
