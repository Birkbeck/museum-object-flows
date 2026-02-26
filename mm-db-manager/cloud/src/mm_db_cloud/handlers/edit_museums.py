from __future__ import annotations

import logging

from flask import Blueprint, jsonify, request

from mm_db_cloud.handlers._pinned_sheet import get_pinned_spreadsheet_id
from mm_db_cloud.models.edit_museums import EditMuseumsRequest
from mm_db_cloud.services.edit_museums_service import EditMuseumsService
from mm_db_cloud.services.auth import verify_request
from mm_db_cloud.services.sheets_service import SheetsService

bp = Blueprint("edit_museums_domain", __name__)
log = logging.getLogger(__name__)


@bp.post("/editMuseums")
def edit_museums():
    auth_resp = verify_request(request)
    if auth_resp is not None:
        return auth_resp

    spreadsheet_id = get_pinned_spreadsheet_id()

    sheets = SheetsService()
    svc = EditMuseumsService(sheets)

    try:
        resp = svc.run(EditMuseumsRequest(), spreadsheet_id=spreadsheet_id)
        return jsonify(resp.to_dict()), 200
    except Exception:
        log.exception("editMuseums handler crashed")
        raise
