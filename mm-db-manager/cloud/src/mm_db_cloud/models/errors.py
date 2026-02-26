from __future__ import annotations


class RequestValidationError(ValueError):
    pass


class AuthError(PermissionError):
    pass
