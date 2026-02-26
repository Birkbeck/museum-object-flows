import os
from pathlib import Path

from dotenv import load_dotenv


def pytest_sessionstart(session):
    # Load cloud/.env if present (local dev), but don't fail in CI
    root = Path(__file__).resolve().parents[1]  # cloud/
    env_path = root / ".env"
    if env_path.exists():
        load_dotenv(env_path)
