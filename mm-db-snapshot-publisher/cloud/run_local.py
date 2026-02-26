import os
from types import SimpleNamespace

from dotenv import load_dotenv
from main import publish


class FakeRequest:
    def __init__(self):
        self.headers = {"X-Publish-Token": os.environ.get("PUBLISH_TOKEN", "")}


if __name__ == "__main__":
    load_dotenv()
    response = publish(FakeRequest())
    print(response)
