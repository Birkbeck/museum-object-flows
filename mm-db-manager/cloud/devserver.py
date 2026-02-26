import os
from mm_db_cloud.app import create_app

app = create_app()

if __name__ == "__main__":
    # default to 8080 to match your test config
    port = int(os.environ.get("PORT", "8080"))
    app.run(host="127.0.0.1", port=port, debug=True)
