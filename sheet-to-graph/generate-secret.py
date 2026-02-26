import secrets

with open(".secret.txt", "w") as f:
    f.write(secrets.token_urlsafe(32))
