import functions_framework
from werkzeug.wrappers import Response

from mm_db_cloud.app import create_app

_flask_app = create_app()


@functions_framework.http
def app(request):
    return Response.from_app(_flask_app, request.environ)
