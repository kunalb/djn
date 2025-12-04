import uvicorn

from fastapi import FastAPI, Request, Response
from jsonrpcserver import async_dispatch, method, Success

app = FastAPI()


@method
async def ping():
    return Success("pong")


@app.post("/")
async def index(request: Request) -> Response:
    return Response(await async_dispatch(await request.body()))


def main() -> None:
    uvicorn.run(app, port=5000)

if __name__ == "__main__":
    main()
