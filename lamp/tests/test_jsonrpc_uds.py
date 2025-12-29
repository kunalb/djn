import asyncio
import json
from contextlib import suppress
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import Any

import httpx
from jsonrpcserver import async_dispatch

from lamp import _handle_client, _response_to_bytes


def test_uds_server_handles_request() -> None:
    async def run_test() -> None:
        with TemporaryDirectory() as tmpdir:
            socket_path = Path(tmpdir) / "lamp.sock"
            server = await asyncio.start_unix_server(
                _handle_client,
                path=str(socket_path),
            )

            serve_task = asyncio.create_task(server.serve_forever())
            try:
                reader, writer = await asyncio.open_unix_connection(
                    str(socket_path)
                )
                request = {
                    "jsonrpc": "2.0",
                    "id": 1,
                    "method": "ping",
                }
                writer.write((json.dumps(request) + "\n").encode("utf-8"))
                await writer.drain()

                response_line = await reader.readline()
                response = json.loads(response_line.decode("utf-8"))

                assert response["id"] == 1
                assert response["result"] == "pong3"

                writer.close()
                await writer.wait_closed()
            finally:
                serve_task.cancel()
                with suppress(asyncio.CancelledError):
                    await serve_task
                server.close()
                await server.wait_closed()

    asyncio.run(run_test())


def test_chat_completions_forwarding(monkeypatch: Any) -> None:
    class FakeResponse:
        def raise_for_status(self) -> None:
            return None

        def json(self) -> dict[str, Any]:
            return {"id": "resp", "choices": [{"message": {"content": "hi"}}]}

    class FakeClient:
        def __init__(self, *args: Any, **kwargs: Any) -> None:
            self.calls: list[dict[str, Any]] = []

        async def __aenter__(self) -> "FakeClient":
            return self

        async def __aexit__(self, exc_type, exc, tb) -> None:
            return None

        async def post(self, url: str, headers: dict[str, str], json: dict[str, Any]):
            self.calls.append({"url": url, "headers": headers, "json": json})
            return FakeResponse()

    fake_client = FakeClient()
    monkeypatch.setattr(httpx, "AsyncClient", lambda *args, **kwargs: fake_client)
    monkeypatch.setenv("OPENAI_API_KEY", "test-key")
    monkeypatch.setenv("OPENAI_BASE_URL", "https://example.com")

    async def run_test() -> None:
        request = {
            "jsonrpc": "2.0",
            "id": 5,
            "method": "chat.completions",
            "params": {
                "model": "gpt-test",
                "messages": [{"role": "user", "content": "hello"}],
            },
        }
        response = await async_dispatch(json.dumps(request))
        payload = json.loads(_response_to_bytes(response).decode("utf-8"))

        assert payload["id"] == 5
        assert payload["result"]["choices"][0]["message"]["content"] == "hi"
        assert fake_client.calls
        assert fake_client.calls[0]["url"].endswith("/v1/chat/completions")
        assert fake_client.calls[0]["json"]["model"] == "gpt-test"

    asyncio.run(run_test())
