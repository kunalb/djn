import asyncio
import json
from contextlib import suppress
from pathlib import Path
from tempfile import TemporaryDirectory

from lamp import _handle_client


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
