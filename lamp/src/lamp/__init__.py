import asyncio
import os
import stat
from pathlib import Path
from typing import Any

from jsonrpcserver import async_dispatch, method, Success

@method
async def ping():
    return Success("pong3")


def main() -> None:
    socket_path = Path(os.environ.get("LAMP_UDS", "/tmp/lamp.sock"))
    socket_path.parent.mkdir(parents=True, exist_ok=True)
    _remove_stale_socket(socket_path)
    asyncio.run(_serve_jsonrpc_uds(socket_path))


async def _serve_jsonrpc_uds(socket_path: Path) -> None:
    server = await asyncio.start_unix_server(_handle_client, path=str(socket_path))
    async with server:
        await server.serve_forever()


def _remove_stale_socket(socket_path: Path) -> None:
    try:
        socket_stat = socket_path.stat()
    except FileNotFoundError:
        return

    if stat.S_ISSOCK(socket_stat.st_mode):
        socket_path.unlink()
        return

    raise RuntimeError(
        f"Refusing to replace non-socket path at {socket_path}"
    )


async def _handle_client(
    reader: asyncio.StreamReader,
    writer: asyncio.StreamWriter,
) -> None:
    try:
        while not reader.at_eof():
            line = await reader.readline()
            if not line:
                break
            payload = line.strip()
            if not payload:
                continue
            response = await async_dispatch(payload.decode("utf-8"))
            if response is None:
                continue
            data = _response_to_bytes(response)
            writer.write(data + b"\n")
            await writer.drain()
    finally:
        writer.close()
        await writer.wait_closed()


def _response_to_bytes(response: Any) -> bytes:
    if hasattr(response, "serialize"):
        return response.serialize().encode("utf-8")
    if hasattr(response, "json"):
        return response.json.encode("utf-8")
    return str(response).encode("utf-8")

if __name__ == "__main__":
    main()
