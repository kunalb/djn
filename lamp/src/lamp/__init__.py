import argparse
import asyncio
import json
import os
import socket
import stat
from pathlib import Path
from typing import Any, Mapping

import httpx
from jsonrpcserver import async_dispatch, method, Success

@method
async def ping():
    return Success("pong3")


@method(name="chat.completions")
async def chat_completions(**params: Any):
    response = await _openai_compatible_chat_completion(params)
    return Success(response)


def main() -> None:
    parser = argparse.ArgumentParser(prog="lamp")
    subparsers = parser.add_subparsers(dest="command")
    subparsers.add_parser("serve")
    subparsers.add_parser("ping")
    chat_parser = subparsers.add_parser("chat")
    chat_parser.add_argument("--model", required=True)
    chat_parser.add_argument("--message")
    chat_parser.add_argument("--system")
    chat_parser.add_argument("--temperature", type=float)
    chat_parser.add_argument("--max-tokens", type=int)
    args = parser.parse_args()

    if args.command == "ping":
        _run_ping()
        return
    if args.command == "chat":
        _run_chat(args)
        return

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


def _openai_base_url() -> str:
    base = os.environ.get("OPENAI_BASE_URL") or os.environ.get("OPENAI_API_BASE")
    return base or "https://api.openai.com"


async def _openai_compatible_chat_completion(
    params: Mapping[str, Any],
) -> Mapping[str, Any]:
    api_key = os.environ.get("OPENAI_API_KEY")
    if not api_key:
        raise RuntimeError("OPENAI_API_KEY is not set")

    base_url = _openai_base_url().rstrip("/")
    if base_url.endswith("/v1"):
        url = f"{base_url}/chat/completions"
    else:
        url = f"{base_url}/v1/chat/completions"

    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json",
    }

    if params.get("stream"):
        raise RuntimeError("streaming is not supported")

    async with httpx.AsyncClient(timeout=60.0) as client:
        response = await client.post(url, headers=headers, json=dict(params))
        response.raise_for_status()
        return response.json()


def _run_ping() -> None:
    socket_path = os.environ.get("LAMP_UDS", "/tmp/lamp.sock")
    request = {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "ping",
        "params": {},
    }
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as client:
        client.connect(socket_path)
        client.sendall((json.dumps(request) + "\n").encode("utf-8"))
        response = client.recv(65536)
    print(response.decode("utf-8"))


def _run_chat(args: argparse.Namespace) -> None:
    socket_path = os.environ.get("LAMP_UDS", "/tmp/lamp.sock")
    message = args.message
    if message is None:
        message = input().strip()

    messages = []
    if args.system:
        messages.append({"role": "system", "content": args.system})
    messages.append({"role": "user", "content": message})

    params: dict[str, Any] = {
        "model": args.model,
        "messages": messages,
    }
    if args.temperature is not None:
        params["temperature"] = args.temperature
    if args.max_tokens is not None:
        params["max_tokens"] = args.max_tokens

    request = {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "chat.completions",
        "params": params,
    }
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as client:
        client.connect(socket_path)
        client.sendall((json.dumps(request) + "\n").encode("utf-8"))
        response = client.recv(65536)
    print(response.decode("utf-8"))

if __name__ == "__main__":
    main()
