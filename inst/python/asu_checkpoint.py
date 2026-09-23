"""Synchronously request an RDS save from the dashboard before further solving."""
import json
import os
from pathlib import Path
import time
import uuid


def request_rds_checkpoint(folder, phase, asu_id, timeout=600.0):
    stem = Path(folder) / (phase.lower() + "_" + uuid.uuid4().hex)
    request = Path(str(stem) + ".request.json")
    ack = Path(str(stem) + ".ack.json")
    pending = Path(str(request) + ".pending")
    with pending.open("w", encoding="utf-8") as stream:
        json.dump({"phase": phase, "asu_id": [int(x) for x in asu_id]}, stream)
        stream.flush()
        os.fsync(stream.fileno())
    os.replace(pending, request)
    print(f"[checkpoint] Waiting for RDS save: {stem}.rds", flush=True)
    deadline = time.monotonic() + timeout
    while not ack.exists():
        if time.monotonic() >= deadline:
            raise RuntimeError(f"RDS checkpoint timed out; assignments retained at {request}")
        time.sleep(0.1)
    response = json.loads(ack.read_text(encoding="utf-8"))
    if not response.get("ok"):
        raise RuntimeError(f"RDS checkpoint failed: {response.get('error', 'unknown error')}; assignments: {request}")
    print(f"[checkpoint] RDS saved: {response['path']}", flush=True)
