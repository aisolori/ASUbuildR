"""Detached supervisor: the browser never owns solver lifetime or output pipes."""
import argparse
import codecs
import datetime
import json
import os
from pathlib import Path
import subprocess
import time
import traceback


def write_timestamped(log, text):
    """UTC capture times, including milliseconds, on every nonempty line."""
    for line in text.splitlines():
        stamp = datetime.datetime.now(datetime.timezone.utc).isoformat(timespec='milliseconds')
        log.write(f'{stamp} {line}\n' if line else '\n')
    log.flush()


class TimestampedOutput:
    """Tail a regular spool file; solver output never depends on a live pipe."""
    def __init__(self, source, log):
        self.source = source
        self.log = log
        self.decoder = codecs.getincrementaldecoder('utf-8')(errors='replace')
        self.pending = ''

    def drain(self, final=False):
        # Read only the bytes present at entry so a noisy solver cannot starve
        # status heartbeats. Keep partial lines/UTF-8 characters until complete.
        remaining = max(0, os.fstat(self.source.fileno()).st_size - self.source.tell())
        while remaining:
            chunk = self.source.read(min(65536, remaining))
            if not chunk:
                break
            remaining -= len(chunk)
            self.pending += self.decoder.decode(chunk)
            lines = self.pending.split('\n')
            self.pending = lines.pop()
            if lines:
                write_timestamped(self.log, '\n'.join(line.rstrip('\r') for line in lines) + '\n')
        if final:
            self.pending += self.decoder.decode(b'', final=True)
            if self.pending:
                write_timestamped(self.log, self.pending)
            self.pending = ''
            self.decoder.reset()


def wait_with_logging(child, output, heartbeat):
    next_heartbeat = 0.0
    while True:
        output.drain()
        if child.poll() is not None:
            output.drain(final=True)
            return child.wait()
        now = time.monotonic()
        if now >= next_heartbeat:
            heartbeat()
            next_heartbeat = now + .5
        time.sleep(.1)


def atomic_json(path, value):
    pending = path.with_suffix(path.suffix + '.pending')
    pending.write_text(json.dumps(value, allow_nan=False), encoding='utf-8')
    for attempt in range(20):
        try:
            os.replace(pending, path)
            return
        except PermissionError:
            if attempt == 19:
                raise
            time.sleep(.1)


def supervise(folder):
    folder = Path(folder).resolve()
    config = json.loads((folder / 'job.json').read_text(encoding='utf-8'))
    state = dict(status='starting', supervisor_pid=os.getpid(), solver_pid=None,
                 started_at=time.time(), exit_code=None)
    child = None
    def publish(**changes):
        state.update(changes, updated_at=time.time())
        atomic_json(folder / 'status.json', state)
    # Preserve raw output even if this supervisor fails. The timestamped log is
    # the dashboard/download source; the raw spool is a diagnostic fallback.
    with (folder / 'solver.log').open('a', encoding='utf-8', buffering=1) as log, \
            (folder / 'solver.raw.log').open('ab', buffering=0) as raw, \
            (folder / 'solver.raw.log').open('rb', buffering=0) as source:
        output = TimestampedOutput(source, log)
        def event(message):
            write_timestamped(log, f'[job] {message}')
        try:
            publish()
            event('Starting detached solver; browser disconnects do not stop this job')
            env = os.environ.copy()
            env.update(PYTHONUNBUFFERED='1', OMP_NUM_THREADS='1',
                       MKL_NUM_THREADS='1', OPENBLAS_NUM_THREADS='1',
                       PYTHONIOENCODING='utf-8')
            flags = subprocess.CREATE_NO_WINDOW if os.name == 'nt' else 0
            child = subprocess.Popen(
                [config['python'], '-u', str(folder / 'runner.py')],
                cwd=folder, stdin=subprocess.DEVNULL, stdout=raw, stderr=subprocess.STDOUT,
                env=env, creationflags=flags, start_new_session=os.name != 'nt')
            publish(status='running', solver_pid=child.pid)
            event(f'Solver pid={child.pid}')
            code = wait_with_logging(child, output, publish)
            event(f'Solver exit_code={code}')
            success = code == 0 and (folder / 'out.json').exists()
            # Final/progress recovery is an independent R subprocess, not Shiny.
            publish(status='finalizing', exit_code=code)
            recovery = subprocess.Popen(
                [config['rscript'], '--vanilla', str(folder / 'recover.R'), str(folder)],
                cwd=folder, stdin=subprocess.DEVNULL, stdout=raw, stderr=subprocess.STDOUT,
                creationflags=flags)
            recovery_code = wait_with_logging(recovery, output, publish)
            if recovery_code:
                event(f'RDS export failed (exit_code={recovery_code}); JSON/input files retained')
            terminal = ('stopped' if (folder / 'stop.flag').exists() else 'completed') if success else 'failed'
            publish(status=terminal, finished_at=time.time(), recovery_exit_code=recovery_code)
            event(f'Job {terminal}; recovery_exit_code={recovery_code}')
            return 0 if success else 1
        except BaseException:
            output.drain()
            write_timestamped(log, traceback.format_exc())
            # Do not kill an already-running solver if the supervisor fails.
            publish(status='supervisor_failed', finished_at=time.time(),
                    error=traceback.format_exc())
            return 1


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('folder')
    args = parser.parse_args()
    if os.name != 'nt':
        try:
            os.setsid()
        except OSError:
            pass
    raise SystemExit(supervise(args.folder))
