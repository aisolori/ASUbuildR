"""Detached supervisor: the browser never owns solver lifetime or output pipes."""
import argparse
import datetime
import json
import os
from pathlib import Path
import subprocess
import time
import traceback


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
    with (folder / 'solver.log').open('a', encoding='utf-8', buffering=1) as log:
        def event(message):
            stamp = datetime.datetime.now(datetime.timezone.utc).isoformat()
            log.write(f'{stamp} [job] {message}\n')
            log.flush()
        try:
            publish()
            event('Starting detached solver; browser disconnects do not stop this job')
            env = os.environ.copy()
            env.update(PYTHONUNBUFFERED='1', OMP_NUM_THREADS='1',
                       MKL_NUM_THREADS='1', OPENBLAS_NUM_THREADS='1')
            flags = subprocess.CREATE_NO_WINDOW if os.name == 'nt' else 0
            child = subprocess.Popen(
                [config['python'], '-u', str(folder / 'runner.py')],
                cwd=folder, stdin=subprocess.DEVNULL, stdout=log, stderr=subprocess.STDOUT,
                env=env, creationflags=flags, start_new_session=os.name != 'nt')
            publish(status='running', solver_pid=child.pid)
            event(f'Solver pid={child.pid}')
            while child.poll() is None:
                publish()
                time.sleep(.5)
            code = child.wait()
            event(f'Solver exit_code={code}')
            success = code == 0 and (folder / 'out.json').exists()
            # Final/progress recovery is an independent R subprocess, not Shiny.
            publish(status='finalizing', exit_code=code)
            recovery = subprocess.run(
                [config['rscript'], '--vanilla', str(folder / 'recover.R'), str(folder)],
                cwd=folder, stdin=subprocess.DEVNULL, stdout=log, stderr=subprocess.STDOUT,
                creationflags=flags)
            if recovery.returncode:
                event(f'RDS export failed (exit_code={recovery.returncode}); JSON/input files retained')
            terminal = ('stopped' if (folder / 'stop.flag').exists() else 'completed') if success else 'failed'
            publish(status=terminal, finished_at=time.time(), recovery_exit_code=recovery.returncode)
            event(f'Job {terminal}; recovery_exit_code={recovery.returncode}')
            return 0 if success else 1
        except BaseException:
            traceback.print_exc(file=log)
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
