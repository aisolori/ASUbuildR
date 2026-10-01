const assert = require('node:assert/strict');
const { test } = require('node:test');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const source = fs.readFileSync(path.join(__dirname, '../inst/shiny_app/save-as.js'), 'utf8');

function fixture({ native = true, cancel = false, ok = true, writeError = false,
                   stream = false, filename = 'saved_data.rds' } = {}) {
  const calls = [], notices = [], bytes = Buffer.from([0, 31, 139, 255, 65]);
  const attrs = { href: '/session/test/download/save_data' };
  const link = { dataset: { asuSaveName: filename },
    classList: { contains: () => false },
    getAttribute: name => attrs[name],
    setAttribute: (name, value) => { attrs[name] = value; },
    removeAttribute: name => { delete attrs[name]; }
  };
  let written;
  const writable = {
    write: async value => { if (writeError) throw new Error('Disk full'); written = value; },
    close: async () => calls.push('closed'), abort: async () => calls.push('aborted')
  };
  const window = {
    Shiny: { setInputValue: (id, value) => notices.push(value) },
    fetch: async (href, options) => {
      calls.push('fetch');
      assert.equal(href, attrs.href);
      assert.equal(options.credentials, 'same-origin');
      return { ok, headers: { get: () => 'application/octet-stream' },
        body: stream ? { pipeTo: async target => { await target.write(bytes); await target.close(); } } : null,
        blob: async () => bytes };
    }
  };
  if (native) window.showSaveFilePicker = async options => {
    calls.push('picker');
    assert.equal(options.suggestedName, filename);
    assert.equal(Object.values(options.types[0].accept)[0][0], path.extname(filename));
    if (cancel) throw Object.assign(new Error('Cancelled'), { name: 'AbortError' });
    return { name: filename, createWritable: async () => { calls.push('writable'); return writable; } };
  };
  let listener;
  vm.runInNewContext(source, { window, document: { addEventListener: (name, fn, capture) => {
    assert.equal(name, 'click'); assert.equal(capture, true); listener = fn;
  } } });
  const event = { target: { closest: () => link }, button: 0,
    preventDefault: () => calls.push('prevent'), stopImmediatePropagation: () => calls.push('stop') };
  return { calls, notices, attrs, event, listener, bytes, written: () => written };
}

for (const stream of [false, true]) test(`RDS preserves binary bytes; streaming=${stream}`, async () => {
  const f = fixture({ stream });
  await f.listener(f.event);
  assert.deepEqual(f.calls, ['prevent', 'stop', 'picker', 'fetch', 'writable', 'closed']);
  assert.deepEqual(f.written(), f.bytes);
  assert.equal(f.attrs['aria-busy'], undefined);
  assert.match(f.notices[0].message, /Saved saved_data.rds/);
});
test('Log saves with its own filename and filter', async () => {
  const f = fixture({ filename: 'asu_solver.log' });
  await f.listener(f.event);
  assert.match(f.notices[0].message, /Saved asu_solver.log/);
});
test('Cancellation does not fetch or write a file', async () => {
  const f = fixture({ cancel: true });
  await f.listener(f.event);
  assert.deepEqual(f.calls, ['prevent', 'stop', 'picker']);
  assert.deepEqual(f.notices, []);
  assert.equal(f.attrs['aria-busy'], undefined);
});
test('Unsupported browsers keep the normal Shiny download', async () => {
  const f = fixture({ native: false });
  await f.listener(f.event);
  assert.deepEqual(f.calls, []);
  assert.match(f.notices[0].message, /Ask where to save/);
});
test('Failed download is not written or reported as saved', async () => {
  const f = fixture({ ok: false });
  await f.listener(f.event);
  assert.equal(f.written(), undefined);
  assert.equal(f.notices[0].type, 'error');
  assert.equal(f.attrs['aria-busy'], undefined);
});
test('Failed write aborts the stream and reports an error', async () => {
  const f = fixture({ writeError: true });
  await f.listener(f.event);
  assert.ok(f.calls.includes('aborted'));
  assert.equal(f.notices[0].type, 'error');
});
test('Repeated click during saving does not open another dialog', async () => {
  const f = fixture();
  f.attrs['aria-busy'] = 'true';
  await f.listener(f.event);
  assert.deepEqual(f.calls, ['prevent', 'stop']);
});
