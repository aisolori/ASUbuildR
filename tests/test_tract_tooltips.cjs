const assert = require('node:assert/strict');
const { test } = require('node:test');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');

const dashboard = fs.readFileSync(
  path.join(__dirname, '../inst/shiny_app/ASU_Flexdashboard_mapgl.Rmd'), 'utf8');
const script = dashboard.match(/<script>([\s\S]*?)<\/script>/)[1];
const renderHook = dashboard.match(/htmlwidgets::onRender\(map, "([\s\S]*?)"\)/)[1];

function fixture() {
  const handlers = {}, maps = {}, intervals = new Map(), timeouts = [], popups = [];
  const shownPages = [];
  let nextTimer = 1;
  class Popup {
    constructor() { popups.push(this); }
    setLngLat(value) { this.location = value; return this; }
    setHTML(value) { this.html = value; return this; }
    addTo(map) { this.map = map; return this; }
    remove() { this.map = null; }
  }
  const window = {
    Shiny: { addCustomMessageHandler: (name, handler) => {
      // Match Shiny's real registration contract, including callback arity.
      assert.equal(typeof handler, 'function', 'handler must be a function');
      assert.equal(handler.length, 1, 'handler must be a function that takes one argument');
      handlers[name] = handler;
    } },
    FlexDashboardUtils: { showPage: page => shownPages.push(page) },
    HTMLWidgets: { find: selector => maps[selector.slice(1)]
      ? { getMap: () => maps[selector.slice(1)] } : null },
    maplibregl: { Popup },
    setInterval: callback => { const id = nextTimer++; intervals.set(id, callback); return id; },
    clearInterval: id => intervals.delete(id),
    setTimeout: callback => { timeouts.push(callback); }
  };
  const context = vm.createContext({ window });
  vm.runInContext(script, context);
  const hook = vm.runInContext('(' + renderHook + ')', context);
  function newMap(id) {
    const listeners = {}, canvas = { style: {} };
    const map = {
      ready: false, layerReady: false, tilesLoaded: true, listeners,
      getStyle() { return this.ready ? { version: 8 } : undefined; },
      isStyleLoaded() { return this.ready && this.tilesLoaded; },
      getLayer() {
        // MapLibre dereferences an absent style before it is ready.
        assert.ok(this.ready, 'queried layers before style readiness');
        return this.layerReady ? { id: 'basemap' } : null;
      },
      getCanvas: () => canvas,
      on(name, layer, listener) {
        assert.equal(layer, 'basemap');
        assert.equal(listeners[name], undefined, 'installed duplicate hover listeners');
        listeners[name] = listener;
      }
    };
    maps[id] = map;
    return map;
  }
  return {
    window, maps, intervals, timeouts, popups, newMap, shownPages,
    showInitial: () => handlers['asu-show-initial-asu']({}),
    render: id => hook({ id }),
    update: (id, asunum) => handlers['asu-tooltip-state']({
      id, updates: [{ geoid: '32001000100', asunum }]
    }),
    tick: () => [...intervals.values()].forEach(callback => callback())
  };
}

const feature = {
  lngLat: { lng: -119, lat: 39 },
  features: [{ properties: {
    GEOID: '32001000100', asunum: 0, tract_pop_cur: 1000,
    tract_ASU_clf: 500, tract_ASU_urate: 6.5, tract_ASU_unemp: 32
  } }]
};

test('Shiny registration completes and the initial-ASU navigation handler works', () => {
  const f = fixture();
  assert.equal(f.window._asuTooltipStateHandlerInstalled, true);
  assert.equal(typeof f.window._asuEnsureTooltip, 'function');
  f.showInitial();
  assert.deepEqual(f.shownPages, ['#section-load-initial-asu']);
});

for (const id of ['initial_map', 'edit_map']) {
  test(`${id}: visible tracts get tooltips while other source tiles are pending`, () => {
    const f = fixture(), map = f.newMap(id);
    map.ready = map.layerReady = true;
    map.tilesLoaded = false;
    assert.equal(map.isStyleLoaded(), false);
    f.render(id);
    assert.equal(f.popups.length, 1, 'pending tiles must not block tooltip installation');
    assert.equal(f.intervals.size, 0);
    map.listeners.mousemove(feature);
    assert.equal(f.popups[0].map, map);
    assert.ok(f.popups[0].html.includes('32001000100'));
  });

  test(`${id}: early updates retry until style and tract layer are ready`, () => {
    const f = fixture(), map = f.newMap(id);
    assert.doesNotThrow(() => f.update(id, 7));
    assert.equal(f.intervals.size, 1);
    f.tick();
    assert.equal(f.popups.length, 0);
    map.ready = true;
    f.tick();
    assert.equal(f.popups.length, 0);
    map.layerReady = true;
    f.tick();
    assert.equal(f.intervals.size, 0);
    map.listeners.mousemove(feature);
    const popup = f.popups[0];
    assert.equal(popup.map, map);
    for (const value of ['ASU Number: </strong>7', '32001000100',
                        '1,000', '500', '6.5%', '32']) {
      assert.ok(popup.html.includes(value), `missing tract information: ${value}`);
    }
    f.update(id, 9);
    map.listeners.mousemove(feature);
    assert.ok(popup.html.includes('ASU Number: </strong>9'));
    map.listeners.mouseleave();
    assert.equal(popup.map, null);
    assert.equal(map.getCanvas().style.cursor, '');
  });

  test(`${id}: render hook installs hover on a replacement map without assignment updates`, () => {
    const f = fixture(), oldMap = f.newMap(id);
    oldMap.ready = oldMap.layerReady = true;
    f.update(id, 7);
    assert.equal(f.popups.length, 1);
    const replacement = f.newMap(id);
    f.render(id);
    replacement.ready = replacement.layerReady = true;
    f.tick();
    replacement.listeners.mousemove(feature);
    assert.equal(f.popups.length, 2);
    assert.equal(f.popups[1].map, replacement);
    assert.ok(f.popups[1].html.includes('ASU Number: </strong>7'));
    f.render(id);
    assert.equal(f.popups.length, 2);
    assert.equal(f.intervals.size, 0);
  });
}

test('render hook waits for script registration and works without an assignment message', () => {
  const f = fixture(), map = f.newMap('initial_map');
  const ensure = f.window._asuEnsureTooltip;
  delete f.window._asuEnsureTooltip;
  f.render('initial_map');
  assert.equal(f.timeouts.length, 1);
  f.window._asuEnsureTooltip = ensure;
  map.ready = map.layerReady = true;
  f.timeouts.shift()();
  map.listeners.mousemove(feature);
  assert.ok(f.popups[0].html.includes('ASU Number: </strong>0'));
});

test('late Popup dependency remains retryable and successful install cancels pending timer', () => {
  const f = fixture(), map = f.newMap('initial_map');
  map.ready = map.layerReady = true;
  const library = f.window.maplibregl;
  delete f.window.maplibregl;
  assert.doesNotThrow(() => f.update('initial_map', 7));
  assert.equal(map._asuDynamicTooltipInstalled, undefined);
  assert.equal(f.intervals.size, 1);
  f.window.maplibregl = library;
  f.render('initial_map');
  assert.equal(f.intervals.size, 0);
  assert.equal(f.popups.length, 1);
});
