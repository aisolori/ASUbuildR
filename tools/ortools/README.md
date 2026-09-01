# Tract-first OR-Tools wheel

ASUbuildR's tract-first continuous-probing experiment requires a custom
OR-Tools 9.15 wheel. Stock OR-Tools does not expose variable-order control for
`ContinuousProber`.

The patch adds `SatParameters.continuous_probing_order`. Its default remains
integer-first, so unmodified workers preserve upstream behavior. ASUbuildR uses
Boolean-first order for one fast and one standard probe worker while retaining
one integer-first worker of each type.

## Build

Run the **Build tract-first OR-Tools wheel** workflow manually in GitHub
Actions. It checks out `google/or-tools` at `v9.15`, applies
`continuous-probing-order-v9.15.patch`, builds with Python 3.11 and Visual
Studio 2022, runs a solver smoke test, and uploads this artifact:

```text
ortools-9.15-tract-first-windows-py311
```

## Install

Download and extract the artifact, then install its wheel into the ASUbuildR
Python environment:

```powershell
C:\Users\aisolorio\AppData\Local\r-miniconda\envs\asu-cpsat\python.exe `
  -m pip install --force-reinstall C:\path\to\ortools-9.15-*.whl
```

Confirm that the custom field is present:

```powershell
C:\Users\aisolorio\AppData\Local\r-miniconda\envs\asu-cpsat\python.exe -c `
  "from ortools.sat.python import cp_model; p=cp_model.CpSolver().parameters; print('continuous_probing_order' in p.DESCRIPTOR.fields_by_name)"
```

With `use_tract_first_search=True`, the solver log should then report:

```text
tract-first probing: one fast and one standard worker probe tract Booleans before flow bounds
```

A stock wheel remains supported and logs that tract-first probing is
unavailable while retaining all integer-first probe workers.
