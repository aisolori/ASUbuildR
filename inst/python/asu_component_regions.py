"""Valid regional strengthening for unlabelled, component-first ASU search.

For any tract region R, if none of its outside neighbors is selected, every
selected connected component touching R lies wholly inside R. Thus its selected
tracts are a union of qualifying ASUs: their aggregate rate surplus is
nonnegative, and a nonempty selection meets the population and labor minima.
The region need not itself be connected or qualifying. These are necessary,
not sufficient, conditions and never freeze a heuristic tract assignment.
"""
from __future__ import annotations


def iter_regions(graph, nodes, max_nodes=512, rings=2):
    """Yield the seed and bounded whole-ring expansions as sorted tuples.

    At most ``rings + 1`` regions are returned. Oversized seeds are skipped;
    expansion stops before exceeding ``max_nodes`` rather than truncating a
    ring. Node and ring caps bound cut generation only, not the global search.
    """
    region = set(nodes)
    if not region or len(region) > max_nodes:
        return
    yield tuple(sorted(region))
    frontier = region.copy()
    for _ in range(max(0, int(rings))):
        following = set()
        for node in frontier:
            for neighbor in graph[node]:
                if neighbor not in region:
                    following.add(neighbor)
                    if len(region) + len(following) > max_nodes:
                        return
        if not following:
            return
        region.update(following)
        frontier = following
        yield tuple(sorted(region))


def add_region_constraints(model, x, graph, u, emp, pop, surplus, tau,
                           pop_thresh, region, *, name):
    """Add safe population/rate/labor cuts for an arbitrary tract region.

    Inputs use the caller's already validated integer counts and exact rate
    surplus coefficients. ``name`` must uniquely identify this region in the
    calling model. Return the number of constraints added (including Boolean
    definitions), or zero for an empty region.
    """
    members = set(region)
    if not members:
        return 0
    nodes = sorted(members)
    boundary = sorted({neighbor for node in nodes for neighbor in graph[node]
                       if neighbor not in members})
    count = 0
    boundary_active = None
    if boundary:
        boundary_active = model.NewBoolVar(f"{name}_boundary")
        model.AddMaxEquality(boundary_active, [x[node] for node in boundary])
        count += 1

    active = model.NewBoolVar(f"{name}_active")
    model.AddMaxEquality(active, [x[node] for node in nodes])
    count += 1

    def isolated(constraint):
        nonlocal count
        if boundary_active is not None:
            constraint.OnlyEnforceIf(boundary_active.Not())
        count += 1

    # If even the entire region cannot meet a necessary size/labor condition,
    # no nonempty isolated subset can qualify. This stronger special case also
    # avoids unnecessarily materializing weighted expressions for tiny regions.
    if sum(pop[node] for node in nodes) < pop_thresh or (
            tau > 0 and not any(u[node] + emp[node] for node in nodes)):
        isolated(model.Add(active == 0))
        return count

    isolated(model.Add(sum(surplus[node] * x[node] for node in nodes) >= 0))
    if pop_thresh > 0:
        isolated(model.Add(sum(pop[node] * x[node] for node in nodes)
                           >= int(pop_thresh) * active))
    if tau > 0:
        isolated(model.Add(sum((u[node] + emp[node]) * x[node] for node in nodes)
                           >= active))
    return count
