Plot[Abs[x], {x, -4, 4}, PlotRange -> {-4, 4}, AspectRatio -> 1]

Und[x_] := If[Abs[x] < 1, 1 / x, x];
Plot[Und[x], {x, -4, 4}, PlotRange -> {-4, 4}, AspectRatio -> 1]

Usp[x_, base_] := base^Abs[Log[base, x]];
Plot[Usp[x, E], {x, -4, 4}, PlotRange -> {-4, 4}, AspectRatio -> 1]

Udp[x_, base_] := base^Und[Log[base, x]];
Plot[Udp[x, E], {x, -4, 4}, PlotRange -> {-4, 4}, AspectRatio -> 1]

Plot[Udp[x, 5], {x, -10, 10}, PlotRange -> {-10, 10}, AspectRatio -> 1]
