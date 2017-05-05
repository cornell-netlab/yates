#!/bin/bash
for topo in HiberniaGlobal Highwinds Tinet ; do
    python LatencyLines.py ../../data/rtt/${topo}.rtt ~/src/git/kulfi/expData/${topo}/paths ~/src/git/merlin-docs/papers/sigcomm2017/figures/paths_case_study/${topo}Hop.pdf hop
    python LatencyDiffPoints.py ../../data/rtt/${topo}.rtt ~/src/git/kulfi/expData/${topo}/paths ~/src/git/merlin-docs/papers/sigcomm2017/figures/paths_case_study/${topo}Hop.pdf hop
    python LatencyLines.py ../../data/rtt/${topo}.rtt ~/src/git/kulfi/expData/${topo}/paths ~/src/git/merlin-docs/papers/sigcomm2017/figures/paths_case_study/${topo}RTT.pdf rtt;
    python LatencyDiffPoints.py ../../data/rtt/${topo}.rtt ~/src/git/kulfi/expData/${topo}/paths ~/src/git/merlin-docs/papers/sigcomm2017/figures/paths_case_study/${topo}RTT.pdf rtt;
done

