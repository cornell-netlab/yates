python Timeline.py ~/bc-results/v3_rtt_budget_time-1480560014-bc_scale_1_budget_4_numtm_168
python Timeline.py ~/bc-results/v3_rtt_robust_time-1480560014-bc_scale_1_budget_4_-robust_-lr-delay_0_-fail-num_1_
python Timeline.py ~/bc-results/v3_rtt_robust_time-1480560014-bc_scale_1_budget_4_-robust_-fail-num_1_
python TimeVsIterations.py ~/bc-results/v3_rtt_budget_time-1480560014-bc_scale_1_budget_4_numtm_168
python ChurnVsIterations.py ~/bc-results/v3_rtt_budget_time-1480560014-bc_scale_1_budget_4_numtm_168 TM
python BClatency.py ~/bc-results/v3_rtt_budget_time-1480560014-bc_scale_1_budget_4_numtm_168/LatencyDistributionVsIterations.dat

cp ~/bc-results/v3_rtt_budget_time-1480560014-bc_scale_1_budget_4_numtm_168/timeline.pdf ~/src/git/merlin-docs/papers/sigcomm2017/figures/bc/b4s1base.pdf
cp ~/bc-results/v3_rtt_budget_time-1480560014-bc_scale_1_budget_4_numtm_168/metriclegend.pdf ~/src/git/merlin-docs/papers/sigcomm2017/figures/bc/metriclegend.pdf
cp ~/bc-results/v3_rtt_robust_time-1480560014-bc_scale_1_budget_4_-robust_-fail-num_1_/timeline.pdf ~/src/git/merlin-docs/papers/sigcomm2017/figures/bc/b4s1f1.pdf
cp ~/bc-results/v3_rtt_robust_time-1480560014-bc_scale_1_budget_4_-robust_-lr-delay_0_-fail-num_1_/timeline.pdf ~/src/git/merlin-docs/papers/sigcomm2017/figures/bc/b4s1f1lr.pdf
cp ~/bc-results/v3_rtt_budget_time-1480560014-bc_scale_1_budget_4_numtm_168/TMChurnVsIterations.pdf ~/src/git/merlin-docs/papers/sigcomm2017/figures/bc/
cp ~/bc-results/v3_rtt_budget_time-1480560014-bc_scale_1_budget_4_numtm_168/TimeVsIterations.pdf ~/src/git/merlin-docs/papers/sigcomm2017/figures/bc/
cp ~/bc-results/v3_rtt_budget_time-1480560014-bc_scale_1_budget_4_numtm_168/LatencyCDF.pdf ~/src/git/merlin-docs/papers/sigcomm2017/figures/bc/

echo "Congestion CCDF..."
python BCcongestionCDF.py no ~/bc-results/v3_rtt_budget_time-1480560014-bc_scale_1_budget_4_numtm_168/EdgeCongestionVsIterations.dat CongestionCCDF
python BCcongestionCDF.py no ~/bc-results/v3_rtt_budget_time-1480560014-bc_scale_1_budget_4_numtm_168/EdgeExpCongestionVsIterations.dat ExpCongestionCCDF
cp ~/bc-results/v3_rtt_budget_time-1480560014-bc_scale_1_budget_4_numtm_168/CongestionCCDF.pdf ~/src/git/merlin-docs/papers/sigcomm2017/figures/bc/
cp ~/bc-results/v3_rtt_budget_time-1480560014-bc_scale_1_budget_4_numtm_168/ExpCongestionCCDF.pdf ~/src/git/merlin-docs/papers/sigcomm2017/figures/bc/
