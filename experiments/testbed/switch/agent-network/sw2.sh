# agent network
export PATH=/ovs/bin:$PATH
ovs-ofctl del-flows s5 ip,nw_src=10.0.0.100
ovs-ofctl del-flows s5 ip,nw_dst=10.0.0.100
ovs-ofctl del-flows s6 ip,nw_src=10.0.0.100
ovs-ofctl del-flows s6 ip,nw_dst=10.0.0.100
ovs-ofctl del-flows s7 ip,nw_src=10.0.0.100
ovs-ofctl del-flows s7 ip,nw_dst=10.0.0.100
ovs-ofctl del-flows s8 ip,nw_src=10.0.0.100
ovs-ofctl del-flows s8 ip,nw_dst=10.0.0.100


ovs-ofctl add-flow s5 ip,nw_dst=10.0.0.5,priority=5,actions=output:1
ovs-ofctl add-flow s6 ip,nw_dst=10.0.0.6,priority=5,actions=output:13
ovs-ofctl add-flow s7 ip,nw_dst=10.0.0.7,priority=5,actions=output:25
ovs-ofctl add-flow s8 ip,nw_dst=10.0.0.8,priority=5,actions=output:37

ovs-ofctl add-flow s8 ip,nw_dst=10.0.0.100,priority=5,actions=output:48

