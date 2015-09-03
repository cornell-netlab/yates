# agent network
export PATH=/ovs/bin:$PATH
ovs-ofctl del-flows s9 ip,nw_src=10.0.0.100
ovs-ofctl del-flows s9 ip,nw_dst=10.0.0.100
ovs-ofctl del-flows s10 ip,nw_src=10.0.0.100
ovs-ofctl del-flows s10 ip,nw_dst=10.0.0.100
ovs-ofctl del-flows s11 ip,nw_src=10.0.0.100
ovs-ofctl del-flows s11 ip,nw_dst=10.0.0.100
ovs-ofctl del-flows s12 ip,nw_src=10.0.0.100
ovs-ofctl del-flows s12 ip,nw_dst=10.0.0.100


ovs-ofctl add-flow s9 ip,nw_dst=10.0.0.9,priority=5,actions=output:1
ovs-ofctl add-flow s10 ip,nw_dst=10.0.0.10,priority=5,actions=output:13
ovs-ofctl add-flow s11 ip,nw_dst=10.0.0.11,priority=5,actions=output:25
ovs-ofctl add-flow s12 ip,nw_dst=10.0.0.12,priority=5,actions=output:37

ovs-ofctl add-flow s12 ip,nw_dst=10.0.0.100,priority=5,actions=output:48

ovs-ofctl add-flow s12 ip,nw_dst=10.0.0.1,priority=5,actions=output:46
ovs-ofctl add-flow s12 ip,nw_dst=10.0.0.2,priority=5,actions=output:46
ovs-ofctl add-flow s12 ip,nw_dst=10.0.0.3,priority=5,actions=output:46
ovs-ofctl add-flow s12 ip,nw_dst=10.0.0.4,priority=5,actions=output:46
ovs-ofctl add-flow s12 ip,nw_dst=10.0.0.5,priority=5,actions=output:47
ovs-ofctl add-flow s12 ip,nw_dst=10.0.0.6,priority=5,actions=output:47
ovs-ofctl add-flow s12 ip,nw_dst=10.0.0.7,priority=5,actions=output:47
ovs-ofctl add-flow s12 ip,nw_dst=10.0.0.8,priority=5,actions=output:47
