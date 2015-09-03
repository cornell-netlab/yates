# agent network
export PATH=/ovs/bin:$PATH
ovs-ofctl del-flows s1 ip,nw_src=10.0.0.100
ovs-ofctl del-flows s1 ip,nw_dst=10.0.0.100
ovs-ofctl del-flows s2 ip,nw_src=10.0.0.100
ovs-ofctl del-flows s2 ip,nw_dst=10.0.0.100
ovs-ofctl del-flows s3 ip,nw_src=10.0.0.100
ovs-ofctl del-flows s3 ip,nw_dst=10.0.0.100
ovs-ofctl del-flows s4 ip,nw_src=10.0.0.100
ovs-ofctl del-flows s4 ip,nw_dst=10.0.0.100


ovs-ofctl add-flow s1 ip,nw_dst=10.0.0.1,priority=5,actions=output:1
ovs-ofctl add-flow s2 ip,nw_dst=10.0.0.2,priority=5,actions=output:13
ovs-ofctl add-flow s3 ip,nw_dst=10.0.0.3,priority=5,actions=output:25
ovs-ofctl add-flow s4 ip,nw_dst=10.0.0.4,priority=5,actions=output:37

ovs-ofctl add-flow s4 ip,nw_dst=10.0.0.100,priority=5,actions=output:48

