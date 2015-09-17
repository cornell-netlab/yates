## s1
export PATH=/ovs/bin:$PATH
ovs-vsctl del-br s1
ovs-vsctl add-br s1 -- set bridge s1 datapath_type=pica8 protocols=OpenFlow10 -- set bridge s1 other-config:datapath-id=0000000000000001
ovs-vsctl add-port s1 "ge-1/1/1" vlan_mode=trunk -- set interface "ge-1/1/1" type="pica8" options:link_speed=990M
ovs-vsctl add-port s1 "ge-1/1/2" vlan_mode=trunk -- set interface "ge-1/1/2" type="pica8" options:link_speed=990M
ovs-vsctl add-port s1 "ge-1/1/3" vlan_mode=trunk -- set interface "ge-1/1/3" type="pica8" options:link_speed=990M
ovs-vsctl add-port s1 "ge-1/1/4" vlan_mode=trunk -- set interface "ge-1/1/4" type="pica8" options:link_speed=990M
ovs-vsctl add-port s1 "ge-1/1/5" vlan_mode=trunk -- set interface "ge-1/1/5" type="pica8" options:link_speed=990M
ovs-vsctl add-port s1 "ge-1/1/6" vlan_mode=trunk -- set interface "ge-1/1/6" type="pica8" options:link_speed=990M
ovs-vsctl add-port s1 "ge-1/1/7" vlan_mode=trunk -- set interface "ge-1/1/7" type="pica8" options:link_speed=990M
ovs-vsctl add-port s1 "ge-1/1/8" vlan_mode=trunk -- set interface "ge-1/1/8" type="pica8" options:link_speed=990M
ovs-vsctl add-port s1 "ge-1/1/9" vlan_mode=trunk -- set interface "ge-1/1/9" type="pica8" options:link_speed=990M
ovs-vsctl add-port s1 "ge-1/1/10" vlan_mode=trunk -- set interface "ge-1/1/10" type="pica8" options:link_speed=990M
ovs-vsctl add-port s1 "ge-1/1/11" vlan_mode=trunk -- set interface "ge-1/1/11" type="pica8" options:link_speed=990M
ovs-vsctl add-port s1 "ge-1/1/12" vlan_mode=trunk -- set interface "ge-1/1/12" type="pica8" options:link_speed=990M

ovs-vsctl set-controller s1 tcp:192.168.1.10:6633
ovs-vsctl set-fail-mode s1 secure
echo "Switch 1 configured"
sleep 2


## s2
ovs-vsctl del-br s2
ovs-vsctl add-br s2 -- set bridge s2 datapath_type=pica8 protocols=OpenFlow10 -- set bridge s2 other-config:datapath-id=0000000000000002
ovs-vsctl add-port s2 "ge-1/1/13" vlan_mode=trunk -- set interface "ge-1/1/13" type="pica8" options:link_speed=990M
ovs-vsctl add-port s2 "ge-1/1/14" vlan_mode=trunk -- set interface "ge-1/1/14" type="pica8" options:link_speed=990M
ovs-vsctl add-port s2 "ge-1/1/15" vlan_mode=trunk -- set interface "ge-1/1/15" type="pica8" options:link_speed=990M
ovs-vsctl add-port s2 "ge-1/1/16" vlan_mode=trunk -- set interface "ge-1/1/16" type="pica8" options:link_speed=990M
ovs-vsctl add-port s2 "ge-1/1/17" vlan_mode=trunk -- set interface "ge-1/1/17" type="pica8" options:link_speed=990M
ovs-vsctl add-port s2 "ge-1/1/18" vlan_mode=trunk -- set interface "ge-1/1/18" type="pica8" options:link_speed=990M
ovs-vsctl add-port s2 "ge-1/1/19" vlan_mode=trunk -- set interface "ge-1/1/19" type="pica8" options:link_speed=990M
ovs-vsctl add-port s2 "ge-1/1/20" vlan_mode=trunk -- set interface "ge-1/1/20" type="pica8" options:link_speed=990M
ovs-vsctl add-port s2 "ge-1/1/21" vlan_mode=trunk -- set interface "ge-1/1/21" type="pica8" options:link_speed=990M
ovs-vsctl add-port s2 "ge-1/1/22" vlan_mode=trunk -- set interface "ge-1/1/22" type="pica8" options:link_speed=990M
ovs-vsctl add-port s2 "ge-1/1/23" vlan_mode=trunk -- set interface "ge-1/1/23" type="pica8" options:link_speed=990M
ovs-vsctl add-port s2 "ge-1/1/24" vlan_mode=trunk -- set interface "ge-1/1/24" type="pica8" options:link_speed=990M

ovs-vsctl set-controller s2 tcp:192.168.1.10:6633
ovs-vsctl set-fail-mode s2 secure

echo "Switch 2 configured"
sleep 2


## s3
ovs-vsctl del-br s3
ovs-vsctl add-br s3 -- set bridge s3 datapath_type=pica8 protocols=OpenFlow10 -- set bridge s3 other-config:datapath-id=0000000000000003
ovs-vsctl add-port s3 "ge-1/1/25" vlan_mode=trunk -- set interface "ge-1/1/25" type="pica8" options:link_speed=990M
ovs-vsctl add-port s3 "ge-1/1/26" vlan_mode=trunk -- set interface "ge-1/1/26" type="pica8" options:link_speed=990M
ovs-vsctl add-port s3 "ge-1/1/27" vlan_mode=trunk -- set interface "ge-1/1/27" type="pica8" options:link_speed=990M
ovs-vsctl add-port s3 "ge-1/1/28" vlan_mode=trunk -- set interface "ge-1/1/28" type="pica8" options:link_speed=990M
ovs-vsctl add-port s3 "ge-1/1/29" vlan_mode=trunk -- set interface "ge-1/1/29" type="pica8" options:link_speed=990M
ovs-vsctl add-port s3 "ge-1/1/30" vlan_mode=trunk -- set interface "ge-1/1/30" type="pica8" options:link_speed=990M
ovs-vsctl add-port s3 "ge-1/1/31" vlan_mode=trunk -- set interface "ge-1/1/31" type="pica8" options:link_speed=990M
ovs-vsctl add-port s3 "ge-1/1/32" vlan_mode=trunk -- set interface "ge-1/1/32" type="pica8" options:link_speed=990M
ovs-vsctl add-port s3 "ge-1/1/33" vlan_mode=trunk -- set interface "ge-1/1/33" type="pica8" options:link_speed=990M
ovs-vsctl add-port s3 "ge-1/1/34" vlan_mode=trunk -- set interface "ge-1/1/34" type="pica8" options:link_speed=990M
ovs-vsctl add-port s3 "ge-1/1/35" vlan_mode=trunk -- set interface "ge-1/1/35" type="pica8" options:link_speed=990M
ovs-vsctl add-port s3 "ge-1/1/36" vlan_mode=trunk -- set interface "ge-1/1/36" type="pica8" options:link_speed=990M

ovs-vsctl set-controller s3 tcp:192.168.1.10:6633
ovs-vsctl set-fail-mode s3 secure

echo "Switch 3 configured"
sleep 2


## s4
ovs-vsctl del-br s4
ovs-vsctl add-br s4 -- set bridge s4 datapath_type=pica8 protocols=OpenFlow10 -- set bridge s4 other-config:datapath-id=0000000000000004
ovs-vsctl add-port s4 "ge-1/1/37" vlan_mode=trunk -- set interface "ge-1/1/37" type="pica8" options:link_speed=990M
ovs-vsctl add-port s4 "ge-1/1/38" vlan_mode=trunk -- set interface "ge-1/1/38" type="pica8" options:link_speed=990M
ovs-vsctl add-port s4 "ge-1/1/39" vlan_mode=trunk -- set interface "ge-1/1/39" type="pica8" options:link_speed=990M
ovs-vsctl add-port s4 "ge-1/1/40" vlan_mode=trunk -- set interface "ge-1/1/40" type="pica8" options:link_speed=990M
ovs-vsctl add-port s4 "ge-1/1/41" vlan_mode=trunk -- set interface "ge-1/1/41" type="pica8" options:link_speed=990M
ovs-vsctl add-port s4 "ge-1/1/42" vlan_mode=trunk -- set interface "ge-1/1/42" type="pica8" options:link_speed=990M
ovs-vsctl add-port s4 "ge-1/1/43" vlan_mode=trunk -- set interface "ge-1/1/43" type="pica8" options:link_speed=990M
ovs-vsctl add-port s4 "ge-1/1/44" vlan_mode=trunk -- set interface "ge-1/1/44" type="pica8" options:link_speed=990M
ovs-vsctl add-port s4 "ge-1/1/45" vlan_mode=trunk -- set interface "ge-1/1/45" type="pica8" options:link_speed=990M
ovs-vsctl add-port s4 "ge-1/1/46" vlan_mode=trunk -- set interface "ge-1/1/46" type="pica8" options:link_speed=990M
ovs-vsctl add-port s4 "ge-1/1/47" vlan_mode=trunk -- set interface "ge-1/1/47" type="pica8" options:link_speed=990M
ovs-vsctl add-port s4 "ge-1/1/48" vlan_mode=trunk -- set interface "ge-1/1/48" type="pica8" options:link_speed=990M

ovs-vsctl set-controller s4 tcp:192.168.1.10:6633
ovs-vsctl set-fail-mode s4 secure

echo "Switch 4 configured"
sleep 2


