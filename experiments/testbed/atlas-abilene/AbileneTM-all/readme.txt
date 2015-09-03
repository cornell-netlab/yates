* Format of X*.gz

  Each file contains 12x24x7 = 2016 5-min traffic matrices.

  Unit: (100 bytes / 5 minutes)  // 100 is the pkt sampling rate

  Each line belongs to one TM; and each line contains 144x5=720 values 
  organized as follows:

    <realOD_1> \
    <simpleGravityOD_1> \
    <simpleTomogravityOD_1> \
    <generalGravityOD_1> \
    <generalTomogravityOD_1> \

    ...

    <realOD_144> \
    <simpleGravityOD_144> \
    <simpleTomogravityOD_144> \
    <generalGravityOD_144> \
    <generalTomogravityOD_144> \

  where simpleGravity means the independence model: p(s,d) = p(s)p(d),
  whereas generalGravity means the conditional independence model,
  which treats outbound traffic differently.  simpleTomogravity and
  generalTomogravity are the tomogravity estimates with either
  simpleGravity or generalGravity as the prior.

  For Abilene, simpleGravity and generalGravity gives similar results.
  But in other networks, generalTomogravity often performs much
  better.

* Format of A

  A is the routing matrix.  it should be self-explanatory.

* Dates (the first day of each week)

  2004-03-01 X01
  2004-03-08 X02
  2004-04-02 X03
  2004-04-09 X04
  2004-04-22 X05
  2004-05-01 X06
  2004-05-08 X07
  2004-05-15 X08
  2004-05-22 X09
  2004-05-29 X10
  2004-06-05 X11
  2004-06-12 X12
  2004-06-19 X13
  2004-06-26 X14
  2004-07-03 X15
  2004-07-10 X16
  2004-07-17 X17
  2004-07-24 X18
  2004-07-31 X19
  2004-08-07 X20
  2004-08-13 X21
  2004-08-21 X22
  2004-08-28 X23
  2004-09-04 X24

* Collection methodology

  The idea is to directly work with destination AS numbers (which
  themselves are computed by the routers using the BGP tables when each
  netflow record is generated).

    (1) I examine the netflow leaving the egress links and build a set of
        destination AS numbers that egress at each router

    (2) from (1), for each destination AS number, I can find all the egress
        routers correspond to this AS.

    (3) At each ingress router, for each flow record, I can map its
        destination AS number to a set of egress routers using results
        of (2).  I then simulate OSPF to pick one egress router that is
        the closest.

  The above works as long as the destination AS is not Abilene (AS11537).

  When the destination is Abilene, in principle you could do the same
  thing on destination prefixes.  Unfortunately, the Abilene netflow
  mask out the last 11 bits of all the IP addresses, so it is impossible
  to get the true destination prefixes for Abilene.  As a result, the
  TMs I generated do not include any traffic destined for AS11537
  (fortunately they don't account for too much traffic).
