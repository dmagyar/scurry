# Introduction #

Scurry doesn't have a central server; this makes the task of network management and deployment a little tougher.

# Details #

Problems Scurry needs to solve:
  * IP address assignment.
    * For now, peers will be responsible for picking their own IP address.
    * Some protection against duplicate addresses should be possible.
  * Configuring UDP tunnels between peers
    * One problem is that in large networks, we don't want every peer connected to every other peer.
    * Another problem is that we need a good way for two clients behind NAT's to connect to eachother.
      * The Skype method of NAT/Firewall punching will probably be suitable to solve some of this problem (though it requires a third party to help establish the connection).