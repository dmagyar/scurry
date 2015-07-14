# Introduction #

This is a short description which will help you get Scurry running on various platforms.


# Linux #
  * Checkout the scurry project.
    * svn checkout http://scurry.googlecode.com/svn/trunk/ scurry
  * cd scurry
  * Edit 'files/config.json'
    * The "vpn" object defines the IP and netmask which will be used on the VPN.
    * The "network" object defines the IP and port upon which scurry will listen for connections from other peers.
  * Edit 'files/tracker.json'
    * This is an array of peers.
    * Each peer in the array needs a host address and a port number.
    * Scurry will attempt to open communication channels with these peers when it starts.
  * runhaskell Setup configure
  * runhaskell Setup build
  * Since we're creating a network device, we'll need to run the following command as root.
  * sudo ./dist/build/scurry/scurry files/config.json files/tracker.json
  * Once the Scurry starts, you'll notice that you have a new tapX device when you run ifconfig. It will have the netmask and IP address set in 'files/config.json'.

# Windows #
  * Windows Vista and XP are supported through the TAP-Win32 driver. This is installed with the OpenVPN package (and some other VPN clients).
  * To use the TAP-Win32 installed with OpenVPN...
    * Start -> All Programs -> OpenVPN -> Add a new TAP-Win32 virtual ethernet adapter
    * This installs a new virtual Ethernet adapter
  * Build scurry as described in the Linux heading above.
  * For Windows Vista
    * In an administrator command prompt run: dist\build\scurry\scurry.exe files\config.json files\tracker.json
  * For Windows XP
    * In a command prompt in the same directory that you built scurry run:
> > dist\build\scurry\scurry.exe files\config.json files\tracker.json


# OS/X #
  * Install [TUNTAP driver](http://tuntaposx.sourceforge.net/) for OS X
  * Same steps as for Linux

# FreeBSD (possibly others) #
  * Run 'kldload if\_tap'
  * Same steps as for Linux