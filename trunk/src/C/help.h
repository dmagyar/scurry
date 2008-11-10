#ifndef __HELP_HASKELL_VPN__
#define __HELP_HASKELL_VPN__ __HELP_HASKELL_VPN__

typedef uint32_t ip4_addr_t;

/* A struct used to pass information about
 * a tap device around. */
struct tap_info {
    int fd;
    char mac[8]; /* Use 8, instead of 6, to force alignment */
};

/* Open a tap device. The file descriptor and the
 * MAC address are written to the tap_info struct.
 * On failure, a negative value is returned. */
int open_tap(ip4_addr_t local_ip,
             ip4_addr_t local_mask,
             struct tap_info * ti);

/* Close a tap device. */
void close_tun(int tun_fd);

/* Read a frame from a tap device. The buffer needs
 * to be at least as large as the MTU of the device. */
int read_tap(int fd, char * buf, int len);

/* Write a frame to a tap device. The frame length
 * must be less than the MTU of the device. */
int write_tap(int fd, const char * buf, int len);

#endif /* __HELP_HASKELL_VPN__ */

