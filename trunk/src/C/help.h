#ifndef __HELP_HASKELL_VPN__
#define __HELP_HASKELL_VPN__ __HELP_HASKELL_VPN__

typedef u_int32_t ip4_addr_t;

struct tap_info {
    int fd;
    char mac[8]; /* Use 8, instead of 6, to force alignment */
};

int open_tap(ip4_addr_t local_ip, ip4_addr_t local_mask, struct tap_info * ti);
void close_tun(int tun_fd);

#endif /* __HELP_HASKELL_VPN__ */
