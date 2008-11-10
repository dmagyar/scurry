#ifndef __HELP_HASKELL_VPN__
#define __HELP_HASKELL_VPN__ __HELP_HASKELL_VPN__

typedef uint32_t ip4_addr_t; // #Job - changed to uint32_t as it is more cross platform compatable (and doesn't work on windows)

struct tap_info {
    int fd;
    char mac[8]; /* Use 8, instead of 6, to force alignment */
};

int open_tap(ip4_addr_t local_ip, ip4_addr_t local_mask, struct tap_info * ti);
void close_tun(int tun_fd);


int read_tap(int fd, char * buf, int len);
int write_tap(int fd, const char * buf, int len);
#endif /* __HELP_HASKELL_VPN__ */
