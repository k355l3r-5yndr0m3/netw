#include <sys/socket.h>
#if __linux__
#include <linux/version.h>

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,27)
#define USE_LINUX_SOCK_NONBLOCK 1
#endif

#endif

#if !USE_LINUX_SOCK_NONBLOCK
#include <fcntl.h>
#endif

int socket_wrapper(int domain, int type, int protocol) {
#if USE_LINUX_SOCK_NONBLOCK
    return socket(domain, type | SOCK_NONBLOCK, protocol);
#else
    int flags;
    int fd;
    if ((fd = socket(domain, type, protocol)) == -1)
        return -1;
    if ((flags = fcntl(fd, F_GETFL)) == -1)
        return -1;
    if (fcntl(fd, F_SETFL, flags | flags) == -1)
        return -1;
    return fd;
#endif
}
