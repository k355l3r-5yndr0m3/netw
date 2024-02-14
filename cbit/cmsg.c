#include <stddef.h>
#include <sys/socket.h>

// It is recommeded to use the macros instead of manipulating msg_control directly
// for portability purposes
struct cmsghdr *cmsg_nxthdr(void *control, size_t controllen, struct cmsghdr *cmsg) {
    struct msghdr msghdr = { .msg_control = control, .msg_controllen = controllen };
    return CMSG_NXTHDR(&msghdr, cmsg);
}

struct cmsghdr *cmsg_firsthdr(void *control, size_t controllen) {
    struct msghdr msghdr = { .msg_control = control, .msg_controllen = controllen };
    return CMSG_FIRSTHDR(&msghdr);
}

