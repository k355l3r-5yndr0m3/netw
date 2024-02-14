#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>

#include "Rts.h"

#define SMALL_NELEM 4

ssize_t sendmsg_wrapper0(int fd, 
                        struct sockaddr *name, socklen_t namelen,
                        StgArrBytes **iovecs, ptrdiff_t *offs, size_t *len, size_t iovlen,
                        void *control, size_t controllen,
                        int flags) {
    struct iovec short_iovecs[SMALL_NELEM]; // use if iovlen is small enough
                                            // else allocate on the heap
    struct iovec *iov = iovlen > SMALL_NELEM ? malloc(sizeof(*iov) * iovlen) : short_iovecs;
    for (size_t i = 0; i < iovlen; ++i)
        iov[i] = (struct iovec){ .iov_base = (char*)iovecs[i]->payload + offs[i], .iov_len = len[i] };

    struct msghdr msghdr = {
        .msg_name       = name,
        .msg_namelen    = namelen,
        .msg_iov        = iov,
        .msg_iovlen     = iovlen, 
        .msg_control    = control,
        .msg_controllen = controllen,
    };
    ssize_t ret = sendmsg(fd, &msghdr, flags);
    if (iovlen > SMALL_NELEM)
        free(iov);
    return ret;
}

ssize_t sendmsg_wrapper1(int fd,
                         struct sockaddr *name, socklen_t namelen,
                         struct iovec *iov, size_t iovlen,
                         void *control, size_t controllen,
                         int flags) {
    struct msghdr msghdr = {
        .msg_name       = name,
        .msg_namelen    = namelen,
        .msg_iov        = iov,
        .msg_iovlen     = iovlen, 
        .msg_control    = control,
        .msg_controllen = controllen,
    };
    return sendmsg(fd, &msghdr, flags);
}
