#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <sys/socket.h>

#include "Rts.h"

#define SMALL_NELEM 4

ssize_t recvmsg_wrapper0(int sockfd,
                        struct sockaddr *name, socklen_t *namelen,
                        StgArrBytes **iovecs, ptrdiff_t *offs, size_t *size, size_t iovlen,
                        void *control, size_t *controllen,
                        int *flags) {
    struct iovec short_iovecs[SMALL_NELEM];
    struct iovec *iov = iovlen > SMALL_NELEM ? malloc(sizeof(*iov) * iovlen) : short_iovecs;

    for (size_t i = 0; i < iovlen; ++i)
        iov[i] = (struct iovec){ .iov_base = (char*)iovecs[i]->payload + offs[i], .iov_len = size[i] };

    struct msghdr msghdr = {
        .msg_name = name, .msg_namelen = *namelen,
        .msg_iov = iov, .msg_iovlen = iovlen,
        .msg_control = control, .msg_controllen = *controllen,
    };

    ssize_t recved = recvmsg(sockfd, &msghdr, *flags);

    if (iovlen > SMALL_NELEM)
        free(iov);
    
    // outputing
    *namelen = msghdr.msg_namelen;
    *controllen = msghdr.msg_controllen;
    *flags = msghdr.msg_flags;
    
    return recved;
}

ssize_t recvmsg_wrapper1(int sockfd,
                         struct sockaddr *name, socklen_t *namelen,
                         struct iovec *iov, size_t iovlen,
                         void *control, size_t *controllen,
                         int *flags) {
    struct msghdr msghdr = {
        .msg_name = name, .msg_namelen = *namelen,
        .msg_iov = iov, .msg_iovlen = iovlen,
        .msg_control = control, .msg_controllen = *controllen,
    };
    ssize_t recved = recvmsg(sockfd, &msghdr, *flags);

    *namelen = msghdr.msg_namelen;
    *controllen = msghdr.msg_controllen;
    *flags = msghdr.msg_flags;

    return recved;
}
