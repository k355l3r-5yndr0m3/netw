#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <sys/socket.h>

ssize_t send_wrapper(int socket, const char *buffer, ptrdiff_t offs, size_t size, int flags) { return send(socket, buffer + offs, size, flags); }
ssize_t sendto_wrapper(int socket, const char *buffer, ptrdiff_t offs, size_t size, int flags,
                       const struct sockaddr *name, socklen_t namelen) {
    return sendto(socket, buffer + offs, size, flags, name, namelen);
}
