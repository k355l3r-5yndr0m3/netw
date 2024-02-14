#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <sys/socket.h>


ssize_t recv_wrapper(int sockfd, unsigned char *buffer, ptrdiff_t offs, size_t size, int flags) {
    return recv(sockfd, buffer + offs, size, flags);
}

ssize_t recvfrom_wrapper(int fd, char *restrict buf, ptrdiff_t offs, size_t size, int flags, struct sockaddr *restrict addr, socklen_t *addr_len) {
    return recvfrom(fd, buf + offs, size, flags, addr, addr_len);
}
