// This C file contains static_assertions and compile time check (using template haskell)
#include <assert.h>
#include <stdbool.h>
#include <sys/socket.h>

// static_assert(cond, msg);

#define CHECK_CMSG_LEN_BEHAVIOR(s) static_assert(CMSG_LEN(s) == CMSG_LEN(0) + s, "CMSG_LEN behavior unexpected!")
CHECK_CMSG_LEN_BEHAVIOR(0);
CHECK_CMSG_LEN_BEHAVIOR(1);
CHECK_CMSG_LEN_BEHAVIOR(2);
CHECK_CMSG_LEN_BEHAVIOR(3);
CHECK_CMSG_LEN_BEHAVIOR(4);
CHECK_CMSG_LEN_BEHAVIOR(5);
CHECK_CMSG_LEN_BEHAVIOR(6);
CHECK_CMSG_LEN_BEHAVIOR(7);
CHECK_CMSG_LEN_BEHAVIOR(8);
CHECK_CMSG_LEN_BEHAVIOR(9);
CHECK_CMSG_LEN_BEHAVIOR(10);
CHECK_CMSG_LEN_BEHAVIOR(11);
CHECK_CMSG_LEN_BEHAVIOR(12);
CHECK_CMSG_LEN_BEHAVIOR(13);
CHECK_CMSG_LEN_BEHAVIOR(14);
CHECK_CMSG_LEN_BEHAVIOR(15);
CHECK_CMSG_LEN_BEHAVIOR(16);
CHECK_CMSG_LEN_BEHAVIOR(17);
CHECK_CMSG_LEN_BEHAVIOR(18);
CHECK_CMSG_LEN_BEHAVIOR(19);
CHECK_CMSG_LEN_BEHAVIOR(20);
CHECK_CMSG_LEN_BEHAVIOR(21);
CHECK_CMSG_LEN_BEHAVIOR(22);
CHECK_CMSG_LEN_BEHAVIOR(23);
CHECK_CMSG_LEN_BEHAVIOR(24);
CHECK_CMSG_LEN_BEHAVIOR(25);
CHECK_CMSG_LEN_BEHAVIOR(26);
CHECK_CMSG_LEN_BEHAVIOR(27);
CHECK_CMSG_LEN_BEHAVIOR(28);
CHECK_CMSG_LEN_BEHAVIOR(29);
CHECK_CMSG_LEN_BEHAVIOR(30);
CHECK_CMSG_LEN_BEHAVIOR(31);
CHECK_CMSG_LEN_BEHAVIOR(32);
#undef CHECK_CMSG_LEN_BEHAVIOR

#define CHECK_CMSG_DATA_BEHAVIOR(s0, s1) static_assert((char*)CMSG_DATA((struct cmsghdr*)s0) - (char*)s0 == (char*)CMSG_DATA((struct cmsghdr*)s1) - (char*)s1, "CMSG_DATA behavior unexpected!");
CHECK_CMSG_DATA_BEHAVIOR(0x33, 0x72);
CHECK_CMSG_DATA_BEHAVIOR(0x7924, 0x73262);
CHECK_CMSG_DATA_BEHAVIOR(0x9482764, 0x832);
CHECK_CMSG_DATA_BEHAVIOR(0x2460, 0x731);
#undef CHECK_CMSG_LEN_BEHAVIOR

