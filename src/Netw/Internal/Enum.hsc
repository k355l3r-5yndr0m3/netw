{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module Netw.Internal.Enum where
import Netw.Internal.Protocols
{- NOTE: This module is platform-depenent (Linux)
 - -}
#include <sys/socket.h>
#include <netinet/in.h>

import Foreign
import Foreign.C


newtype ProtocolFamily  = ProtocolFamily CInt deriving (Eq)
-- TODO: Add more and maybe use macro to check if value is defined
-- PF_LOCAL and PF_UNIX should be the same on every platform
#if linux_HOST_OS
pattern PF_UNIX         = ProtocolFamily #{const PF_UNIX       }
pattern PF_LOCAL        = ProtocolFamily #{const PF_LOCAL      }
pattern PF_INET         = ProtocolFamily #{const PF_INET       }
pattern PF_AX25         = ProtocolFamily #{const PF_AX25       }
pattern PF_IPX          = ProtocolFamily #{const PF_IPX        }
pattern PF_APPLETALK    = ProtocolFamily #{const PF_APPLETALK  }
pattern PF_NETROM       = ProtocolFamily #{const PF_NETROM     }
pattern PF_BRIDGE       = ProtocolFamily #{const PF_BRIDGE     }
pattern PF_ATMPVC       = ProtocolFamily #{const PF_ATMPVC     }
pattern PF_X25          = ProtocolFamily #{const PF_X25        }
pattern PF_INET6        = ProtocolFamily #{const PF_INET6      }
pattern PF_ROSE         = ProtocolFamily #{const PF_ROSE       }
pattern PF_DECnet       = ProtocolFamily #{const PF_DECnet     }
pattern PF_NETBEUI      = ProtocolFamily #{const PF_NETBEUI    }
pattern PF_SECURITY     = ProtocolFamily #{const PF_SECURITY   }
pattern PF_KEY          = ProtocolFamily #{const PF_KEY        }
pattern PF_NETLINK      = ProtocolFamily #{const PF_NETLINK    }
pattern PF_PACKET       = ProtocolFamily #{const PF_PACKET     }
pattern PF_ECONET       = ProtocolFamily #{const PF_ECONET     }
pattern PF_ATMSVC       = ProtocolFamily #{const PF_ATMSVC     }
pattern PF_RDS          = ProtocolFamily #{const PF_RDS        }
pattern PF_IRDA         = ProtocolFamily #{const PF_IRDA       }
pattern PF_PPPOX        = ProtocolFamily #{const PF_PPPOX      }
pattern PF_WANPIPE      = ProtocolFamily #{const PF_WANPIPE    }
pattern PF_LLC          = ProtocolFamily #{const PF_LLC        }
pattern PF_IB           = ProtocolFamily #{const PF_IB         }
pattern PF_MPLS         = ProtocolFamily #{const PF_MPLS       }
pattern PF_CAN          = ProtocolFamily #{const PF_CAN        }
pattern PF_TIPC         = ProtocolFamily #{const PF_TIPC       }
pattern PF_BLUETOOTH    = ProtocolFamily #{const PF_BLUETOOTH  }
pattern PF_IUCV         = ProtocolFamily #{const PF_IUCV       }
pattern PF_RXRPC        = ProtocolFamily #{const PF_RXRPC      }
pattern PF_ISDN         = ProtocolFamily #{const PF_ISDN       }
pattern PF_PHONET       = ProtocolFamily #{const PF_PHONET     }
pattern PF_IEEE802154   = ProtocolFamily #{const PF_IEEE802154 }
pattern PF_CAIF         = ProtocolFamily #{const PF_CAIF       }
pattern PF_ALG          = ProtocolFamily #{const PF_ALG        }
pattern PF_VSOCK        = ProtocolFamily #{const PF_VSOCK      }
pattern PF_KCM          = ProtocolFamily #{const PF_KCM        }
pattern PF_QIPCRTR      = ProtocolFamily #{const PF_QIPCRTR    }
pattern PF_SMC          = ProtocolFamily #{const PF_SMC        }
pattern PF_XDP          = ProtocolFamily #{const PF_XDP        }
#endif

newtype SocketType      = SocketType CInt deriving (Eq, Bits)
pattern SOCK_STREAM     = SocketType #{const SOCK_STREAM    }
pattern SOCK_DGRAM      = SocketType #{const SOCK_DGRAM     }
pattern SOCK_SEQPACKET  = SocketType #{const SOCK_SEQPACKET }

#if linux_HOST_OS
pattern SOCK_RAW        = SocketType #{const SOCK_RAW       }
pattern SOCK_RDM        = SocketType #{const SOCK_RDM       }
pattern SOCK_PACKET     = SocketType #{const SOCK_PACKET    }

-- These are a little different
pattern SOCK_NONBLOCK   = SocketType #{const SOCK_NONBLOCK  }
pattern SOCK_CLOEXEC    = SocketType #{const SOCK_CLOEXEC   }
#endif

newtype Protocol        = Protocol CInt deriving Eq
pattern DefaultProtocol = Protocol 0
-- Some values have different name from the equivalent in 
-- netinet/in.h
$(declareProtocols 'Protocol)

-- These value are not defined in /etc/protocols but is defined
-- in netinet/in.h
#if linux_HOST_OS
pattern IPPROTO_IP      = Protocol #{const IPPROTO_IP   }
pattern IPPROTO_RAW     = Protocol #{const IPPROTO_RAW  }
pattern IPPROTO_MPTCP   = Protocol #{const IPPROTO_MPTCP}
pattern IPPROTO_MH      = Protocol #{const IPPROTO_MH   }
#endif

newtype MsgFlags        = MsgFlags CInt deriving (Eq, Bits, Storable)
pattern MSG_EOR         = MsgFlags #{const MSG_EOR      }
pattern MSG_OOB         = MsgFlags #{const MSG_OOB      }
pattern MSG_NOSIGNAL    = MsgFlags #{const MSG_NOSIGNAL }
pattern MSG_PEEK        = MsgFlags #{const MSG_PEEK     }
pattern MSG_WAITALL     = MsgFlags #{const MSG_WAITALL  }
pattern MSG_TRUNC       = MsgFlags #{const MSG_TRUNC    }
pattern MSG_CTRUNC      = MsgFlags #{const MSG_CTRUNC   }

-- These seem to be specific to linux
-- They don't appear in posix man pages
#if linux_HOST_OS
pattern MSG_CONFIRM      = MsgFlags (#const MSG_CONFIRM   )   
pattern MSG_DONTROUTE    = MsgFlags (#const MSG_DONTROUTE )   
pattern MSG_DONTWAIT     = MsgFlags (#const MSG_DONTWAIT  )   
pattern MSG_MORE         = MsgFlags (#const MSG_MORE      )   
pattern MSG_FASTOPEN     = MsgFlags (#const MSG_FASTOPEN  )    

pattern MSG_CMSG_CLOEXEC = MsgFlags (#const MSG_CMSG_CLOEXEC )
pattern MSG_ERRQUEUE     = MsgFlags (#const MSG_ERRQUEUE     )
#endif

newtype ShutHow   = ShutHow CInt
pattern SHUT_RD   = ShutHow #{const SHUT_RD  }
pattern SHUT_WR   = ShutHow #{const SHUT_WR  }
pattern SHUT_RDWR = ShutHow #{const SHUT_RDWR}
