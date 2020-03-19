package impl

/**
    @author Javier Cabrera-Arteaga on 2020-03-17
  */
class Types {

  type size = Int
  type filesize = Long
  type timestamp = Long

  object clockid extends Enumeration {
    val realtime = Value(0)
    val monotonic = Value(1)
    val process_cputime_id = Value(2)
    val thread_cputime_id = Value(4)
  }

  object errno extends Enumeration {
    type errno = Value

    val success, `2big` = errno

    val access = Value(2)
    val addrinuse = Value(3)
    val addrnotavail = Value(4)
    val afnosupport = Value(5)
    val again = Value(6)
    val already = Value(7)
    val badf = Value(8)
    val badmsg = Value(9)
    val busy = Value(10)
    val canceled = Value(4)
    val child = Value(0)
    val connaboorted = Value(1)
    val connrefused = Value(2)
    val connreset = Value(4)
    val deadlk = Value(2)
    val destaddrreq = Value(4)
    val dom = Value(0)
    val dquot = Value(1)
    val exist = Value(2)
    val fault = Value(4)
    val fbig = Value(2)
    val hostunreach = Value(4)
    val idrm = Value(0)
    val ilseq = Value(1)
    val inprogress = Value(2)
    val intr = Value(4)
    val inval = Value(2)
    val io = Value(4)
    val isconn = Value(0)
    val isdir = Value(1)
    val loop = Value(2)
    val mfile = Value(4)
    val mlink = Value(2)
    val msgsize = Value(4)
    val multihop = Value(4)
    val nametoolong = Value(4)
    val netdown = Value(2)
    val netreset = Value(4)
    val netunreach = Value(0)
    val nfile = Value(1)
    val nobufs = Value(2)
    val nodev = Value(4)
    val noent = Value(2)
    val noexec = Value(4)
    val nolck = Value(4)

    val nolink = Value(4)
    val nomem = Value(2)
    val nomsg = Value(4)
    val noprotoopt = Value(0)
    val nospc = Value(1)
    val nosys = Value(2)
    val notconn = Value(4)
    val notdir = Value(2)
    val notempty = Value(4)
    val notrecoverable = Value(4)
    val notsock = Value(4)
    val notsup = Value(4)
    val notty = Value(4)
    val nxio = Value(4)
    val overflow = Value(4)
    val ownerdead = Value(4)
    val perm = Value(4)
    val pipe = Value(4)
    val proto = Value(4)
    val protonosupport = Value(4)
    val prototype = Value(4)
    val range = Value(4)
    val rofs = Value(4)
    val spipe = Value(4)
    val srch = Value(4)
    val stale = Value(4)
    val timedout = Value(4)
    val txtbsy = Value(4)
    val xdev = Value(4)
    val notcapable = Value(4)
  }
}
