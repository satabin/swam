package swam
package stdlib

import utest.{TestSuite, Tests, test}
import runtime._
import swam.test.util._
import utest._
import better.files._
import fastparse._
import cats.effect._
import swam.text.parser
import swam.witx.parser.{ModuleParser, TypesParser}

import scala.concurrent.ExecutionContext
object WitParser extends TestSuite {

  val tests = Tests {
    test("console_tracer") {

      val types =
        """;; Type names used by low-level WASI interfaces.
                     |;;
                     |;; Some content here is derived from [CloudABI](https://github.com/NuxiNL/cloudabi).
                     |;;
                     |;; This is a `witx` file. See [here](https://github.com/WebAssembly/WASI/tree/master/docs/witx.md)
                     |;; for an explanation of what that means.
                     |
                     |(typename $size u32)
                     |
                     |;;; Non-negative file size or length of a region within a file.
                     |(typename $filesize u64)
                     |
                     |;;; Timestamp in nanoseconds.
                     |(typename $timestamp u64)
                     |
                     |;;; Identifiers for clocks.
                     |(typename $clockid
                     |  (enum u32
                     |    ;;; The clock measuring real time. Time value zero corresponds with
                     |    ;;; 1970-01-01T00:00:00Z.
                     |    $realtime
                     |    ;;; The store-wide monotonic clock, which is defined as a clock measuring
                     |    ;;; real time, whose value cannot be adjusted and which cannot have negative
                     |    ;;; clock jumps. The epoch of this clock is undefined. The absolute time
                     |    ;;; value of this clock therefore has no meaning.
                     |    $monotonic
                     |    ;;; The CPU-time clock associated with the current process.
                     |    $process_cputime_id
                     |    ;;; The CPU-time clock associated with the current thread.
                     |    $thread_cputime_id
                     |  )
                     |)
                     |
                     |;;; Error codes returned by functions.
                     |;;; Not all of these error codes are returned by the functions provided by this
                     |;;; API; some are used in higher-level library layers, and others are provided
                     |;;; merely for alignment with POSIX.
                     |(typename $errno
                     |  (enum u16
                     |    ;;; No error occurred. System call completed successfully.
                     |    $success
                     |    ;;; Argument list too long.
                     |    $2big
                     |    ;;; Permission denied.
                     |    $acces
                     |    ;;; Address in use.
                     |    $addrinuse
                     |    ;;; Address not available.
                     |    $addrnotavail
                     |    ;;; Address family not supported.
                     |    $afnosupport
                     |    ;;; Resource unavailable, or operation would block.
                     |    $again
                     |    ;;; Connection already in progress.
                     |    $already
                     |    ;;; Bad file descriptor.
                     |    $badf
                     |    ;;; Bad message.
                     |    $badmsg
                     |    ;;; Device or resource busy.
                     |    $busy
                     |    ;;; Operation canceled.
                     |    $canceled
                     |    ;;; No child processes.
                     |    $child
                     |    ;;; Connection aborted.
                     |    $connaborted
                     |    ;;; Connection refused.
                     |    $connrefused
                     |    ;;; Connection reset.
                     |    $connreset
                     |    ;;; Resource deadlock would occur.
                     |    $deadlk
                     |    ;;; Destination address required.
                     |    $destaddrreq
                     |    ;;; Mathematics argument out of domain of function.
                     |    $dom
                     |    ;;; Reserved.
                     |    $dquot
                     |    ;;; File exists.
                     |    $exist
                     |    ;;; Bad address.
                     |    $fault
                     |    ;;; File too large.
                     |    $fbig
                     |    ;;; Host is unreachable.
                     |    $hostunreach
                     |    ;;; Identifier removed.
                     |    $idrm
                     |    ;;; Illegal byte sequence.
                     |    $ilseq
                     |    ;;; Operation in progress.
                     |    $inprogress
                     |    ;;; Interrupted function.
                     |    $intr
                     |    ;;; Invalid argument.
                     |    $inval
                     |    ;;; I/O error.
                     |    $io
                     |    ;;; Socket is connected.
                     |    $isconn
                     |    ;;; Is a directory.
                     |    $isdir
                     |    ;;; Too many levels of symbolic links.
                     |    $loop
                     |    ;;; File descriptor value too large.
                     |    $mfile
                     |    ;;; Too many links.
                     |    $mlink
                     |    ;;; Message too large.
                     |    $msgsize
                     |    ;;; Reserved.
                     |    $multihop
                     |    ;;; Filename too long.
                     |    $nametoolong
                     |    ;;; Network is down.
                     |    $netdown
                     |    ;;; Connection aborted by network.
                     |    $netreset
                     |    ;;; Network unreachable.
                     |    $netunreach
                     |    ;;; Too many files open in system.
                     |    $nfile
                     |    ;;; No buffer space available.
                     |    $nobufs
                     |    ;;; No such device.
                     |    $nodev
                     |    ;;; No such file or directory.
                     |    $noent
                     |    ;;; Executable file format error.
                     |    $noexec
                     |    ;;; No locks available.
                     |    $nolck
                     |    ;;; Reserved.
                     |    $nolink
                     |    ;;; Not enough space.
                     |    $nomem
                     |    ;;; No message of the desired type.
                     |    $nomsg
                     |    ;;; Protocol not available.
                     |    $noprotoopt
                     |    ;;; No space left on device.
                     |    $nospc
                     |    ;;; Function not supported.
                     |    $nosys
                     |    ;;; The socket is not connected.
                     |    $notconn
                     |    ;;; Not a directory or a symbolic link to a directory.
                     |    $notdir
                     |    ;;; Directory not empty.
                     |    $notempty
                     |    ;;; State not recoverable.
                     |    $notrecoverable
                     |    ;;; Not a socket.
                     |    $notsock
                     |    ;;; Not supported, or operation not supported on socket.
                     |    $notsup
                     |    ;;; Inappropriate I/O control operation.
                     |    $notty
                     |    ;;; No such device or address.
                     |    $nxio
                     |    ;;; Value too large to be stored in data type.
                     |    $overflow
                     |    ;;; Previous owner died.
                     |    $ownerdead
                     |    ;;; Operation not permitted.
                     |    $perm
                     |    ;;; Broken pipe.
                     |    $pipe
                     |    ;;; Protocol error.
                     |    $proto
                     |    ;;; Protocol not supported.
                     |    $protonosupport
                     |    ;;; Protocol wrong type for socket.
                     |    $prototype
                     |    ;;; Result too large.
                     |    $range
                     |    ;;; Read-only file system.
                     |    $rofs
                     |    ;;; Invalid seek.
                     |    $spipe
                     |    ;;; No such process.
                     |    $srch
                     |    ;;; Reserved.
                     |    $stale
                     |    ;;; Connection timed out.
                     |    $timedout
                     |    ;;; Text file busy.
                     |    $txtbsy
                     |    ;;; Cross-device link.
                     |    $xdev
                     |    ;;; Extension: Capabilities insufficient.
                     |    $notcapable
                     |  )
                     |)
                     |
                     |;;; File descriptor rights, determining which actions may be performed.
                     |(typename $rights
                     |  (flags u64
                     |    ;;; The right to invoke `fd_datasync`.
                     |    ;;
                     |    ;;; If `path_open` is set, includes the right to invoke
                     |    ;;; `path_open` with `fdflags::dsync`.
                     |    $fd_datasync
                     |    ;;; The right to invoke `fd_read` and `sock_recv`.
                     |    ;;
                     |    ;;; If `rights::fd_seek` is set, includes the right to invoke `fd_pread`.
                     |    $fd_read
                     |    ;;; The right to invoke `fd_seek`. This flag implies `rights::fd_tell`.
                     |    $fd_seek
                     |    ;;; The right to invoke `fd_fdstat_set_flags`.
                     |    $fd_fdstat_set_flags
                     |    ;;; The right to invoke `fd_sync`.
                     |    ;;
                     |    ;;; If `path_open` is set, includes the right to invoke
                     |    ;;; `path_open` with `fdflags::rsync` and `fdflags::dsync`.
                     |    $fd_sync
                     |    ;;; The right to invoke `fd_seek` in such a way that the file offset
                     |    ;;; remains unaltered (i.e., `whence::cur` with offset zero), or to
                     |    ;;; invoke `fd_tell`.
                     |    $fd_tell
                     |    ;;; The right to invoke `fd_write` and `sock_send`.
                     |    ;;; If `rights::fd_seek` is set, includes the right to invoke `fd_pwrite`.
                     |    $fd_write
                     |    ;;; The right to invoke `fd_advise`.
                     |    $fd_advise
                     |    ;;; The right to invoke `fd_allocate`.
                     |    $fd_allocate
                     |    ;;; The right to invoke `path_create_directory`.
                     |    $path_create_directory
                     |    ;;; If `path_open` is set, the right to invoke `path_open` with `oflags::creat`.
                     |    $path_create_file
                     |    ;;; The right to invoke `path_link` with the file descriptor as the
                     |    ;;; source directory.
                     |    $path_link_source
                     |    ;;; The right to invoke `path_link` with the file descriptor as the
                     |    ;;; target directory.
                     |    $path_link_target
                     |    ;;; The right to invoke `path_open`.
                     |    $path_open
                     |    ;;; The right to invoke `fd_readdir`.
                     |    $fd_readdir
                     |    ;;; The right to invoke `path_readlink`.
                     |    $path_readlink
                     |    ;;; The right to invoke `path_rename` with the file descriptor as the source directory.
                     |    $path_rename_source
                     |    ;;; The right to invoke `path_rename` with the file descriptor as the target directory.
                     |    $path_rename_target
                     |    ;;; The right to invoke `path_filestat_get`.
                     |    $path_filestat_get
                     |    ;;; The right to change a file's size (there is no `path_filestat_set_size`).
                     |    ;;; If `path_open` is set, includes the right to invoke `path_open` with `oflags::trunc`.
                     |    $path_filestat_set_size
                     |    ;;; The right to invoke `path_filestat_set_times`.
                     |    $path_filestat_set_times
                     |    ;;; The right to invoke `fd_filestat_get`.
                     |    $fd_filestat_get
                     |    ;;; The right to invoke `fd_filestat_set_size`.
                     |    $fd_filestat_set_size
                     |    ;;; The right to invoke `fd_filestat_set_times`.
                     |    $fd_filestat_set_times
                     |    ;;; The right to invoke `path_symlink`.
                     |    $path_symlink
                     |    ;;; The right to invoke `path_remove_directory`.
                     |    $path_remove_directory
                     |    ;;; The right to invoke `path_unlink_file`.
                     |    $path_unlink_file
                     |    ;;; If `rights::fd_read` is set, includes the right to invoke `poll_oneoff` to subscribe to `eventtype::fd_read`.
                     |    ;;; If `rights::fd_write` is set, includes the right to invoke `poll_oneoff` to subscribe to `eventtype::fd_write`.
                     |    $poll_fd_readwrite
                     |    ;;; The right to invoke `sock_shutdown`.
                     |    $sock_shutdown
                     |  )
                     |)
                     |
                     |;;; A file descriptor handle.
                     |(typename $fd (handle))
                     |
                     |;;; A region of memory for scatter/gather reads.
                     |(typename $iovec
                     |  (struct
                     |    ;;; The address of the buffer to be filled.
                     |    (field $buf (@witx pointer u8))
                     |    ;;; The length of the buffer to be filled.
                     |    (field $buf_len $size)
                     |  )
                     |)
                     |
                     |;;; A region of memory for scatter/gather writes.
                     |(typename $ciovec
                     |  (struct
                     |    ;;; The address of the buffer to be written.
                     |    (field $buf (@witx const_pointer u8))
                     |    ;;; The length of the buffer to be written.
                     |    (field $buf_len $size)
                     |  )
                     |)
                     |
                     |(typename $iovec_array (array $iovec))
                     |(typename $ciovec_array (array $ciovec))
                     |
                     |;;; Relative offset within a file.
                     |(typename $filedelta s64)
                     |
                     |;;; The position relative to which to set the offset of the file descriptor.
                     |(typename $whence
                     |  (enum u8
                     |    ;;; Seek relative to start-of-file.
                     |    $set
                     |    ;;; Seek relative to current position.
                     |    $cur
                     |    ;;; Seek relative to end-of-file.
                     |    $end
                     |  )
                     |)
                     |
                     |;;; A reference to the offset of a directory entry.
                     |;;;
                     |;;; The value 0 signifies the start of the directory.
                     |(typename $dircookie u64)
                     |
                     |;;; The type for the $d_namlen field of $dirent.
                     |(typename $dirnamlen u32)
                     |
                     |;;; File serial number that is unique within its file system.
                     |(typename $inode u64)
                     |
                     |;;; The type of a file descriptor or file.
                     |(typename $filetype
                     |  (enum u8
                     |    ;;; The type of the file descriptor or file is unknown or is different from any of the other types specified.
                     |    $unknown
                     |    ;;; The file descriptor or file refers to a block device inode.
                     |    $block_device
                     |    ;;; The file descriptor or file refers to a character device inode.
                     |    $character_device
                     |    ;;; The file descriptor or file refers to a directory inode.
                     |    $directory
                     |    ;;; The file descriptor or file refers to a regular file inode.
                     |    $regular_file
                     |    ;;; The file descriptor or file refers to a datagram socket.
                     |    $socket_dgram
                     |    ;;; The file descriptor or file refers to a byte-stream socket.
                     |    $socket_stream
                     |    ;;; The file refers to a symbolic link inode.
                     |    $symbolic_link
                     |  )
                     |)
                     |
                     |;;; A directory entry.
                     |(typename $dirent
                     |  (struct
                     |    ;;; The offset of the next directory entry stored in this directory.
                     |    (field $d_next $dircookie)
                     |    ;;; The serial number of the file referred to by this directory entry.
                     |    (field $d_ino $inode)
                     |    ;;; The length of the name of the directory entry.
                     |    (field $d_namlen $dirnamlen)
                     |    ;;; The type of the file referred to by this directory entry.
                     |    (field $d_type $filetype)
                     |  )
                     |)
                     |
                     |;;; File or memory access pattern advisory information.
                     |(typename $advice
                     |  (enum u8
                     |    ;;; The application has no advice to give on its behavior with respect to the specified data.
                     |    $normal
                     |    ;;; The application expects to access the specified data sequentially from lower offsets to higher offsets.
                     |    $sequential
                     |    ;;; The application expects to access the specified data in a random order.
                     |    $random
                     |    ;;; The application expects to access the specified data in the near future.
                     |    $willneed
                     |    ;;; The application expects that it will not access the specified data in the near future.
                     |    $dontneed
                     |    ;;; The application expects to access the specified data once and then not reuse it thereafter.
                     |    $noreuse
                     |  )
                     |)
                     |
                     |;;; File descriptor flags.
                     |(typename $fdflags
                     |  (flags u16
                     |    ;;; Append mode: Data written to the file is always appended to the file's end.
                     |    $append
                     |    ;;; Write according to synchronized I/O data integrity completion. Only the data stored in the file is synchronized.
                     |    $dsync
                     |    ;;; Non-blocking mode.
                     |    $nonblock
                     |    ;;; Synchronized read I/O operations.
                     |    $rsync
                     |    ;;; Write according to synchronized I/O file integrity completion. In
                     |    ;;; addition to synchronizing the data stored in the file, the implementation
                     |    ;;; may also synchronously update the file's metadata.
                     |    $sync
                     |  )
                     |)
                     |
                     |;;; File descriptor attributes.
                     |(typename $fdstat
                     |  (struct
                     |    ;;; File type.
                     |    (field $fs_filetype $filetype)
                     |    ;;; File descriptor flags.
                     |    (field $fs_flags $fdflags)
                     |    ;;; Rights that apply to this file descriptor.
                     |    (field $fs_rights_base $rights)
                     |    ;;; Maximum set of rights that may be installed on new file descriptors that
                     |    ;;; are created through this file descriptor, e.g., through `path_open`.
                     |    (field $fs_rights_inheriting $rights)
                     |  )
                     |)
                     |
                     |;;; Identifier for a device containing a file system. Can be used in combination
                     |;;; with `inode` to uniquely identify a file or directory in the filesystem.
                     |(typename $device u64)
                     |
                     |;;; Which file time attributes to adjust.
                     |(typename $fstflags
                     |  (flags u16
                     |    ;;; Adjust the last data access timestamp to the value stored in `filestat::atim`.
                     |    $atim
                     |    ;;; Adjust the last data access timestamp to the time of clock `clockid::realtime`.
                     |    $atim_now
                     |    ;;; Adjust the last data modification timestamp to the value stored in `filestat::mtim`.
                     |    $mtim
                     |    ;;; Adjust the last data modification timestamp to the time of clock `clockid::realtime`.
                     |    $mtim_now
                     |  )
                     |)
                     |
                     |;;; Flags determining the method of how paths are resolved.
                     |(typename $lookupflags
                     |  (flags u32
                     |    ;;; As long as the resolved path corresponds to a symbolic link, it is expanded.
                     |    $symlink_follow
                     |  )
                     |)
                     |
                     |;;; Open flags used by `path_open`.
                     |(typename $oflags
                     |  (flags u16
                     |    ;;; Create file if it does not exist.
                     |    $creat
                     |    ;;; Fail if not a directory.
                     |    $directory
                     |    ;;; Fail if file already exists.
                     |    $excl
                     |    ;;; Truncate file to size 0.
                     |    $trunc
                     |  )
                     |)
                     |
                     |;;; Number of hard links to an inode.
                     |(typename $linkcount u64)
                     |
                     |;;; File attributes.
                     |(typename $filestat
                     |  (struct
                     |    ;;; Device ID of device containing the file.
                     |    (field $dev $device)
                     |    ;;; File serial number.
                     |    (field $ino $inode)
                     |    ;;; File type.
                     |    (field $filetype $filetype)
                     |    ;;; Number of hard links to the file.
                     |    (field $nlink $linkcount)
                     |    ;;; For regular files, the file size in bytes. For symbolic links, the length in bytes of the pathname contained in the symbolic link.
                     |    (field $size $filesize)
                     |    ;;; Last data access timestamp.
                     |    (field $atim $timestamp)
                     |    ;;; Last data modification timestamp.
                     |    (field $mtim $timestamp)
                     |    ;;; Last file status change timestamp.
                     |    (field $ctim $timestamp)
                     |  )
                     |)
                     |
                     |;;; User-provided value that may be attached to objects that is retained when
                     |;;; extracted from the implementation.
                     |(typename $userdata u64)
                     |
                     |;;; Type of a subscription to an event or its occurrence.
                     |(typename $eventtype
                     |  (enum u8
                     |    ;;; The time value of clock `subscription_clock::id` has
                     |    ;;; reached timestamp `subscription_clock::timeout`.
                     |    $clock
                     |    ;;; File descriptor `subscription_fd_readwrite::file_descriptor` has data
                     |    ;;; available for reading. This event always triggers for regular files.
                     |    $fd_read
                     |    ;;; File descriptor `subscription_fd_readwrite::file_descriptor` has capacity
                     |    ;;; available for writing. This event always triggers for regular files.
                     |    $fd_write
                     |  )
                     |)
                     |
                     |;;; The state of the file descriptor subscribed to with
                     |;;; `eventtype::fd_read` or `eventtype::fd_write`.
                     |(typename $eventrwflags
                     |  (flags u16
                     |    ;;; The peer of this socket has closed or disconnected.
                     |    $fd_readwrite_hangup
                     |  )
                     |)
                     |
                     |;;; The contents of an $event when type is `eventtype::fd_read` or
                     |;;; `eventtype::fd_write`.
                     |(typename $event_fd_readwrite
                     |  (struct
                     |    ;;; The number of bytes available for reading or writing.
                     |    (field $nbytes $filesize)
                     |    ;;; The state of the file descriptor.
                     |    (field $flags $eventrwflags)
                     |  )
                     |)
                     |
                     |;;; An event that occurred.
                     |(typename $event
                     |  (struct
                     |    ;;; User-provided value that got attached to `subscription::userdata`.
                     |    (field $userdata $userdata)
                     |    ;;; If non-zero, an error that occurred while processing the subscription request.
                     |    (field $error $errno)
                     |    ;;; The type of event that occured
                     |    (field $type $eventtype)
                     |    ;;; The contents of the event, if it is an `eventtype::fd_read` or
                     |    ;;; `eventtype::fd_write`. `eventtype::clock` events ignore this field.
                     |    (field $fd_readwrite $event_fd_readwrite)
                     |  )
                     |)
                     |
                     |;;; Flags determining how to interpret the timestamp provided in
                     |;;; `subscription_clock::timeout`.
                     |(typename $subclockflags
                     |  (flags u16
                     |    ;;; If set, treat the timestamp provided in
                     |    ;;; `subscription_clock::timeout` as an absolute timestamp of clock
                     |    ;;; `subscription_clock::id`. If clear, treat the timestamp
                     |    ;;; provided in `subscription_clock::timeout` relative to the
                     |    ;;; current time value of clock `subscription_clock::id`.
                     |    $subscription_clock_abstime
                     |  )
                     |)
                     |
                     |;;; The contents of a `subscription` when type is `eventtype::clock`.
                     |(typename $subscription_clock
                     |  (struct
                     |    ;;; The clock against which to compare the timestamp.
                     |    (field $id $clockid)
                     |    ;;; The absolute or relative timestamp.
                     |    (field $timeout $timestamp)
                     |    ;;; The amount of time that the implementation may wait additionally
                     |    ;;; to coalesce with other events.
                     |    (field $precision $timestamp)
                     |    ;;; Flags specifying whether the timeout is absolute or relative
                     |    (field $flags $subclockflags)
                     |  )
                     |)
                     |
                     |;;; The contents of a `subscription` when type is type is
                     |;;; `eventtype::fd_read` or `eventtype::fd_write`.
                     |(typename $subscription_fd_readwrite
                     |  (struct
                     |    ;;; The file descriptor on which to wait for it to become ready for reading or writing.
                     |    (field $file_descriptor $fd)
                     |  )
                     |)
                     |
                     |;;; The contents of a `subscription`.
                     |(typename $subscription_u
                     |  (union $eventtype
                     |    (field $clock $subscription_clock)
                     |    (field $fd_read  $subscription_fd_readwrite)
                     |    (field $fd_write $subscription_fd_readwrite)
                     |  )
                     |)
                     |
                     |;;; Subscription to an event.
                     |(typename $subscription
                     |  (struct
                     |    ;;; User-provided value that is attached to the subscription in the
                     |    ;;; implementation and returned through `event::userdata`.
                     |    (field $userdata $userdata)
                     |    ;;; The type of the event to which to subscribe, and its contents
                     |    (field $u $subscription_u)
                     |  )
                     |)
                     |
                     |;;; Exit code generated by a process when exiting.
                     |(typename $exitcode u32)
                     |
                     |;;; Signal condition.
                     |(typename $signal
                     |  (enum u8
                     |    ;;; No signal. Note that POSIX has special semantics for `kill(pid, 0)`,
                     |    ;;; so this value is reserved.
                     |    $none
                     |    ;;; Hangup.
                     |    ;;; Action: Terminates the process.
                     |    $hup
                     |    ;;; Terminate interrupt signal.
                     |    ;;; Action: Terminates the process.
                     |    $int
                     |    ;;; Terminal quit signal.
                     |    ;;; Action: Terminates the process.
                     |    $quit
                     |    ;;; Illegal instruction.
                     |    ;;; Action: Terminates the process.
                     |    $ill
                     |    ;;; Trace/breakpoint trap.
                     |    ;;; Action: Terminates the process.
                     |    $trap
                     |    ;;; Process abort signal.
                     |    ;;; Action: Terminates the process.
                     |    $abrt
                     |    ;;; Access to an undefined portion of a memory object.
                     |    ;;; Action: Terminates the process.
                     |    $bus
                     |    ;;; Erroneous arithmetic operation.
                     |    ;;; Action: Terminates the process.
                     |    $fpe
                     |    ;;; Kill.
                     |    ;;; Action: Terminates the process.
                     |    $kill
                     |    ;;; User-defined signal 1.
                     |    ;;; Action: Terminates the process.
                     |    $usr1
                     |    ;;; Invalid memory reference.
                     |    ;;; Action: Terminates the process.
                     |    $segv
                     |    ;;; User-defined signal 2.
                     |    ;;; Action: Terminates the process.
                     |    $usr2
                     |    ;;; Write on a pipe with no one to read it.
                     |    ;;; Action: Ignored.
                     |    $pipe
                     |    ;;; Alarm clock.
                     |    ;;; Action: Terminates the process.
                     |    $alrm
                     |    ;;; Termination signal.
                     |    ;;; Action: Terminates the process.
                     |    $term
                     |    ;;; Child process terminated, stopped, or continued.
                     |    ;;; Action: Ignored.
                     |    $chld
                     |    ;;; Continue executing, if stopped.
                     |    ;;; Action: Continues executing, if stopped.
                     |    $cont
                     |    ;;; Stop executing.
                     |    ;;; Action: Stops executing.
                     |    $stop
                     |    ;;; Terminal stop signal.
                     |    ;;; Action: Stops executing.
                     |    $tstp
                     |    ;;; Background process attempting read.
                     |    ;;; Action: Stops executing.
                     |    $ttin
                     |    ;;; Background process attempting write.
                     |    ;;; Action: Stops executing.
                     |    $ttou
                     |    ;;; High bandwidth data is available at a socket.
                     |    ;;; Action: Ignored.
                     |    $urg
                     |    ;;; CPU time limit exceeded.
                     |    ;;; Action: Terminates the process.
                     |    $xcpu
                     |    ;;; File size limit exceeded.
                     |    ;;; Action: Terminates the process.
                     |    $xfsz
                     |    ;;; Virtual timer expired.
                     |    ;;; Action: Terminates the process.
                     |    $vtalrm
                     |    ;;; Profiling timer expired.
                     |    ;;; Action: Terminates the process.
                     |    $prof
                     |    ;;; Window changed.
                     |    ;;; Action: Ignored.
                     |    $winch
                     |    ;;; I/O possible.
                     |    ;;; Action: Terminates the process.
                     |    $poll
                     |    ;;; Power failure.
                     |    ;;; Action: Terminates the process.
                     |    $pwr
                     |    ;;; Bad system call.
                     |    ;;; Action: Terminates the process.
                     |    $sys
                     |  )
                     |)
                     |
                     |;;; Flags provided to `sock_recv`.
                     |(typename $riflags
                     |  (flags u16
                     |    ;;; Returns the message without removing it from the socket's receive queue.
                     |    $recv_peek
                     |    ;;; On byte-stream sockets, block until the full amount of data can be returned.
                     |    $recv_waitall
                     |  )
                     |)
                     |
                     |;;; Flags returned by `sock_recv`.
                     |(typename $roflags
                     |  (flags u16
                     |    ;;; Returned by `sock_recv`: Message data has been truncated.
                     |    $recv_data_truncated
                     |  )
                     |)
                     |
                     |;;; Flags provided to `sock_send`. As there are currently no flags
                     |;;; defined, it must be set to zero.
                     |(typename $siflags u16)
                     |
                     |;;; Which channels on a socket to shut down.
                     |(typename $sdflags
                     |  (flags u8
                     |    ;;; Disables further receive operations.
                     |    $rd
                     |    ;;; Disables further send operations.
                     |    $wr
                     |  )
                     |)
                     |
                     |;;; Identifiers for preopened capabilities.
                     |(typename $preopentype
                     |  (enum u8
                     |    ;;; A pre-opened directory.
                     |    $dir
                     |  )
                     |)
                     |
                     |;;; The contents of a $prestat when type is `preopentype::dir`.
                     |(typename $prestat_dir
                     |  (struct
                     |    ;;; The length of the directory name for use with `fd_prestat_dir_name`.
                     |    (field $pr_name_len $size)
                     |  )
                     |)
                     |
                     |;;; Information about a pre-opened capability.
                     |(typename $prestat
                     |  (union $preopentype
                     |    (field $dir $prestat_dir)
                     |  )
                     |)
                     |""".stripMargin
      val t = parse(types, TypesParser.file(_)).get.value

      t.keys.foreach(i => {
        println(s"$i ${t(i)}")
      })
    }

    test("console_tracer") {

      val funcs =
        """;; WASI Preview. This is an evolution of the API that WASI initially
          |;; launched with.
          |;;
          |;; Some content here is derived from [CloudABI](https://github.com/NuxiNL/cloudabi).
          |;;
          |;; This is a `witx` file. See [here](https://github.com/WebAssembly/WASI/tree/master/docs/witx.md)
          |;; for an explanation of what that means.
          |
          |(use "typenames.witx")
          |
          |(module $wasi_snapshot_preview1
          |  ;;; Linear memory to be accessed by WASI functions that need it.
          |  (import "memory" (memory))
          |
          |  ;;; Read command-line argument data.
          |  ;;; The size of the array should match that returned by `args_sizes_get`
          |  (@interface func (export "args_get")
          |    (param $argv (@witx pointer (@witx pointer u8)))
          |    (param $argv_buf (@witx pointer u8))
          |    (result $error $errno)
          |  )
          |  ;;; Return command-line argument data sizes.
          |  (@interface func (export "args_sizes_get")
          |    (result $error $errno)
          |    ;;; The number of arguments.
          |    (result $argc $size)
          |    ;;; The size of the argument string data.
          |    (result $argv_buf_size $size)
          |  )
          |
          |  ;;; Read environment variable data.
          |  ;;; The sizes of the buffers should match that returned by `environ_sizes_get`.
          |  (@interface func (export "environ_get")
          |    (param $environ (@witx pointer (@witx pointer u8)))
          |    (param $environ_buf (@witx pointer u8))
          |    (result $error $errno)
          |  )
          |  ;;; Return environment variable data sizes.
          |  (@interface func (export "environ_sizes_get")
          |    (result $error $errno)
          |    ;;; The number of environment variable arguments.
          |    (result $environc $size)
          |    ;;; The size of the environment variable data.
          |    (result $environ_buf_size $size)
          |  )
          |
          |  ;;; Return the resolution of a clock.
          |  ;;; Implementations are required to provide a non-zero value for supported clocks. For unsupported clocks,
          |  ;;; return `errno::inval`.
          |  ;;; Note: This is similar to `clock_getres` in POSIX.
          |  (@interface func (export "clock_res_get")
          |    ;;; The clock for which to return the resolution.
          |    (param $id $clockid)
          |    (result $error $errno)
          |    ;;; The resolution of the clock.
          |    (result $resolution $timestamp)
          |  )
          |  ;;; Return the time value of a clock.
          |  ;;; Note: This is similar to `clock_gettime` in POSIX.
          |  (@interface func (export "clock_time_get")
          |    ;;; The clock for which to return the time.
          |    (param $id $clockid)
          |    ;;; The maximum lag (exclusive) that the returned time value may have, compared to its actual value.
          |    (param $precision $timestamp)
          |    (result $error $errno)
          |    ;;; The time value of the clock.
          |    (result $time $timestamp)
          |  )
          |
          |  ;;; Provide file advisory information on a file descriptor.
          |  ;;; Note: This is similar to `posix_fadvise` in POSIX.
          |  (@interface func (export "fd_advise")
          |    (param $fd $fd)
          |    ;;; The offset within the file to which the advisory applies.
          |    (param $offset $filesize)
          |    ;;; The length of the region to which the advisory applies.
          |    (param $len $filesize)
          |    ;;; The advice.
          |    (param $advice $advice)
          |    (result $error $errno)
          |  )
          |
          |  ;;; Force the allocation of space in a file.
          |  ;;; Note: This is similar to `posix_fallocate` in POSIX.
          |  (@interface func (export "fd_allocate")
          |    (param $fd $fd)
          |    ;;; The offset at which to start the allocation.
          |    (param $offset $filesize)
          |    ;;; The length of the area that is allocated.
          |    (param $len $filesize)
          |    (result $error $errno)
          |  )
          |
          |  ;;; Close a file descriptor.
          |  ;;; Note: This is similar to `close` in POSIX.
          |  (@interface func (export "fd_close")
          |    (param $fd $fd)
          |    (result $error $errno)
          |  )
          |
          |  ;;; Synchronize the data of a file to disk.
          |  ;;; Note: This is similar to `fdatasync` in POSIX.
          |  (@interface func (export "fd_datasync")
          |    (param $fd $fd)
          |    (result $error $errno)
          |  )
          |
          |  ;;; Get the attributes of a file descriptor.
          |  ;;; Note: This returns similar flags to `fsync(fd, F_GETFL)` in POSIX, as well as additional fields.
          |  (@interface func (export "fd_fdstat_get")
          |    (param $fd $fd)
          |    (result $error $errno)
          |    ;;; The buffer where the file descriptor's attributes are stored.
          |    (result $stat $fdstat)
          |  )
          |
          |  ;;; Adjust the flags associated with a file descriptor.
          |  ;;; Note: This is similar to `fcntl(fd, F_SETFL, flags)` in POSIX.
          |  (@interface func (export "fd_fdstat_set_flags")
          |    (param $fd $fd)
          |    ;;; The desired values of the file descriptor flags.
          |    (param $flags $fdflags)
          |    (result $error $errno)
          |  )
          |
          |  ;;; Adjust the rights associated with a file descriptor.
          |  ;;; This can only be used to remove rights, and returns `errno::notcapable` if called in a way that would attempt to add rights
          |  (@interface func (export "fd_fdstat_set_rights")
          |    (param $fd $fd)
          |    ;;; The desired rights of the file descriptor.
          |    (param $fs_rights_base $rights)
          |    (param $fs_rights_inheriting $rights)
          |    (result $error $errno)
          |  )
          |
          |  ;;; Return the attributes of an open file.
          |  (@interface func (export "fd_filestat_get")
          |    (param $fd $fd)
          |    (result $error $errno)
          |    ;;; The buffer where the file's attributes are stored.
          |    (result $buf $filestat)
          |  )
          |
          |  ;;; Adjust the size of an open file. If this increases the file's size, the extra bytes are filled with zeros.
          |  ;;; Note: This is similar to `ftruncate` in POSIX.
          |  (@interface func (export "fd_filestat_set_size")
          |    (param $fd $fd)
          |    ;;; The desired file size.
          |    (param $size $filesize)
          |    (result $error $errno)
          |  )
          |
          |  ;;; Adjust the timestamps of an open file or directory.
          |  ;;; Note: This is similar to `futimens` in POSIX.
          |  (@interface func (export "fd_filestat_set_times")
          |    (param $fd $fd)
          |    ;;; The desired values of the data access timestamp.
          |    (param $atim $timestamp)
          |    ;;; The desired values of the data modification timestamp.
          |    (param $mtim $timestamp)
          |    ;;; A bitmask indicating which timestamps to adjust.
          |    (param $fst_flags $fstflags)
          |    (result $error $errno)
          |  )
          |
          |  ;;; Read from a file descriptor, without using and updating the file descriptor's offset.
          |  ;;; Note: This is similar to `preadv` in POSIX.
          |  (@interface func (export "fd_pread")
          |    (param $fd $fd)
          |    ;;; List of scatter/gather vectors in which to store data.
          |    (param $iovs $iovec_array)
          |    ;;; The offset within the file at which to read.
          |    (param $offset $filesize)
          |    (result $error $errno)
          |    ;;; The number of bytes read.
          |    (result $nread $size)
          |  )
          |
          |  ;;; Return a description of the given preopened file descriptor.
          |  (@interface func (export "fd_prestat_get")
          |    (param $fd $fd)
          |    (result $error $errno)
          |    ;;; The buffer where the description is stored.
          |    (result $buf $prestat)
          |  )
          |
          |  ;;; Return a description of the given preopened file descriptor.
          |  (@interface func (export "fd_prestat_dir_name")
          |    (param $fd $fd)
          |    ;;; A buffer into which to write the preopened directory name.
          |    (param $path (@witx pointer u8))
          |    (param $path_len $size)
          |    (result $error $errno)
          |  )
          |
          |  ;;; Write to a file descriptor, without using and updating the file descriptor's offset.
          |  ;;; Note: This is similar to `pwritev` in POSIX.
          |  (@interface func (export "fd_pwrite")
          |    (param $fd $fd)
          |    ;;; List of scatter/gather vectors from which to retrieve data.
          |    (param $iovs $ciovec_array)
          |    ;;; The offset within the file at which to write.
          |    (param $offset $filesize)
          |    (result $error $errno)
          |    ;;; The number of bytes written.
          |    (result $nwritten $size)
          |  )
          |
          |  ;;; Read from a file descriptor.
          |  ;;; Note: This is similar to `readv` in POSIX.
          |  (@interface func (export "fd_read")
          |    (param $fd $fd)
          |    ;;; List of scatter/gather vectors to which to store data.
          |    (param $iovs $iovec_array)
          |    (result $error $errno)
          |    ;;; The number of bytes read.
          |    (result $nread $size)
          |  )
          |
          |  ;;; Read directory entries from a directory.
          |  ;;; When successful, the contents of the output buffer consist of a sequence of
          |  ;;; directory entries. Each directory entry consists of a dirent_t object,
          |  ;;; followed by dirent_t::d_namlen bytes holding the name of the directory
          |  ;;; entry.
          |  ;;
          |  ;;; This function fills the output buffer as much as possible, potentially
          |  ;;; truncating the last directory entry. This allows the caller to grow its
          |  ;;; read buffer size in case it's too small to fit a single large directory
          |  ;;; entry, or skip the oversized directory entry.
          |  (@interface func (export "fd_readdir")
          |    (param $fd $fd)
          |    ;;; The buffer where directory entries are stored
          |    (param $buf (@witx pointer u8))
          |    (param $buf_len $size)
          |    ;;; The location within the directory to start reading
          |    (param $cookie $dircookie)
          |    (result $error $errno)
          |    ;;; The number of bytes stored in the read buffer. If less than the size of the read buffer, the end of the directory has been reached.
          |    (result $bufused $size)
          |  )
          |
          |  ;;; Atomically replace a file descriptor by renumbering another file descriptor.
          |  ;;
          |  ;;; Due to the strong focus on thread safety, this environment does not provide
          |  ;;; a mechanism to duplicate or renumber a file descriptor to an arbitrary
          |  ;;; number, like `dup2()`. This would be prone to race conditions, as an actual
          |  ;;; file descriptor with the same number could be allocated by a different
          |  ;;; thread at the same time.
          |  ;;
          |  ;;; This function provides a way to atomically renumber file descriptors, which
          |  ;;; would disappear if `dup2()` were to be removed entirely.
          |  (@interface func (export "fd_renumber")
          |    (param $fd $fd)
          |    ;;; The file descriptor to overwrite.
          |    (param $to $fd)
          |    (result $error $errno)
          |  )
          |
          |  ;;; Move the offset of a file descriptor.
          |  ;;; Note: This is similar to `lseek` in POSIX.
          |  (@interface func (export "fd_seek")
          |    (param $fd $fd)
          |    ;;; The number of bytes to move.
          |    (param $offset $filedelta)
          |    ;;; The base from which the offset is relative.
          |    (param $whence $whence)
          |    (result $error $errno)
          |    ;;; The new offset of the file descriptor, relative to the start of the file.
          |    (result $newoffset $filesize)
          |  )
          |
          |  ;;; Synchronize the data and metadata of a file to disk.
          |  ;;; Note: This is similar to `fsync` in POSIX.
          |  (@interface func (export "fd_sync")
          |    (param $fd $fd)
          |    (result $error $errno)
          |  )
          |
          |  ;;; Return the current offset of a file descriptor.
          |  ;;; Note: This is similar to `lseek(fd, 0, SEEK_CUR)` in POSIX.
          |  (@interface func (export "fd_tell")
          |    (param $fd $fd)
          |    (result $error $errno)
          |    ;;; The current offset of the file descriptor, relative to the start of the file.
          |    (result $offset $filesize)
          |  )
          |
          |  ;;; Write to a file descriptor.
          |  ;;; Note: This is similar to `writev` in POSIX.
          |  (@interface func (export "fd_write")
          |    (param $fd $fd)
          |    ;;; List of scatter/gather vectors from which to retrieve data.
          |    (param $iovs $ciovec_array)
          |    (result $error $errno)
          |    ;;; The number of bytes written.
          |    (result $nwritten $size)
          |  )
          |
          |  ;;; Create a directory.
          |  ;;; Note: This is similar to `mkdirat` in POSIX.
          |  (@interface func (export "path_create_directory")
          |    (param $fd $fd)
          |    ;;; The path at which to create the directory.
          |    (param $path string)
          |    (result $error $errno)
          |  )
          |
          |  ;;; Return the attributes of a file or directory.
          |  ;;; Note: This is similar to `stat` in POSIX.
          |  (@interface func (export "path_filestat_get")
          |    (param $fd $fd)
          |    ;;; Flags determining the method of how the path is resolved.
          |    (param $flags $lookupflags)
          |    ;;; The path of the file or directory to inspect.
          |    (param $path string)
          |    (result $error $errno)
          |    ;;; The buffer where the file's attributes are stored.
          |    (result $buf $filestat)
          |  )
          |
          |  ;;; Adjust the timestamps of a file or directory.
          |  ;;; Note: This is similar to `utimensat` in POSIX.
          |  (@interface func (export "path_filestat_set_times")
          |    (param $fd $fd)
          |    ;;; Flags determining the method of how the path is resolved.
          |    (param $flags $lookupflags)
          |    ;;; The path of the file or directory to operate on.
          |    (param $path string)
          |    ;;; The desired values of the data access timestamp.
          |    (param $atim $timestamp)
          |    ;;; The desired values of the data modification timestamp.
          |    (param $mtim $timestamp)
          |    ;;; A bitmask indicating which timestamps to adjust.
          |    (param $fst_flags $fstflags)
          |    (result $error $errno)
          |  )
          |
          |  ;;; Create a hard link.
          |  ;;; Note: This is similar to `linkat` in POSIX.
          |  (@interface func (export "path_link")
          |    (param $old_fd $fd)
          |    ;;; Flags determining the method of how the path is resolved.
          |    (param $old_flags $lookupflags)
          |    ;;; The source path from which to link.
          |    (param $old_path string)
          |    ;;; The working directory at which the resolution of the new path starts.
          |    (param $new_fd $fd)
          |    ;;; The destination path at which to create the hard link.
          |    (param $new_path string)
          |    (result $error $errno)
          |  )
          |
          |  ;;; Open a file or directory.
          |  ;;
          |  ;;; The returned file descriptor is not guaranteed to be the lowest-numbered
          |  ;;; file descriptor not currently open; it is randomized to prevent
          |  ;;; applications from depending on making assumptions about indexes, since this
          |  ;;; is error-prone in multi-threaded contexts. The returned file descriptor is
          |  ;;; guaranteed to be less than 2**31.
          |  ;;
          |  ;;; Note: This is similar to `openat` in POSIX.
          |  (@interface func (export "path_open")
          |    (param $fd $fd)
          |    ;;; Flags determining the method of how the path is resolved.
          |    (param $dirflags $lookupflags)
          |    ;;; The relative path of the file or directory to open, relative to the
          |    ;;; `path_open::fd` directory.
          |    (param $path string)
          |    ;;; The method by which to open the file.
          |    (param $oflags $oflags)
          |    ;;; The initial rights of the newly created file descriptor. The
          |    ;;; implementation is allowed to return a file descriptor with fewer rights
          |    ;;; than specified, if and only if those rights do not apply to the type of
          |    ;;; file being opened.
          |    ;;
          |    ;;; The *base* rights are rights that will apply to operations using the file
          |    ;;; descriptor itself, while the *inheriting* rights are rights that apply to
          |    ;;; file descriptors derived from it.
          |    (param $fs_rights_base $rights)
          |    (param $fs_rights_inherting $rights)
          |    (param $fdflags $fdflags)
          |    (result $error $errno)
          |    ;;; The file descriptor of the file that has been opened.
          |    (result $opened_fd $fd)
          |  )
          |
          |  ;;; Read the contents of a symbolic link.
          |  ;;; Note: This is similar to `readlinkat` in POSIX.
          |  (@interface func (export "path_readlink")
          |    (param $fd $fd)
          |    ;;; The path of the symbolic link from which to read.
          |    (param $path string)
          |    ;;; The buffer to which to write the contents of the symbolic link.
          |    (param $buf (@witx pointer u8))
          |    (param $buf_len $size)
          |    (result $error $errno)
          |    ;;; The number of bytes placed in the buffer.
          |    (result $bufused $size)
          |  )
          |
          |  ;;; Remove a directory.
          |  ;;; Return `errno::notempty` if the directory is not empty.
          |  ;;; Note: This is similar to `unlinkat(fd, path, AT_REMOVEDIR)` in POSIX.
          |  (@interface func (export "path_remove_directory")
          |    (param $fd $fd)
          |    ;;; The path to a directory to remove.
          |    (param $path string)
          |    (result $error $errno)
          |  )
          |
          |  ;;; Rename a file or directory.
          |  ;;; Note: This is similar to `renameat` in POSIX.
          |  (@interface func (export "path_rename")
          |    (param $fd $fd)
          |    ;;; The source path of the file or directory to rename.
          |    (param $old_path string)
          |    ;;; The working directory at which the resolution of the new path starts.
          |    (param $new_fd $fd)
          |    ;;; The destination path to which to rename the file or directory.
          |    (param $new_path string)
          |    (result $error $errno)
          |  )
          |
          |  ;;; Create a symbolic link.
          |  ;;; Note: This is similar to `symlinkat` in POSIX.
          |  (@interface func (export "path_symlink")
          |    ;;; The contents of the symbolic link.
          |    (param $old_path string)
          |    (param $fd $fd)
          |    ;;; The destination path at which to create the symbolic link.
          |    (param $new_path string)
          |    (result $error $errno)
          |  )
          |
          |
          |  ;;; Unlink a file.
          |  ;;; Return `errno::isdir` if the path refers to a directory.
          |  ;;; Note: This is similar to `unlinkat(fd, path, 0)` in POSIX.
          |  (@interface func (export "path_unlink_file")
          |    (param $fd $fd)
          |    ;;; The path to a file to unlink.
          |    (param $path string)
          |    (result $error $errno)
          |  )
          |
          |  ;;; Concurrently poll for the occurrence of a set of events.
          |  (@interface func (export "poll_oneoff")
          |    ;;; The events to which to subscribe.
          |    (param $in (@witx const_pointer $subscription))
          |    ;;; The events that have occurred.
          |    (param $out (@witx pointer $event))
          |    ;;; Both the number of subscriptions and events.
          |    (param $nsubscriptions $size)
          |    (result $error $errno)
          |    ;;; The number of events stored.
          |    (result $nevents $size)
          |  )
          |
          |  ;;; Terminate the process normally. An exit code of 0 indicates successful
          |  ;;; termination of the program. The meanings of other values is dependent on
          |  ;;; the environment.
          |  (@interface func (export "proc_exit")
          |    ;;; The exit code returned by the process.
          |    (param $rval $exitcode)
          |  )
          |
          |  ;;; Send a signal to the process of the calling thread.
          |  ;;; Note: This is similar to `raise` in POSIX.
          |  (@interface func (export "proc_raise")
          |    ;;; The signal condition to trigger.
          |    (param $sig $signal)
          |    (result $error $errno)
          |  )
          |
          |  ;;; Temporarily yield execution of the calling thread.
          |  ;;; Note: This is similar to `sched_yield` in POSIX.
          |  (@interface func (export "sched_yield")
          |    (result $error $errno)
          |  )
          |
          |  ;;; Write high-quality random data into a buffer.
          |  ;;; This function blocks when the implementation is unable to immediately
          |  ;;; provide sufficient high-quality random data.
          |  ;;; This function may execute slowly, so when large mounts of random data are
          |  ;;; required, it's advisable to use this function to seed a pseudo-random
          |  ;;; number generator, rather than to provide the random data directly.
          |  (@interface func (export "random_get")
          |    ;;; The buffer to fill with random data.
          |    (param $buf (@witx pointer u8))
          |    (param $buf_len $size)
          |    (result $error $errno)
          |  )
          |
          |  ;;; Receive a message from a socket.
          |  ;;; Note: This is similar to `recv` in POSIX, though it also supports reading
          |  ;;; the data into multiple buffers in the manner of `readv`.
          |  (@interface func (export "sock_recv")
          |    (param $fd $fd)
          |    ;;; List of scatter/gather vectors to which to store data.
          |    (param $ri_data $iovec_array)
          |    ;;; Message flags.
          |    (param $ri_flags $riflags)
          |    (result $error $errno)
          |    ;;; Number of bytes stored in ri_data.
          |    (result $ro_datalen $size)
          |    ;;; Message flags.
          |    (result $ro_flags $roflags)
          |  )
          |
          |  ;;; Send a message on a socket.
          |  ;;; Note: This is similar to `send` in POSIX, though it also supports writing
          |  ;;; the data from multiple buffers in the manner of `writev`.
          |  (@interface func (export "sock_send")
          |    (param $fd $fd)
          |    ;;; List of scatter/gather vectors to which to retrieve data
          |    (param $si_data $ciovec_array)
          |    ;;; Message flags.
          |    (param $si_flags $siflags)
          |    (result $error $errno)
          |    ;;; Number of bytes transmitted.
          |    (result $so_datalen $size)
          |  )
          |
          |  ;;; Shut down socket send and receive channels.
          |  ;;; Note: This is similar to `shutdown` in POSIX.
          |  (@interface func (export "sock_shutdown")
          |    (param $fd $fd)
          |    ;;; Which channels on the socket to shut down.
          |    (param $how $sdflags)
          |    (result $error $errno)
          |  )
          |)""".stripMargin

      val t = parse(funcs, ModuleParser.file(_)).get.value

      println(t)
    }

  }

}
