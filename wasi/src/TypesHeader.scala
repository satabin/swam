package swam
package wasi

import swam.wasi.Types.rightsFlags

/**
  * @author Javier Cabrera-Arteaga on 2020-04-13
  */
object TypesHeader {
  val RIGHTS_ALL: Long = rightsFlags.fd_datasync.id |
    rightsFlags.fd_read.id |
    rightsFlags.fd_seek.id |
    rightsFlags.fd_fdstat_set_flags.id |
    rightsFlags.fd_sync.id |
    rightsFlags.fd_tell.id |
    rightsFlags.fd_write.id |
    rightsFlags.fd_advise.id |
    rightsFlags.fd_allocate.id |
    rightsFlags.path_create_directory.id |
    rightsFlags.path_create_file.id |
    rightsFlags.path_link_source.id |
    rightsFlags.path_link_target.id |
    rightsFlags.path_open.id |
    rightsFlags.fd_readdir.id |
    rightsFlags.path_readlink.id |
    rightsFlags.path_rename_source.id |
    rightsFlags.path_rename_target.id |
    rightsFlags.path_filestat_get.id |
    rightsFlags.path_filestat_set_size.id |
    rightsFlags.path_filestat_set_times.id |
    rightsFlags.fd_filestat_get.id |
    rightsFlags.fd_filestat_set_times.id |
    rightsFlags.fd_filestat_set_size.id |
    rightsFlags.path_symlink.id |
    rightsFlags.path_unlink_file.id |
    rightsFlags.path_remove_directory.id |
    rightsFlags.poll_fd_readwrite.id |
    rightsFlags.sock_shutdown.id
  val RIGHTS_BLOCK_DEVICE_BASE: Long = RIGHTS_ALL
  val RIGHTS_BLOCK_DEVICE_INHERITING: Long = RIGHTS_ALL
  val RIGHTS_CHARACTER_DEVICE_BASE: Long = RIGHTS_ALL
  val RIGHTS_CHARACTER_DEVICE_INHERITING: Long = RIGHTS_ALL
  val RIGHTS_REGULAR_FILE_BASE: Long = rightsFlags.fd_datasync.id |
    rightsFlags.fd_read.id |
    rightsFlags.fd_seek.id |
    rightsFlags.fd_fdstat_set_flags.id |
    rightsFlags.fd_sync.id |
    rightsFlags.fd_tell.id |
    rightsFlags.fd_write.id |
    rightsFlags.fd_advise.id |
    rightsFlags.fd_allocate.id |
    rightsFlags.fd_filestat_get.id |
    rightsFlags.fd_filestat_set_size.id |
    rightsFlags.fd_filestat_set_times.id |
    rightsFlags.poll_fd_readwrite.id
  val RIGHTS_REGULAR_FILE_INHERITING: Long = 0
  val RIGHTS_DIRECTORY_BASE: Long = rightsFlags.fd_fdstat_set_flags.id |
    rightsFlags.fd_sync.id |
    rightsFlags.fd_advise.id |
    rightsFlags.path_create_directory.id |
    rightsFlags.path_create_file.id |
    rightsFlags.path_link_source.id |
    rightsFlags.path_link_target.id |
    rightsFlags.path_open.id |
    rightsFlags.fd_readdir.id |
    rightsFlags.path_readlink.id |
    rightsFlags.path_rename_source.id |
    rightsFlags.path_rename_target.id |
    rightsFlags.path_filestat_get.id |
    rightsFlags.path_filestat_set_size.id |
    rightsFlags.path_filestat_set_times.id |
    rightsFlags.fd_filestat_get.id |
    rightsFlags.fd_filestat_set_times.id |
    rightsFlags.path_symlink.id |
    rightsFlags.path_unlink_file.id |
    rightsFlags.path_remove_directory.id |
    rightsFlags.poll_fd_readwrite.id
  val RIGHTS_DIRECTORY_INHERITING: Long = RIGHTS_DIRECTORY_BASE | RIGHTS_REGULAR_FILE_BASE
  val RIGHTS_SOCKET_BASE: Long = rightsFlags.fd_read.id |
    rightsFlags.fd_fdstat_set_flags.id |
    rightsFlags.fd_write.id |
    rightsFlags.fd_filestat_get.id |
    rightsFlags.poll_fd_readwrite.id |
    rightsFlags.sock_shutdown.id
  val RIGHTS_SOCKET_INHERITING: Long = RIGHTS_ALL
  val RIGHTS_TTY_BASE: Long = rightsFlags.fd_read.id |
    rightsFlags.fd_fdstat_set_flags.id |
    rightsFlags.fd_write.id |
    rightsFlags.fd_filestat_get.id |
    rightsFlags.poll_fd_readwrite.id

  val RIGHTS_TTY_INHERITING = 0L

  val STDIN_DEFAULT_RIGHTS: Long = rightsFlags.fd_datasync.id |
    rightsFlags.fd_read.id |
    rightsFlags.fd_sync.id |
    rightsFlags.fd_advise.id |
    rightsFlags.fd_filestat_get.id |
    rightsFlags.poll_fd_readwrite.id
  val STDOUT_DEFAULT_RIGHTS: Long = rightsFlags.fd_datasync.id |
    rightsFlags.fd_write.id |
    rightsFlags.fd_sync.id |
    rightsFlags.fd_advise.id |
    rightsFlags.fd_filestat_get.id |
    rightsFlags.poll_fd_readwrite.id
  val STDERR_DEFAULT_RIGHTS: Long = STDOUT_DEFAULT_RIGHTS

  val WASI_PREOPENTYPE_DIR = 0
  val WASI_DIRCOOKIE_START = 0

  val O_RDWR = 2
  val O_RDONLY = 1
  val O_WRONLY = 0
  val O_CREAT = 512
  val O_DIRECTORY = 1048576
  val O_EXCL = 2048
  val O_TRUNC = 1024
  val O_APPEND = 8
  val O_DSYNC = 4194304
  val O_SYNC = 128
  val O_NON_BLOCK = 4
  val O_RSYNC = 16

  val WASI_O_CREAT = 1
  val WASI_O_DIRECTORY = 2
  val WASI_O_EXCL = 4
  val WASI_O_TRUNC = 8

  val WASI_FD_FLAG_APPEND = 1
  val WASI_FD_FLAG_DSYNC = 2
  val WASI_FD_FLAG_NONBLOCK = 4
  val WASI_FD_RSYNC = 8
  val WASI_FD_SYNC = 16

}
