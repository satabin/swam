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
}
