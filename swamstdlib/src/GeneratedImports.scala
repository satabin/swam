import cats.effect.IO
import swam.runtime.imports.{AsInstance, AsInterface, Imports, TCMap}
import swam.runtime.formats._
import swam.runtime.formats.DefaultFormatters._

trait GeneratedImports {
  type AsIIO[T] = AsInterface[T, IO]
  type AsIsIO[T] = AsInstance[T, IO]

  /*

FCLOSE(3)		 BSD Library Functions Manual		     FCLOSE(3)

NAME
     fclose, fcloseall -- close a stream

LIBRARY
     Standard C Library (libc, -lc)

SYNOPSIS
     #include &lt;stdio.h&gt;

     int
     fclose(FILE *stream);

     void
     fcloseall(void);

DESCRIPTION
     The fclose() function dissociates the named stream from its underlying
     file or set of functions.	If the stream was being used for output, any
     buffered data is written first, using fflush(3).

     The fcloseall() function calls fclose() on all open streams.

RETURN VALUES
     Upon successful completion 0 is returned.	Otherwise, EOF is returned and
     the global variable errno is set to indicate the error.  In either case
     no further access to the stream is possible.

ERRORS
     The fclose() function may also fail and set errno for any of the errors
     specified for the routines close(2) or fflush(3).

NOTES
     The fclose() function does not handle NULL arguments; they will result in
     a segmentation violation.	This is intentional - it makes it easier to
     make sure programs written under FreeBSD are bug free.  This behaviour is
     an implementation detail, and programs should not rely upon it.

SEE ALSO
     close(2), fflush(3), fopen(3), setbuf(3)

STANDARDS
     The fclose() function conforms to ISO/IEC 9899:1990 (``ISO C90'').

     The fcloseall() function first appeared in FreeBSD 7.0.

BSD				April 22, 2006				   BSD

   */
  def envFclose(p0: Int): IO[Int]
  /*

FREAD(3)		 BSD Library Functions Manual		      FREAD(3)

NAME
     fread, fwrite -- binary stream input/output

LIBRARY
     Standard C Library (libc, -lc)

SYNOPSIS
     #include &lt;stdio.h&gt;

     size_t
     fread(void *restrict ptr, size_t size, size_t nitems,
	 FILE *restrict stream);

     size_t
     fwrite(const void *restrict ptr, size_t size, size_t nitems,
	 FILE *restrict stream);

DESCRIPTION
     The function fread() reads nitems objects, each size bytes long, from the
     stream pointed to by stream, storing them at the location given by ptr.

     The function fwrite() writes nitems objects, each size bytes long, to the
     stream pointed to by stream, obtaining them from the location given by
     ptr.

RETURN VALUES
     The functions fread() and fwrite() advance the file position indicator
     for the stream by the number of bytes read or written.  They return the
     number of objects read or written.  If an error occurs, or the end-of-
     file is reached, the return value is a short object count (or zero).

     The function fread() does not distinguish between end-of-file and error;
     callers must use feof(3) and ferror(3) to determine which occurred.  The
     function fwrite() returns a value less than nitems only if a write error
     has occurred.

SEE ALSO
     read(2), write(2)

STANDARDS
     The functions fread() and fwrite() conform to ISO/IEC 9899:1990
     (``ISO C90'').

BSD				 March 8, 1994				   BSD

   */
  def envFread(p0: Int, p1: Int, p2: Int, p3: Int): IO[Int]
  /*

STRTOUL(3)		 BSD Library Functions Manual		    STRTOUL(3)

NAME
     strtoul, strtoull, strtoumax, strtouq -- convert a string to an unsigned
     long, unsigned long long, uintmax_t, or u_quad_t integer

LIBRARY
     Standard C Library (libc, -lc)

SYNOPSIS
     #include &lt;stdlib.h&gt;

     unsigned long
     strtoul(const char *restrict str, char **restrict endptr, int base);

     unsigned long long
     strtoull(const char *restrict str, char **restrict endptr, int base);

     #include &lt;inttypes.h&gt;

     uintmax_t
     strtoumax(const char *restrict str, char **restrict endptr, int base);

     #include &lt;sys/types.h&gt;
     #include &lt;stdlib.h&gt;
     #include &lt;limits.h&gt;

     u_quad_t
     strtouq(const char *str, char **endptr, int base);

DESCRIPTION
     The strtoul() function converts the string in str to an unsigned long
     value.  The strtoull() function converts the string in str to an unsigned
     long long value.  The strtoumax() function converts the string in str to
     an uintmax_t value.  The strtouq() function converts the string in str to
     a u_quad_t value.	The conversion is done according to the given base,
     which must be between 2 and 36 inclusive, or be the special value 0.

     The string may begin with an arbitrary amount of white space (as deter-
     mined by isspace(3)) followed by a single optional `+' or `-' sign.  If
     base is zero or 16, the string may then include a ``0x'' prefix, and the
     number will be read in base 16; otherwise, a zero base is taken as 10
     (decimal) unless the next character is `0', in which case it is taken as
     8 (octal).

     The remainder of the string is converted to an unsigned long value in the
     obvious manner, stopping at the end of the string or at the first charac-
     ter that does not produce a valid digit in the given base.  (In bases
     above 10, the letter `A' in either upper or lower case represents 10, `B'
     represents 11, and so forth, with `Z' representing 35.)

     If endptr is not NULL, strtoul() stores the address of the first invalid
     character in *endptr.  If there were no digits at all, however, strtoul()
     stores the original value of str in *endptr.  (Thus, if *str is not `\0'
     but **endptr is `\0' on return, the entire string was valid.)

RETURN VALUES
     The strtoul(), strtoull(), strtoumax() and strtouq() functions return
     either the result of the conversion or, if there was a leading minus
     sign, the negation of the result of the conversion, unless the original
     (non-negated) value would overflow; in the latter case, strtoul() returns
     ULONG_MAX, strtoull() returns ULLONG_MAX, strtoumax() returns
     UINTMAX_MAX, and strtouq() returns ULLONG_MAX.  In all cases, errno is
     set to ERANGE.  If no conversion could be performed, 0 is returned and
     the global variable errno is set to EINVAL (the last feature is not por-
     table across all platforms).

ERRORS
     [EINVAL]		The value of base is not supported or no conversion
			could be performed (the last feature is not portable
			across all platforms).

     [ERANGE]		The given string was out of range; the value converted
			has been clamped.

LEGACY SYNOPSIS
     #include &lt;stdlib.h&gt;
     #include &lt;limits.h&gt;

     &lt;limits.h&gt; is necessary for the strtoul() and strtoull() functions.

SEE ALSO
     strtol(3), strtol_l(3), strtonum(3), wcstoul(3), compat(5)

STANDARDS
     The strtoul() function conforms to ISO/IEC 9899:1990 (``ISO C90'').  The
     strtoull() and strtoumax() functions conform to ISO/IEC 9899:1999
     (``ISO C99'').  The BSD strtouq() function is deprecated.

BSD			       November 28, 2001			   BSD

   */
  def envStrtoul(p0: Int, p1: Int, p2: Int): IO[Int]
  /*

PRINTF(1)		  BSD General Commands Manual		     PRINTF(1)

NAME
     printf -- formatted output

SYNOPSIS
     printf format [arguments ...]

DESCRIPTION
     The printf utility formats and prints its arguments, after the first,
     under control of the format.  The format is a character string which con-
     tains three types of objects: plain characters, which are simply copied
     to standard output, character escape sequences which are converted and
     copied to the standard output, and format specifications, each of which
     causes printing of the next successive argument.

     The arguments after the first are treated as strings if the corresponding
     format is either c, b or s; otherwise it is evaluated as a C constant,
     with the following extensions:

	   o   A leading plus or minus sign is allowed.
	   o   If the leading character is a single or double quote, the value
	       is the character code of the next character.

     The format string is reused as often as necessary to satisfy the
     arguments.  Any extra format specifications are evaluated with zero or
     the null string.

     Character escape sequences are in backslash notation as defined in the
     ANSI X3.159-1989 (``ANSI C89''), with extensions.	The characters and
     their meanings are as follows:

	   \a	   Write a &lt;bell&gt; character.
	   \b	   Write a &lt;backspace&gt; character.
	   \c	   Ignore remaining characters in this string.
	   \f	   Write a &lt;form-feed&gt; character.
	   \n	   Write a &lt;new-line&gt; character.
	   \r	   Write a &lt;carriage return&gt; character.
	   \t	   Write a &lt;tab&gt; character.
	   \v	   Write a &lt;vertical tab&gt; character.
	   \'	   Write a &lt;single quote&gt; character.
	   \\	   Write a backslash character.
	   \num    Write a byte whose value is the 1-, 2-, or 3-digit octal
		   number num.	Multibyte characters can be constructed using
		   multiple \num sequences.

     Each format specification is introduced by the percent character (``%'').
     The remainder of the format specification includes, in the following
     order:

     Zero or more of the following flags:

	     #	     A `#' character specifying that the value should be
		     printed in an ``alternate form''.	For b, c, d, s and u
		     formats, this option has no effect.  For the o formats
		     the precision of the number is increased to force the
		     first character of the output string to a zero.  For the
		     x (X) format, a non-zero result has the string 0x (0X)
		     prepended to it.  For a, A, e, E, f, F, g and G formats,
		     the result will always contain a decimal point, even if
		     no digits follow the point (normally, a decimal point
		     only appears in the results of those formats if a digit
		     follows the decimal point).  For g and G formats, trail-
		     ing zeros are not removed from the result as they would
		     otherwise be;

	     -	     A minus sign `-' which specifies left adjustment of the
		     output in the indicated field;

	     +	     A `+' character specifying that there should always be a
		     sign placed before the number when using signed formats.

	     ` '     A space specifying that a blank should be left before a
		     positive number for a signed format.  A `+' overrides a
		     space if both are used;

	     0	     A zero `0' character indicating that zero-padding should
		     be used rather than blank-padding.  A `-' overrides a `0'
		     if both are used;

     Field Width:
	     An optional digit string specifying a field width; if the output
	     string has fewer bytes than the field width it will be blank-
	     padded on the left (or right, if the left-adjustment indicator
	     has been given) to make up the field width (note that a leading
	     zero is a flag, but an embedded zero is part of a field width);

     Precision:
	     An optional period, `.', followed by an optional digit string
	     giving a precision which specifies the number of digits to appear
	     after the decimal point, for e and f formats, or the maximum num-
	     ber of bytes to be printed from a string; if the digit string is
	     missing, the precision is treated as zero;

     Format:
	     A character which indicates the type of format to use (one of
	     diouxXfFeEgGaAcsb).  The uppercase formats differ from their low-
	     ercase counterparts only in that the output of the former is
	     entirely in uppercase.  The floating-point format specifiers
	     (fFeEgGaA) may be prefixed by an L to request that additional
	     precision be used, if available.

     A field width or precision may be `*' instead of a digit string.  In this
     case an argument supplies the field width or precision.

     The format characters and their meanings are:

     diouXx	 The argument is printed as a signed decimal (d or i),
		 unsigned octal, unsigned decimal, or unsigned hexadecimal (X
		 or x), respectively.

     fF 	 The argument is printed in the style `[-]ddd.ddd' where the
		 number of d's after the decimal point is equal to the preci-
		 sion specification for the argument.  If the precision is
		 missing, 6 digits are given; if the precision is explicitly
		 0, no digits and no decimal point are printed.  The values
		 infinity and NaN are printed as `inf' and `nan', respec-
		 tively.

     eE 	 The argument is printed in the style e `[-d.ddd+-dd]' where
		 there is one digit before the decimal point and the number
		 after is equal to the precision specification for the argu-
		 ment; when the precision is missing, 6 digits are produced.
		 The values infinity and NaN are printed as `inf' and `nan',
		 respectively.

     gG 	 The argument is printed in style f (F) or in style e (E)
		 whichever gives full precision in minimum space.

     aA 	 The argument is printed in style `[-h.hhh+-pd]' where there
		 is one digit before the hexadecimal point and the number
		 after is equal to the precision specification for the argu-
		 ment; when the precision is missing, enough digits are pro-
		 duced to convey the argument's exact double-precision float-
		 ing-point representation.  The values infinity and NaN are
		 printed as `inf' and `nan', respectively.

     c		 The first byte of argument is printed.

     s		 Bytes from the string argument are printed until the end is
		 reached or until the number of bytes indicated by the preci-
		 sion specification is reached; however if the precision is 0
		 or missing, the string is printed entirely.

     b		 As for s, but interpret character escapes in backslash nota-
		 tion in the string argument.  The permitted escape sequences
		 are slightly different in that octal escapes are \0num
		 instead of \num.

     n$ 	 Allows reordering of the output according to argument.

     %		 Print a `%'; no argument is used.

     The decimal point character is defined in the program's locale (category
     LC_NUMERIC).

     In no case does a non-existent or small field width cause truncation of a
     field; padding takes place only if the specified field width exceeds the
     actual width.

     Some shells may provide a builtin printf command which is similar or
     identical to this utility.  Consult the builtin(1) manual page.

EXIT STATUS
     The printf utility exits 0 on success, and &gt;0 if an error occurs.

COMPATIBILITY
     The traditional BSD behavior of converting arguments of numeric formats
     not beginning with a digit to the ASCII code of the first character is
     not supported.

SEE ALSO
     builtin(1), echo(1), sh(1), printf(3)

STANDARDS
     The printf command is expected to be compatible with the IEEE Std 1003.2
     (``POSIX.2'') specification.

HISTORY
     The printf command appeared in 4.3BSD-Reno.  It is modeled after the
     standard library function, printf(3).

CAVEATS
     ANSI hexadecimal character constants were deliberately not provided.

     Trying to print a dash (&quot;-&quot;) as the first character causes printf to
     interpret the dash as a program argument.	-- must be used before format.

     If the locale contains multibyte characters (such as UTF-8), the c format
     and b and s formats with a precision may not operate as expected.

BUGS
     Since the floating point numbers are translated from ASCII to floating-
     point and then back again, floating-point precision may be lost.  (By
     default, the number is translated to an IEEE-754 double-precision value
     before being printed.  The L modifier may produce additional precision,
     depending on the hardware platform.)

     The escape sequence \000 is the string terminator.  When present in the
     argument for the b format, the argument will be truncated at the \000
     character.

     Multibyte characters are not recognized in format strings (this is only a
     problem if `%' can appear inside a multibyte character).

BSD				April 21, 2014				   BSD

   */
  def envPrintf(p0: Int, p1: Int): IO[Int]
  /*

FOPEN(3)		 BSD Library Functions Manual		      FOPEN(3)

NAME
     fopen, fdopen, freopen, fmemopen -- stream open functions

LIBRARY
     Standard C Library (libc, -lc)

SYNOPSIS
     #include &lt;stdio.h&gt;

     FILE *
     fopen(const char * restrict path, const char * restrict mode);

     FILE *
     fdopen(int fildes, const char *mode);

     FILE *
     freopen(const char *path, const char *mode, FILE *stream);

     FILE *
     fmemopen(void *restrict *buf, size_t size, const char * restrict mode);

DESCRIPTION
     The fopen() function opens the file whose name is the string pointed to
     by path and associates a stream with it.

     The argument mode points to a string beginning with one of the following
     letters:

     ``r''   Open for reading.	The stream is positioned at the beginning of
	     the file.	Fail if the file does not exist.

     ``w''   Open for writing.	The stream is positioned at the beginning of
	     the file.	Create the file if it does not exist.

     ``a''   Open for writing.	The stream is positioned at the end of the
	     file.  Subsequent writes to the file will always end up at the
	     then current end of file, irrespective of any intervening
	     fseek(3) or similar.  Create the file if it does not exist.

     An optional ``+'' following ``r'', ``w'', or ``a'' opens the file for
     both reading and writing.	An optional ``x'' following ``w'' or ``w+''
     causes the fopen() call to fail if the file already exists.  An optional
     ``e'' following the above causes the fopen() call to set the FD_CLOEXEC
     flag on the underlying file descriptor.

     The mode string can also include the letter ``b'' after either the ``+''
     or the first letter.  This is strictly for compatibility with ISO/IEC
     9899:1990 (``ISO C90'') and has effect only for fmemopen() ; otherwise
     ``b'' is ignored.

     Any created files will have mode ``S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP
     | S_IROTH | S_IWOTH'' (0666), as modified by the process' umask value
     (see umask(2)).

     Reads and writes may be intermixed on read/write streams in any order;
     however, a file positioning function must be called when switching
     between output and input, unless an input operation encounters end-of-
     file.

     The fdopen() function associates a stream with the existing file descrip-
     tor, fildes.  The mode of the stream must be compatible with the mode of
     the file descriptor.  The ``x'' mode option is ignored.  If the ``e''
     mode option is present, the FD_CLOEXEC flag is set, otherwise it remains
     unchanged.  When the stream is closed via fclose(3), fildes is closed
     also.

     The freopen() function opens the file whose name is the string pointed to
     by path and associates the stream pointed to by stream with it.  The
     original stream (if it exists) is closed.	The mode argument is used just
     as in the fopen() function.

     If the path argument is NULL, freopen() attempts to re-open the file
     associated with stream with a new mode.  The new mode must be compatible
     with the mode that the stream was originally opened with: Streams open
     for reading can only be re-opened for reading, streams open for writing
     can only be re-opened for writing, and streams open for reading and writ-
     ing can be re-opened in any mode.	The ``x'' mode option is not meaning-
     ful in this context.

     The primary use of the freopen() function is to change the file associ-
     ated with a standard text stream (stderr, stdin, or stdout).

     The fmemopen() function associates the buffer given by the buf and size
     arguments with a stream.  The buf argument is either a null pointer or a
     pointer to a buffer that is at least size bytes long.  If a null pointer
     is specified as the buf argument, fmemopen() allocates size bytes of mem-
     ory, and this allocation is automatically freed when the stream is
     closed.  If a non-null pointer is specified, the caller retains ownership
     of the buffer and is responsible for disposing of it after the stream has
     been closed.  Buffers can be opened in text-mode (default) or binary-mode
     (if ``b'' is present in the second or third position of the mode argu-
     ment). Buffers opened in text-mode make sure that writes are terminated
     with a NULL byte, if the last write hasn't filled up the whole buffer.
     Buffers opened in binary-mode never append a NULL byte.

     Input and output against the opened stream will be fully buffered, unless
     it refers to an interactive terminal device, or a different kind of
     buffering is specified in the environment.  See setvbuf(3) for additional
     details.

RETURN VALUES
     Upon successful completion fopen(), fdopen(), freopen() and fmemopen()
     return a FILE pointer.  Otherwise, NULL is returned and the global vari-
     able errno is set to indicate the error.

ERRORS
     [EINVAL]		The mode argument to fopen(), fdopen(), freopen(), or
			fmemopen() was invalid.

     The fopen(), fdopen(), freopen() and fmemopen() functions may also fail
     and set errno for any of the errors specified for the routine malloc(3).

     The fopen() function may also fail and set errno for any of the errors
     specified for the routine open(2).

     The fdopen() function may also fail and set errno for any of the errors
     specified for the routine fcntl(2).

     The freopen() function may also fail and set errno for any of the errors
     specified for the routines open(2), fclose(3) and fflush(3).

     The fmemopen() function may also fail and set errno if the size argument
     is 0.

SEE ALSO
     open(2), fclose(3), fileno(3), fseek(3), funopen(3)

STANDARDS
     The fopen() and freopen() functions conform to ISO/IEC 9899:1990
     (``ISO C90''), with the exception of the ``x'' mode option which conforms
     to ISO/IEC 9899:2011 (``ISO C11'').  The fdopen() function conforms to
     IEEE Std 1003.1-1988 (``POSIX.1'').  The ``e'' mode option does not con-
     form to any standard but is also supported by glibc.  The fmemopen()
     function conforms to IEEE Std 1003.1-2008 (``POSIX.1'').  The ``b'' mode
     does not conform to any standard but is also supported by glibc.

BSD			       January 30, 2013 			   BSD

   */
  def envFopen(p0: Int, p1: Int): IO[Int]
  /*

PUTC(3) 		 BSD Library Functions Manual		       PUTC(3)

NAME
     fputc, putc, putc_unlocked, putchar, putchar_unlocked, putw -- output a
     character or word to a stream

LIBRARY
     Standard C Library (libc, -lc)

SYNOPSIS
     #include &lt;stdio.h&gt;

     int
     fputc(int c, FILE *stream);

     int
     putc(int c, FILE *stream);

     int
     putc_unlocked(int c, FILE *stream);

     int
     putchar(int c);

     int
     putchar_unlocked(int c);

     int
     putw(int w, FILE *stream);

DESCRIPTION
     The fputc() function writes the character c (converted to an ``unsigned
     char'') to the output stream pointed to by stream.

     The putc() macro acts essentially identically to fputc(), but is a macro
     that expands in-line.  It may evaluate stream more than once, so argu-
     ments given to putc() should not be expressions with potential side
     effects.

     The putchar() function is identical to putc() with an output stream of
     stdout.

     The putw() function writes the specified int to the named output stream.

     The putc_unlocked() and putchar_unlocked() functions are equivalent to
     putc() and putchar() respectively, except that the caller is responsible
     for locking the stream with flockfile(3) before calling them.  These
     functions may be used to avoid the overhead of locking the stream for
     each character, and to avoid output being interspersed from multiple
     threads writing to the same stream.

RETURN VALUES
     The functions, fputc(), putc(), putchar(), putc_unlocked(), and
     putchar_unlocked() return the character written.  If an error occurs, the
     value EOF is returned.  The putw() function returns 0 on success; EOF is
     returned if a write error occurs, or if an attempt is made to write a
     read-only stream.

SEE ALSO
     ferror(3), flockfile(3), fopen(3), getc(3), putwc(3), stdio(3)

STANDARDS
     The functions fputc(), putc(), and putchar(), conform to ISO/IEC
     9899:1990 (``ISO C90'').  The putc_unlocked() and putchar_unlocked()
     functions conform to IEEE Std 1003.1-2001 (``POSIX.1'').  A function
     putw() function appeared in Version 6 AT&amp;T UNIX.

BUGS
     The size and byte order of an int varies from one machine to another, and
     putw() is not recommended for portable applications.

BSD			       January 10, 2003 			   BSD

   */
  def envPutchar(p0: Int): IO[Int]
  def imports() = {
    Imports[IO](
      TCMap[String, AsIsIO](
        "env" -> TCMap[String, AsIIO]("fclose" -> envFclose _,
                                      "fread" -> envFread _,
                                      "strtoul" -> envStrtoul _,
                                      "printf" -> envPrintf _,
                                      "fopen" -> envFopen _,
                                      "putchar" -> envPutchar _))
    )
  }
}
