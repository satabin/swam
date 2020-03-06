def stdlib() = {

  Imports[{F}](
    TCMap[String, AsIsIO](
      "env" -> TCMap[String, AsIIO](
        "memory" -> buffer,
        // STD out
        "printf" -> std.printformatted _,
        "puts" -> std.puts _,
        "putchar" -> std.putchar _,
        "perror" -> std.perror _,
        "snprintf" -> std.snprintf _,

        // Mem ops
        "malloc" -> std.malloc _,
        "free" -> std.free _,
        "memcpy" -> std.memcpy _,
        "calloc" -> std.calloc _,


        // Mem ops
        "fopen" -> std.fopen _,
        "fclose" -> std.fclose _,
        "fputc" -> std.fputc _,
        "fgetc" -> std.fgetc _,
        "fgets" -> std.fgets _,

        "rand" -> std.rand _,
        "atoi" -> std.atoi _,
        "strtoull" -> std.stroull _,
        "strcmp" -> std.strcmp _,
        "srand" -> std.srand _,
        "time" -> std.time _,
        "fork" -> std.fork _,
        "sleep" -> std.sleep _,
        "wait" -> std.wait _,
        "strlen" -> std.strlen _,
        "memset" -> std.memset _,
        "sprintf" -> std.sprintf _,
        "realloc" -> std.realloc _,
        "sin" -> std.sin _,
        "cos" -> std.sin _,
        "exit" -> std.exit _,
        "clock" -> std.clock _,
        "__assert_fail" -> std.__assert_fail _,
        "qsort" -> std.qsort _,
        "log10" -> std.log10 _,
        "cexp" -> std.cexp _,
        "__muldc3" -> std.__muldc3 _,
        "__divdc3" -> std.__muldc3 _,
        "strchr" -> std.strchr _,
        "strcpy" -> std.strchr _,
        "strncpy" -> std.strcat _,
        "strncat" -> std.strcat _,

        "memmove" -> std.strcat _,
        "atof" -> std.atof _,
        "setlocale" -> std.setlocate _,
        "SDL_GetTicks" -> std.SDL_GetTicks _,
        "SDL_LockSurface" -> std.SDL_LockSurface _,
        "SDL_UnlockSurface" -> std.SDL_UnlockSurface _,
        "SDL_Flip" -> std.SDL_Flip _,
        "SDL_Init" -> std.SDL_Init _,
        "SDL_SetVideoMode" -> std.SDL_SetVideoMode _,
        "abort" -> std.abort _,
        "strtol" -> std.strtol _,
        "strtok" -> std.strtok _
      )))
}