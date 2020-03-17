package swam
package stdlib
package impl

import cats.effect.IO
import swam.stdlib.GeneratedImports

/**
  * @author Javier Cabrera-Arteaga on 2020-03-12
  */
class Imports extends GeneratedImports {
  override def envGetTotalMemory(): IO[Int] = ???

  override def envTime(p0: Int): IO[Int] = ???

  override def envFread(p0: Int, p1: Int, p2: Int, p3: Int): IO[Int] = ???

  override def envAbort(): IO[Unit] = ???

  override def envSprintf(p0: Int, p1: Int, p2: Int): IO[Int] = ???

  override def envCalloc(p0: Int, p1: Int): IO[Int] = ???

  override def envStrtok(p0: Int, p1: Int): IO[Int] = ???

  override def envMemcpy(p0: Int, p1: Int, p2: Int): IO[Int] = ???

  override def envRealloc(p0: Int, p1: Int): IO[Int] = ???

  override def env_noise_rand_bytes(p0: Int, p1: Int): IO[Unit] = ???

  override def envSDL_UnlockSurface(p0: Int): IO[Unit] = ???

  override def envSDL_Init(p0: Int): IO[Int] = ???

  override def envFclose(p0: Int): IO[Int] = ???

  override def envStrcmp(p0: Int, p1: Int): IO[Int] = ???

  override def envSDL_LockSurface(p0: Int): IO[Int] = ???

  override def envAtoi(p0: Int): IO[Int] = ???

  override def envAbortOnCannotGrowMemory(): IO[Int] = ???

  override def envFopen(p0: Int, p1: Int): IO[Int] = ???

  override def env__divdc3(p0: Int, p1: Double, p2: Double, p3: Double, p4: Double): IO[Unit] = ???

  override def envWait(p0: Int): IO[Int] = ???

  override def envEnlargeMemory(): IO[Int] = ???

  override def envSrand(p0: Int): IO[Unit] = ???

  override def envCos(p0: Double): IO[Double] = ???

  override def envMemset(p0: Int, p1: Int, p2: Int): IO[Int] = ???

  override def envFork(): IO[Int] = ???

  override def envClock(): IO[Int] = ???

  override def envLog10(p0: Double): IO[Double] = ???

  override def env_llvm_stackrestore(p0: Int): IO[Unit] = ???

  override def envStrncpy(p0: Int, p1: Int, p2: Int): IO[Int] = ???

  override def envSDL_SetVideoMode(p0: Int, p1: Int, p2: Int, p3: Int): IO[Int] = ???

  override def envPuts(p0: Int): IO[Int] = ???

  override def envSleep(p0: Int): IO[Int] = ???

  override def env_emscripten_memcpy_big(p0: Int, p1: Int, p2: Int): IO[Int] = ???

  override def env___assert_fail(p0: Int, p1: Int, p2: Int, p3: Int): IO[Unit] = ???

  override def envStrtoul(p0: Int, p1: Int, p2: Int): IO[Int] = ???

  override def env__assert_fail(p0: Int, p1: Int, p2: Int, p3: Int): IO[Unit] = ???

  override def env_llvm_stacksave(): IO[Int] = ???

  override def envSscanf(p0: Int, p1: Int, p2: Int): IO[Int] = ???

  override def envSDL_Flip(p0: Int): IO[Int] = ???

  override def envSetlocale(p0: Int, p1: Int): IO[Int] = ???

  override def envAtof(p0: Int): IO[Double] = ???

  override def envQsort(p0: Int, p1: Int, p2: Int, p3: Int): IO[Unit] = ???

  override def envMalloc(p0: Int): IO[Int] = ???

  override def envSin(p0: Double): IO[Double] = ???

  override def envCexp(p0: Int, p1: Int): IO[Unit] = ???

  override def env___setErrNo(p0: Int): IO[Unit] = ???

  override def envPrintf(p0: Int, p1: Int): IO[Int] = ???

  override def envRand(): IO[Int] = ???

  override def envStrlen(p0: Int): IO[Int] = ???

  override def envPutchar(p0: Int): IO[Int] = ???

  override def envStrtol(p0: Int, p1: Int, p2: Int): IO[Int] = ???

  override def envSnprintf(p0: Int, p1: Int, p2: Int, p3: Int): IO[Int] = ???

  override def envExit(p0: Int): IO[Unit] = ???

  override def env__muldc3(p0: Int, p1: Double, p2: Double, p3: Double, p4: Double): IO[Unit] = ???

  override def envFree(p0: Int): IO[Unit] = ???

  override def envSDL_GetTicks(): IO[Int] = ???
}
