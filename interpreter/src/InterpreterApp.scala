import cats.effect.{ExitCode, IO, IOApp}

/**
    @author Javier Cabrera-Arteaga on 2020-03-12
  */
object InterpreterApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    IO(ExitCode.Success)
  }
}
