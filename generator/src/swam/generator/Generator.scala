package swam.generator

import java.io.File
import java.util.concurrent.Executors

import cats.effect.{Blocker, IO}
import swam.runtime.Engine

import scala.concurrent.ExecutionContext
case class Config(wasms: Seq[File] = Seq(), createBoilerplate: String = "")

/**
    @author Javier Cabrera-Arteaga on 2020-03-07
  */
object Generator extends App {

  implicit val cs = IO.contextShift(ExecutionContext.Implicits.global)

  val blockingPool = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())
  val blocker: Blocker = Blocker.liftExecutionContext(blockingPool)

  val parser = new scopt.OptionParser[Config]("scopt") {
    head("swam-generator")

    help("help").text("prints this usage text")

    opt[String]('p', "create-boilerplate")
      .optional()
      .action((f, c) => c.copy(createBoilerplate = f))
      .text("Create a scala boilerplate project to implement the imports")

    arg[File]("<wasms>...")
      .unbounded()
      .required()
      .minOccurs(1)
      .action((x, c) => c.copy(wasms = c.wasms :+ x))
      .text("WASMs module to extract the imports")

  }

  // parser.parse returns Option[C]
  parser.parse(args, Config()) match {
    case Some(config) => {

      val engine = Engine[IO]().unsafeRunSync()
      val generator = ImportGenerator.make()

      val imports = config.wasms.flatMap(w => engine.compile(w.toPath, blocker, 4096).unsafeRunSync().imports).toVector

      if (config.createBoilerplate.isEmpty) {
        println()
        print(generator.generateImportText(imports))
      } else {
        generator.createScalaProjectForImports(config.createBoilerplate, imports)
      }

      System.exit(0)
    }
    case None => {
      parser.reportError("You must provide a WASM file")
    }
    // arguments are bad, error message will have been displayed
  }

}
