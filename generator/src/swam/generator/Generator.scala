package swam.generator

import java.io.File
import java.util.concurrent.Executors

import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource, Sync}
import org.json4s.DefaultFormats
import swam.runtime.{Engine, Import}
import org.json4s.jackson.Serialization.writePretty
import cats.implicits._

import scala.concurrent.ExecutionContext

case class Config(wasms: Seq[File] = Seq(),
                  printTemplateContext: Boolean = false,
                  createBoilerplate: String = "",
                  className: String = "GeneratedImports",
                  renderTemplate: File = null)

/**
    @author Javier Cabrera-Arteaga on 2020-03-07
  */
object Generator extends IOApp {

  val generator = ImportGenerator[IO]

  def blockingThreadPool[F[_]](implicit F: Sync[F]): Resource[F, ExecutionContext] =
    Resource(F.delay {
      val executor = Executors.newCachedThreadPool()
      val ec = ExecutionContext.fromExecutor(executor)
      (ec, F.delay(executor.shutdown()))
    })

  val parser = new scopt.OptionParser[Config]("scopt") {
    head("swam-generator")

    help("help").text("prints this usage text")
    note("swam-generator generates the scala code using Mustache template engine")

    opt[String]('p', "create-boilerplate")
      .optional()
      .action((f, c) => c.copy(createBoilerplate = f))
      .text("Create a scala boilerplate project to implement the imports")

    opt[String]('n', "trait-class-name")
      .optional()
      .action((f, c) => c.copy(className = f))
      .text("Creates the trait object with the specified name")

    opt[Boolean]('c', "print-context")
      .optional()
      .action((f, c) => c.copy(printTemplateContext = f, createBoilerplate = "")) // Avoid he boilerplate generation
      .text("Prints the template context (JSON) to feed the template engine")

    opt[File]('t', "template")
      .optional()
      .action((x, c) => c.copy(renderTemplate = x))
      .text("Replaces template for the imports generation")

    arg[File]("<wasms>...")
      .unbounded()
      .required()
      .minOccurs(1)
      .action((x, c) => c.copy(wasms = c.wasms :+ x))
      .text("WASMs module to extract the imports")

  }

  def getImports(w: File, blocker: Blocker) =
    for {
      engine <- Engine[IO]()
      module <- engine.compile(w.toPath, blocker, 4096)
    } yield module.imports

  def concatImports(wasms: Seq[File]): IO[Vector[Import]] = Blocker[IO].use { blocker =>
    {
      wasms.map(w => getImports(w, blocker)).reduce((r, l) => r.combine(l))
    }
  }

  def generate(config: Config, imports: Vector[Import]): IO[Unit] = {
    if (config.renderTemplate != null)
      generator.changeTemplate(config.renderTemplate.getPath)

    if (config.createBoilerplate.isEmpty) {
      println()
      if (!config.printTemplateContext)
        IO(println(generator.generateImportText(imports, config.className)))
      else {
        val context = generator.getContext(imports)
        implicit val formats = DefaultFormats

        IO(println(writePretty(context)))
      }
    } else {
      generator.createScalaProjectForImports(config.createBoilerplate, imports, config.className)
    }
  }

  def run(args: List[String]): IO[ExitCode] = parser.parse(args, Config()) match {
    case Some(config) => {
      for {
        imports <- concatImports(config.wasms)
        _ <- generate(config, imports)
      } yield ()
    }.as(ExitCode.Success)
    case None =>
      IO(parser.reportError("You must provide a WASM file")).as(ExitCode.Error)
    // arguments are bad, error message will have been displayed
  }

}
