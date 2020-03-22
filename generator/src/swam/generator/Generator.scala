package swam.generator

import java.io.File
import java.util.concurrent.Executors

import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource, Sync}
import org.json4s.DefaultFormats
import swam.runtime.{Engine, Import}
import org.json4s.jackson.Serialization.writePretty
import cats.implicits._
import swam.generator.witx.{EmitTraverse, ModuleTraverse}
import swam.witx.WitxParser
import swam.witx.parser.ImportContext

import scala.concurrent.ExecutionContext

case class Config(wasms: Seq[File] = Seq(),
                  printTemplateContext: Boolean = false,
                  createBoilerplate: String = "",
                  className: String = "GeneratedImports",
                  renderTemplate: File = null,
                  parseAsWitx: Boolean = false,
                  includeWitxTypesPath: Seq[String] = Seq())

/**
    @author Javier Cabrera-Arteaga on 2020-03-07
  */
object Generator extends IOApp {

  val generator = ImportGenerator[IO]

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

    opt[Boolean]('x', "witx")
      .optional()
      .action((f, c) => c.copy(parseAsWitx = f))
      .text("Parse input as witx definition, generating the module boilerplate in scala")

    opt[String]('i', "includes")
      .optional()
      .unbounded()
      .action((f, c) => c.copy(includeWitxTypesPath = c.includeWitxTypesPath :+ f))
      .text("Include folders to look for types definition in witx parsing")

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
    wasms.map(w => getImports(w, blocker)).reduce((r, l) => r.combine(l))
  }

  def parseWitx(witxFile: File, includes: Seq[String], newPackagePath: String) = {
    if (newPackagePath.isEmpty)
      throw new Exception("You must provide the path to create the boilerplate (--create-boilerplate)")
    Blocker[IO]
      .use(blocker => {
        for {
          parser <- IO(WitxParser[IO])
          ctx <- IO(ImportContext[IO]())
          (types, interface) <- parser.parseModuleInterface(witxFile.toPath, blocker, ctx)
          scalaTypesTemplate <- IO(new EmitTraverse(types).traverseAll("", (s1, s2) => s1 + s2))
          scalaTraitTemplate <- IO(new ModuleTraverse(interface, types).traverseAll("", (s1, s2) => s1 + s2))
          _ <- generator.createScalaProjectForImports(scalaTypesTemplate, scalaTraitTemplate, newPackagePath)
        } yield ()
      })
  }

  def generate(config: Config, imports: Vector[Import]): IO[Unit] = {

    if (config.createBoilerplate.isEmpty) {
      if (!config.printTemplateContext) {
        config.renderTemplate match {
          case null => IO(println(generator.generateImportText(imports, config.className)))
          case template: File =>
            IO(println(generator.generateImportText(imports, config.className, template.getPath)))
        }

      } else {
        val context = generator.getContext(imports)
        implicit val formats = DefaultFormats
        IO(println(writePretty(context)))
      }
    } else
      generator.createScalaProjectForImports(config.createBoilerplate, imports, config.className)
  }

  def run(args: List[String]): IO[ExitCode] = parser.parse(args, Config()) match {
    case Some(config) => {
      if (config.parseAsWitx) {
        parseWitx(config.wasms(0), config.includeWitxTypesPath, config.createBoilerplate)
      } else {
        for {
          imports <- concatImports(config.wasms)
          _ <- generate(config, imports)
        } yield ()
      }
    }.as(ExitCode.Success)
    case None =>
      IO(parser.reportError("You must provide a WASM file")).as(ExitCode.Error)
    // arguments are bad, error message will have been displayed
  }

}
