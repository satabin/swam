package swam
package cfg

import syntax.pretty._
import util.pretty._

import cats.Show

package object dot {

  implicit object CfgDotShow extends Show[CFG] {
    private def transitions(bbs: List[BasicBlock]): List[String] = {
      def loop(toVisit: List[(BasicBlock, Int)], acc: List[String]): List[String] = toVisit match {
        case (BasicBlock(name, insts, jump), id) :: rest =>
          val node =
            if (insts.isEmpty)
              s"""bb$id[label="$name"]"""
            else
              s"""bb$id[label="{$name|${insts.map(_.pretty.render(80)).mkString("\\n")}}"]"""
          val edges = jump match {
            case Some(Jump.To(lbl)) => List(s"bb$id->bb$lbl")
            case Some(Jump.If(tlbl, elbl)) =>
              List(s"""bb$id->bb$tlbl[label="true"]""", s"""bb$id->bb$elbl[label="false"]""")
            case Some(Jump.Table(cases, dflt)) =>
              cases.toList.zipWithIndex
                .map { case (lbl, idx) => s"""bb$id->bb$lbl[label="$idx"]""" } :+ s"""bb$id->bb$dflt[label="default"]"""
            case None => Nil
          }
          loop(rest, edges reverse_::: node :: acc)
        case Nil => acc.reverse
      }
      loop(bbs.zipWithIndex, Nil)
    }
    def show(cfg: CFG): String = {
      s"""digraph {
        rankdir=TB;
        node[shape=record];
        ${transitions(cfg.blocks).mkString(";\n")}
      }"""
    }
  }

}
