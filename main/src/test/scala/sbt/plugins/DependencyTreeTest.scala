/*
 * sbt
 * Copyright 2023, Scala center
 * Copyright 2011 - 2022, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package plugins

import sbt.internal.util.complete.Parser
import DependencyTreeSettings.{ Arg, ArgsParser, Fmt, FmtParser }

object DependencyTreeTest extends verify.BasicTestSuite:
  test("Parse args") {
    assert(parseArgs(List("help")) == List(Arg.Help))
    assert(parseArgs(List("--help")) == List(Arg.Help))
    assert(parseArgs(List("--quiet")) == List(Arg.Quiet))
    assert(parseArgs(List("tree")) == List(Arg.Format(Fmt.Tree)))
    assert(parseArgs(List("--out", "/tmp/deps.txt")) == List(Arg.Out("/tmp/deps.txt")))
    assert(parseArgs(List("--browse")) == List(Arg.Browse))
  }

  test("Parse format") {
    assert(parseFormat("tree") == Fmt.Tree)
    assert(parseFormat("list") == Fmt.List)
    assert(parseFormat("stats") == Fmt.Stats)
    assert(parseFormat("json") == Fmt.Json)
    assert(parseFormat("html") == Fmt.Html)
    assert(parseFormat("graph") == Fmt.Graph)
  }

  def parseArgs(args: List[String]): Seq[Arg] =
    Parser.parse(" " + args.mkString(" "), ArgsParser) match
      case Right(args) => args
      case Left(err)   => sys.error(err)

  def parseFormat(fmt: String): Fmt =
    Parser.parse(fmt, FmtParser) match
      case Right(x)  => x
      case Left(err) => sys.error(err)
end DependencyTreeTest
