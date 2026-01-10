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
import DependencyTreeSettings.{
  Arg,
  ArgsParser,
  Fmt,
  FmtParser,
  ArtifactPattern,
  createArtifactPatternParser
}
import sbt.internal.graph.ModuleGraph
import sbt.internal.graph.GraphModuleId
import sbt.internal.graph.Module
import sbt.util.Logger

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

  test("ArtifactPatternParser with normal modules") {
    val graph = ModuleGraph(
      Seq(
        node("org1", "name1", "1.0"),
        node("org1", "name1", "2.0")
      ),
      Nil
    )
    val parser = createArtifactPatternParser(graph, Logger.Null)

    // Test matching
    assert(
      Parser.parse(" org1 name1 1.0", parser) == Right(
        ArtifactPattern("org1", "name1", Some("1.0"))
      )
    )
    assert(
      Parser.parse(" org1 name1 2.0", parser) == Right(
        ArtifactPattern("org1", "name1", Some("2.0"))
      )
    )
    assert(Parser.parse(" org1 name1", parser) == Right(ArtifactPattern("org1", "name1", None)))
  }

  test("ArtifactPatternParser with empty version") {
    val graph = ModuleGraph(
      Seq(
        node("org1", "name1", "")
      ),
      Nil
    )
    // This should not throw RuntimeException
    val parser = createArtifactPatternParser(graph, Logger.Null)

    // Should parse org and name, version is None
    assert(Parser.parse(" org1 name1", parser) == Right(ArtifactPattern("org1", "name1", None)))

    // Should NOT match empty string as version explicitly
    // This is hard to test because `token(Space ~> id.version)` where version is empty would match " " if it were allowed.
    // But since we filtered it out, it shouldn't match.
    // However, the fallback `success(None)` allows matching just org and name.
  }

  test("ArtifactPatternParser mixed valid and empty versions") {
    val graph = ModuleGraph(
      Seq(
        node("org1", "name1", ""),
        node("org1", "name1", "1.0")
      ),
      Nil
    )
    val parser = createArtifactPatternParser(graph, Logger.Null)

    // Valid version should be selectable
    assert(
      Parser.parse(" org1 name1 1.0", parser) == Right(
        ArtifactPattern("org1", "name1", Some("1.0"))
      )
    )

    // No version should be selectable
    assert(Parser.parse(" org1 name1", parser) == Right(ArtifactPattern("org1", "name1", None)))
  }

  def parseArgs(args: List[String]): Seq[Arg] =
    Parser.parse(" " + args.mkString(" "), ArgsParser) match
      case Right(args) => args
      case Left(err)   => sys.error(err)

  def parseFormat(fmt: String): Fmt =
    Parser.parse(fmt, FmtParser) match
      case Right(x)  => x
      case Left(err) => sys.error(err)

  def node(org: String, name: String, version: String): Module =
    Module(GraphModuleId(org, name, version), None, "", None, None, None)

end DependencyTreeTest
