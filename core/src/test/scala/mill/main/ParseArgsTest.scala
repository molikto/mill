package mill.main

import mill.discover.Mirror
import utest._

object ParseArgsTest extends TestSuite {

  val emptyArgs = Seq.empty[String]

  val singleSelector = Seq("core.compile")

  val singleSelectorWithCross = Seq("bridges[2.12,jvm].compile")

  val singleSelectorWithArgs = Seq("application.run", "hello", "world")

  val multiSelectorsAll =
    Seq("--all", "core.jar", "core.docsJar", "core.sourcesJar")

  val multiSelectorsSeq =
    Seq("--seq", "core.jar", "core.docsJar", "core.sourcesJar")

  val multiSelectorsWithArgsAll = Seq("--all",
                                      "core.compile",
                                      "application.runMain",
                                      "--",
                                      "Main",
                                      "hello",
                                      "world")

  val multiSelectorsBraceExpansion = Seq("--all", "{core,application}.compile")

  val multiSelectorsBraceExpansionWithCross =
    Seq("--all", "bridges[2.12,jvm].{test,jar}")

  val multiSelectorsBraceExpansionWithArgs =
    Seq("--all", "{core,application}.run", "--", "hello", "world")

  val tests = Tests {
    'extractSelsAndArgs - {
      'empty - {
        val (selectors, args) = ParseArgs.extractSelsAndArgs(emptyArgs)

        assert(
          selectors.isEmpty,
          args.isEmpty
        )
      }
      'singleSelector - {
        val (selectors, args) =
          ParseArgs.extractSelsAndArgs(singleSelector)

        assert(
          selectors == singleSelector,
          args.isEmpty
        )
      }
      'singleSelectorWithArgs - {
        val (selectors, args) =
          ParseArgs.extractSelsAndArgs(singleSelectorWithArgs)

        assert(
          selectors == Seq("application.run"),
          args == Seq("hello", "world")
        )
      }
      'multiSelectorsAll - {
        val (selectors, args) =
          ParseArgs.extractSelsAndArgs(multiSelectorsAll)

        assert(
          !selectors.contains("--all"),
          selectors == Seq("core.jar", "core.docsJar", "core.sourcesJar"),
          args.isEmpty
        )
      }
      'multiSelectorsSeq - {
        val (selectors, args) =
          ParseArgs.extractSelsAndArgs(multiSelectorsSeq)

        assert(
          !selectors.contains("--seq"),
          selectors == Seq("core.jar", "core.docsJar", "core.sourcesJar"),
          args.isEmpty
        )
      }
      'multiSelectorsWithArgsAll - {
        val (selectors, args) =
          ParseArgs.extractSelsAndArgs(multiSelectorsWithArgsAll)

        assert(
          !selectors.contains("--all"),
          selectors == Seq("core.compile", "application.runMain"),
          !args.contains("--"),
          args == Seq("Main", "hello", "world")
        )

      }
    }
    'expandBraces - {
      'expandLeft - {
        val expandLeft = "{application,core}.compile"
        val Right(expanded) = ParseArgs.expandBraces(expandLeft)

        assert(
          expanded == List(
            "application.compile",
            "core.compile"
          )
        )
      }
      'expandRight - {
        val expandRight = "application.{jar,docsJar,sourcesJar}"
        val Right(expanded) = ParseArgs.expandBraces(expandRight)

        assert(
          expanded == List(
            "application.jar",
            "application.docsJar",
            "application.sourcesJar"
          )
        )
      }
      'expandBoth - {
        val expandBoth = "{core,application}.{jar,docsJar}"
        val Right(expanded) = ParseArgs.expandBraces(expandBoth)

        assert(
          expanded == List(
            "core.jar",
            "core.docsJar",
            "application.jar",
            "application.docsJar"
          )
        )
      }
      'expandMixed - {
        val Right(expanded) = ParseArgs.expandBraces("{a,b}.{c}.{}.e")

        assert(
          expanded == List(
            "a.{c}.{}.e",
            "b.{c}.{}.e"
          )
        )
      }
      'malformed - {
        val malformed = Seq("core.{compile", "core.{compile,test]")

        malformed.foreach { m =>
          val Left(error) = ParseArgs.expandBraces(m)
          assert(error.contains("Parsing exception"))
        }
      }
      'dontExpand - {
        val dontExpand = Seq("core.compile", "{}.compile", "{core}.compile")

        dontExpand.foreach { ex =>
          val Right(result) = ParseArgs.expandBraces(ex)
          assert(
            result == List(ex)
          )
        }
      }
      'keepUnknownSymbols - {
        val expected = Seq(
          "{a,b}.e<>" -> List("a.e<>", "b.e<>"),
          "a[99]&&" -> List("a[99]&&"),
          "{a,b}.<%%>.{c,d}" -> List(
            "a.<%%>.c",
            "a.<%%>.d",
            "b.<%%>.c",
            "b.<%%>.d"
          )
        )

        expected.foreach {
          case (in, expectedOut) =>
            val Right(out) = ParseArgs.expandBraces(in)
            assert(
              out == expectedOut
            )
        }
      }
    }

    'apply - {
      'rejectEmpty {
        val Left(error) = ParseArgs(Seq.empty)
        assert(error == "Selector cannot be empty")
      }
      'singleSelector - {
        val Right((selectors, args)) =
          ParseArgs(singleSelector)
        assert(
          selectors == List(
            List(
              Mirror.Segment.Label("core"),
              Mirror.Segment.Label("compile")
            )
          ),
          args.isEmpty
        )
      }
      'singleSelectorWithArgs - {
        val Right((selectors, args)) =
          ParseArgs(singleSelectorWithArgs)
        assert(
          selectors == List(
            List(
              Mirror.Segment.Label("application"),
              Mirror.Segment.Label("run")
            )
          ),
          args == Seq("hello", "world")
        )
      }
      'singleSelectorWithCross - {
        val Right((selectors, args)) =
          ParseArgs(singleSelectorWithCross)
        assert(
          selectors == List(
            List(
              Mirror.Segment.Label("bridges"),
              Mirror.Segment.Cross(Seq("2.12", "jvm")),
              Mirror.Segment.Label("compile")
            )
          ),
          args.isEmpty
        )
      }
      'multiSelectorsBraceExpansion - {
        val Right((selectors, args)) =
          ParseArgs(multiSelectorsBraceExpansion)

        assert(
          selectors == List(
            List(
              Mirror.Segment.Label("core"),
              Mirror.Segment.Label("compile")
            ),
            List(
              Mirror.Segment.Label("application"),
              Mirror.Segment.Label("compile")
            )
          ),
          args.isEmpty
        )
      }
      'multiSelectorsBraceExpansionWithArgs - {
        val Right((selectors, args)) =
          ParseArgs(multiSelectorsBraceExpansionWithArgs)

        assert(
          selectors == List(
            List(
              Mirror.Segment.Label("core"),
              Mirror.Segment.Label("run")
            ),
            List(
              Mirror.Segment.Label("application"),
              Mirror.Segment.Label("run")
            )
          ),
          args == Seq("hello", "world")
        )
      }
      'multiSelectorsBraceExpansionWithCross - {
        val Right((selectors, args)) =
          ParseArgs(multiSelectorsBraceExpansionWithCross)

        assert(
          selectors == List(
            List(
              Mirror.Segment.Label("bridges"),
              Mirror.Segment.Cross(Seq("2.12", "jvm")),
              Mirror.Segment.Label("test")
            ),
            List(
              Mirror.Segment.Label("bridges"),
              Mirror.Segment.Cross(Seq("2.12", "jvm")),
              Mirror.Segment.Label("jar")
            )
          ),
          args.isEmpty
        )
      }
    }
  }

}
