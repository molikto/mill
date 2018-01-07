package mill.main

import mill.discover.Mirror.Segment.{Cross, Label}
import utest._

object ParseArgsTest extends TestSuite {

  val emptyArgs = Seq.empty[String]

  val singleSelector = Seq("core.compile")

  val singleSelectorWithCross = Seq("bridges[2.12.4,jvm].compile")

  val singleSelectorWithArgs = Seq("application.run", "hello", "world")

  val singleSelectorWithAllInArgs =
    Seq("application.run", "hello", "world", "--all")

  val multiSelectors =
    Seq("--all", "core.jar", "core.docsJar", "core.sourcesJar")

  val multiSelectorsSeq =
    Seq("--seq", "core.jar", "core.docsJar", "core.sourcesJar")

  val multiSelectorsWithArgs = Seq("--all",
                                   "core.compile",
                                   "application.runMain",
                                   "--",
                                   "Main",
                                   "hello",
                                   "world")

  val multiSelectorsWithArgsWithAllInArgs = Seq("--all",
                                                "core.compile",
                                                "application.runMain",
                                                "--",
                                                "Main",
                                                "--all",
                                                "world")

  val multiSelectorsBraceExpansionWithoutAll = Seq("{core,application}.compile")

  val multiSelectorsBraceExpansion = Seq("--all", "{core,application}.compile")

  val multiSelectorsBraceExpansionWithCross =
    Seq("--all", "bridges[2.12.4,jvm].{test,jar}")

  val multiSelectorsBraceExpansionInsideCross =
    Seq("--all", "bridges[{2.11.11,2.11.8}].jar")

  val multiSelectorsBraceExpansionWithArgs =
    Seq("--all", "{core,application}.run", "--", "hello", "world")

  val tests = Tests {
    'extractSelsAndArgs - {
      'empty - {
        val (selectors, args, isMulti) = ParseArgs.extractSelsAndArgs(emptyArgs)

        assert(
          selectors.isEmpty,
          args.isEmpty,
          !isMulti
        )
      }
      'singleSelector - {
        val (selectors, args, isMulti) =
          ParseArgs.extractSelsAndArgs(singleSelector)

        assert(
          selectors == singleSelector,
          args.isEmpty,
          !isMulti
        )
      }
      'singleSelectorWithArgs - {
        val (selectors, args, isMulti) =
          ParseArgs.extractSelsAndArgs(singleSelectorWithArgs)

        assert(
          selectors == Seq("application.run"),
          args == Seq("hello", "world"),
          !isMulti
        )
      }
      'singleSelectorWithAllInArgs - {
        val (selectors, args, isMulti) =
          ParseArgs.extractSelsAndArgs(singleSelectorWithAllInArgs)

        assert(
          selectors == Seq("application.run"),
          args == Seq("hello", "world", "--all"),
          !isMulti
        )
      }
      'multiSelectors - {
        val (selectors, args, isMulti) =
          ParseArgs.extractSelsAndArgs(multiSelectors)

        assert(
          !selectors.contains("--all"),
          selectors == Seq("core.jar", "core.docsJar", "core.sourcesJar"),
          args.isEmpty,
          isMulti
        )
      }
      'multiSelectorsSeq - {
        val (selectors, args, isMulti) =
          ParseArgs.extractSelsAndArgs(multiSelectorsSeq)

        assert(
          !selectors.contains("--seq"),
          selectors == Seq("core.jar", "core.docsJar", "core.sourcesJar"),
          args.isEmpty,
          isMulti
        )
      }
      'multiSelectorsWithArgs - {
        val (selectors, args, isMulti) =
          ParseArgs.extractSelsAndArgs(multiSelectorsWithArgs)

        assert(
          !selectors.contains("--all"),
          selectors == Seq("core.compile", "application.runMain"),
          !args.contains("--"),
          args == Seq("Main", "hello", "world"),
          isMulti
        )
      }
      'multiSelectorsWithArgsWithAllInArgs - {
        val (selectors, args, isMulti) =
          ParseArgs.extractSelsAndArgs(multiSelectorsWithArgsWithAllInArgs)

        assert(
          !selectors.contains("--all"),
          selectors == Seq("core.compile", "application.runMain"),
          !args.contains("--"),
          args == Seq("Main", "--all", "world"),
          isMulti
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
      'expandNested - {
        val Right(expanded) = ParseArgs.expandBraces("{hello,world.{cow,moo}}")

        assert(
          expanded == List("hello", "world.cow", "world.moo")
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
            List(Label("core"), Label("compile"))
          ),
          args.isEmpty
        )
      }
      'singleSelectorWithArgs - {
        val Right((selectors, args)) =
          ParseArgs(singleSelectorWithArgs)
        assert(
          selectors == List(
            List(Label("application"), Label("run"))
          ),
          args == Seq("hello", "world")
        )
      }
      'singleSelectorWithCross - {
        val Right((selectors, args)) =
          ParseArgs(singleSelectorWithCross)
        assert(
          selectors == List(
            List(Label("bridges"),
                 Cross(Seq("2.12.4", "jvm")),
                 Label("compile"))),
          args.isEmpty
        )
      }
      'multiSelectorsBraceExpansion - {
        val Right((selectors, args)) =
          ParseArgs(multiSelectorsBraceExpansion)

        assert(
          selectors == List(
            List(Label("core"), Label("compile")),
            List(Label("application"), Label("compile"))
          ),
          args.isEmpty
        )
      }
      'multiSelectorsBraceExpansionWithArgs - {
        val Right((selectors, args)) =
          ParseArgs(multiSelectorsBraceExpansionWithArgs)

        assert(
          selectors == List(
            List(Label("core"), Label("run")),
            List(Label("application"), Label("run"))
          ),
          args == Seq("hello", "world")
        )
      }
      'multiSelectorsBraceExpansionWithCross - {
        val Right((selectors, args)) =
          ParseArgs(multiSelectorsBraceExpansionWithCross)

        assert(
          selectors == List(
            List(Label("bridges"), Cross(Seq("2.12.4", "jvm")), Label("test")),
            List(Label("bridges"), Cross(Seq("2.12.4", "jvm")), Label("jar"))
          ),
          args.isEmpty
        )
      }
      'multiSelectorsBraceExpansionInsideCross - {
        val Right((selectors, args)) =
          ParseArgs(multiSelectorsBraceExpansionInsideCross)

        assert(
          selectors == List(
            List(Label("bridges"), Cross(Seq("2.11.11")), Label("jar")),
            List(Label("bridges"), Cross(Seq("2.11.8")), Label("jar"))
          ),
          args.isEmpty
        )
      }
      'multiSelectorsBraceExpansionWithoutAll - {
        val Left(error) =
          ParseArgs(multiSelectorsBraceExpansionWithoutAll)

        assert(error == "Please use --all flag to run multiple tasks")
      }
      'multiSelectorsWithoutAllAsSingle - { // this is how it works when we pass multiple tasks without --all flag
        val argsList = Seq("core.compile", "application.compile")
        val Right((selectors, args)) = ParseArgs(argsList)

        assert(
          selectors == List(
            List(Label("core"), Label("compile"))
          ),
          args == Seq("application.compile")
        )
      }
    }
  }

}
