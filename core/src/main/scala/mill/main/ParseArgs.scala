package mill.main

import mill.util.EitherOps
import fastparse.all._
import mill.define.Segment

object ParseArgs {

  def apply(scriptArgs: Seq[String])
    : Either[String, (List[List[Segment]], Seq[String])] = {
    val (selectors, args, isMultiSelectors) = extractSelsAndArgs(scriptArgs)
    for {
      _ <- validateSelectors(selectors)
      expandedSelectors <- EitherOps
        .sequence(selectors.map(expandBraces))
        .map(_.flatten)
      _ <- validateExpanded(selectors, expandedSelectors, isMultiSelectors)
      selectors <- EitherOps.sequence(expandedSelectors.map(extractSegments))
    } yield (selectors.toList, args)
  }

  def extractSelsAndArgs(
      scriptArgs: Seq[String]): (Seq[String], Seq[String], Boolean) = {
    val multiFlags = Seq("--all", "--seq")
    val isMultiSelectors = scriptArgs.headOption.exists(multiFlags.contains)

    if (isMultiSelectors) {
      val dd = scriptArgs.indexOf("--")
      val selectors = (if (dd == -1) scriptArgs
                       else scriptArgs.take(dd)).filterNot(multiFlags.contains)
      val args = if (dd == -1) Seq.empty else scriptArgs.drop(dd + 1)

      (selectors, args, isMultiSelectors)
    } else {
      (scriptArgs.take(1), scriptArgs.drop(1), isMultiSelectors)
    }
  }

  private def validateSelectors(selectors: Seq[String]): Either[String, Unit] =
    if (selectors.isEmpty || selectors.exists(_.isEmpty))
      Left("Selector cannot be empty")
    else Right(())

  private def validateExpanded(selectors: Seq[String],
                               expanded: Seq[String],
                               isMulti: Boolean): Either[String, Unit] =
    if (!isMulti && expanded.length > selectors.length)
      Left("Please use --all flag to run multiple tasks")
    else Right(())

  def expandBraces(selectorString: String): Either[String, List[String]] =
    parseBraceExpansion(selectorString) match {
      case f: Parsed.Failure           => Left(s"Parsing exception ${f.msg}")
      case Parsed.Success(expanded, _) => Right(expanded.toList)
    }

  private sealed trait Fragment
  private object Fragment {
    case class Keep(value: String) extends Fragment
    case class Expand(values: Seq[Fragment]) extends Fragment

    def unfold(fragments: Seq[Fragment]): Seq[String] = {
      fragments match {
        case Keep(v) +: rest =>
          for (unfolded <- unfold(rest)) yield v + unfolded
        case Expand(vs) +: rest =>
          for {
            inner <- vs.map {
              case Expand(ivs) => unfold(ivs)
              case k: Keep     => unfold(Seq(k))
            }
            v <- inner
            unfolded <- unfold(rest)
          } yield v + unfolded
        case Seq() => Seq("")
      }
    }
  }

  private def parseBraceExpansion(input: String) = {
    val bracesBase = CharsWhile(c => c != ',' && c != '{' && c != '}').!.map(Fragment.Keep)
    val insideBraces = P(bracesBase ~ !"{")

    val withBraces = P("{" ~ CharsWhile(c => c != ',' && c != '}').rep ~ "}").!.map(Fragment.Keep) // TODO: try to remove

    val other = P(CharsWhile(c => c != '{' && c != '}')).!.map(Fragment.Keep)

    def expandRecur: P[Fragment] =
      (bracesBase.? ~ toExpand ~ bracesBase.?).map {
        case (a, b, c) =>
          Fragment.Expand(a.toSeq ++ Seq(b) ++ c.toSeq)
      }

    def toExpand: P[Fragment] =
      P("{" ~ (insideBraces | expandRecur).rep(2, sep = ",").map(Fragment.Expand) ~ "}")

    def braceParser = P(withBraces | toExpand | other)

    val parser = braceParser.rep ~ End

    parser.map(Fragment.unfold).parse(input)
  }

  def extractSegments(selectorString: String): Either[String, List[Segment]] =
    parseSelector(selectorString) match {
      case f: Parsed.Failure           => Left(s"Parsing exception ${f.msg}")
      case Parsed.Success(selector, _) => Right(selector)
    }

  private def parseSelector(input: String) = {
    val segment =
      P(CharsWhileIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).!).map(
        Segment.Label
      )
    val crossSegment =
      P("[" ~ CharsWhile(c => c != ',' && c != ']').!.rep(1, sep = ",") ~ "]")
        .map(Segment.Cross)
    val query = P(segment ~ ("." ~ segment | crossSegment).rep ~ End).map {
      case (h, rest) => h :: rest.toList
    }
    query.parse(input)
  }

}
