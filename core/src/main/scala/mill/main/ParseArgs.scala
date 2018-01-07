package mill.main

import mill.discover.Mirror
import mill.util.EitherOps
import fastparse.all._

object ParseArgs {

  def apply(scriptArgs: Seq[String])
    : Either[String, (List[List[Mirror.Segment]], Seq[String])] = {
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
    case class Expand(values: Seq[String]) extends Fragment

    def unfold(segments: Seq[Fragment]): Seq[String] = {
      segments match {
        case Fragment.Keep(v) +: rest =>
          unfold(rest).map(unfolded => v + unfolded)
        case Fragment.Expand(vs) +: rest =>
          vs.flatMap(v => unfold(rest).map(unfolded => v + unfolded))
        case Seq() => Seq("")
      }
    }
  }

  private def parseBraceExpansion(input: String) = {
    val braceExpansion =
      P("{" ~/ CharsWhile(c => c != ',' && c != '}').!.rep(2, sep = ",") ~/ "}")
        .map(Fragment.Expand)

    val containBraces =
      P("{" ~ CharsWhile(c => c != ',' && c != '}').rep ~ "}").!.map(
        Fragment.Keep)

    val other = P(CharsWhile(c => c != '{' && c != '}')).!.map(Fragment.Keep)

    val split = (containBraces | braceExpansion | other).rep

    val parser = split.map(Fragment.unfold)

    parser.parse(input)
  }

  def extractSegments(
      selectorString: String): Either[String, List[Mirror.Segment]] =
    parseSelector(selectorString) match {
      case f: Parsed.Failure           => Left(s"Parsing exception ${f.msg}")
      case Parsed.Success(selector, _) => Right(selector)
    }

  private def parseSelector(input: String) = {
    val segment =
      P(CharsWhileIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).!).map(
        Mirror.Segment.Label
      )
    val crossSegment =
      P("[" ~ CharsWhile(c => c != ',' && c != ']').!.rep(1, sep = ",") ~ "]")
        .map(
          Mirror.Segment.Cross
        )
    val query = P(segment ~ ("." ~ segment | crossSegment).rep ~ End).map {
      case (h, rest) => h :: rest.toList
    }
    query.parse(input)
  }

}
