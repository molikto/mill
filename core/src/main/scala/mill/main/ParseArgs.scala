package mill.main

import mill.discover.Mirror
import mill.util.EitherOps
import fastparse.all._

object ParseArgs {

  def apply(scriptArgs: Seq[String])
    : Either[String, (List[List[Mirror.Segment]], Seq[String])] = {
    val (selectorStrings, args) = extractSelsAndArgs(scriptArgs)
    for {
      _ <- validateSelectors(selectorStrings)
      expandedSelectors <- EitherOps
        .sequence(selectorStrings.map(expandBraces))
        .map(_.flatten)
      selectors <- EitherOps.sequence(expandedSelectors.map(extractSegments))
    } yield (selectors.toList, args)
  }

  def extractSelsAndArgs(
      scriptArgs: Seq[String]): (Seq[String], Seq[String]) = {
    val multiFlags = Seq("--all", "--seq")
    val isMultiSelectors = scriptArgs.exists(multiFlags.contains)

    if (isMultiSelectors) {
      val filtered = scriptArgs.filterNot(multiFlags.contains)
      val dd = filtered.indexOf("--")
      if (dd == -1) filtered -> Seq.empty
      else filtered.take(dd) -> filtered.drop(dd + 1)
    } else {
      scriptArgs.take(1) -> scriptArgs.drop(1)
    }
  }

  private def validateSelectors(selectors: Seq[String]): Either[String, Unit] =
    if (selectors.isEmpty || selectors.exists(_.isEmpty))
      Left("Selector cannot be empty")
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

    def unfold(segments: Seq[Fragment]): Seq[Seq[String]] = {
      segments match {
        case Fragment.Keep(v) +: rest =>
          unfold(rest).map(unfolded => v +: unfolded)
        case Fragment.Expand(vs) +: rest =>
          vs.flatMap(v => unfold(rest).map(unfolded => v +: unfolded))
        case Seq() => Seq(Seq.empty)
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

    val other = P(CharsWhile(c => c != '{')).!.map(Fragment.Keep)

    val split = (containBraces | braceExpansion | other).rep

    val parser = split.map(e => Fragment.unfold(e).map(_.mkString))

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
