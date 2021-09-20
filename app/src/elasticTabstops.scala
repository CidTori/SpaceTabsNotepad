import scala.annotation.tailrec
import scala.collection.SortedSet


package object elasticTabstops {

  // convenience functions to wrap Java's unintuitive split method
  def split(string: String, char: Char) = string.split(char.toString, -1)  // -1 so we get trailing empty strings
  def splitAndStrip(string: String, regex: String) = string.split(regex)

  // Process runs of Some in list.
  // scala>    processAdjacent((l: List[Option[Int]]) => List.fill(l.length)(Some(l.flatten.max)),
  //                           List(Some(1), Some(2), None, Some(4), Some(5)))
  // res0: List[Option[Int]] = List(Some(2), Some(2), None, Some(5), Some(5))
  @tailrec
  private def processAdjacent[A](unprocessed: List[Option[A]], processed: List[Option[A]] = Nil)
                                (process: List[Option[A]] => List[Option[A]]): List[Option[A]] =
    unprocessed match {
      case Nil => processed
      case head :: _ => {
        val (run, stillUnprocessed) = unprocessed.span(_.isDefined == head.isDefined)
        val newProcessed = if (head.isDefined) process(run) else run
        processAdjacent(stillUnprocessed, processed ::: newProcessed)(process)
      }
    }

  // Replace each item in a run with its highest value.
  // scala>        maxAdjacent(List(Some(1), Some(2), None, Some(4), None, None, Some(7), Some(8), Some(9)))
  // res0: List[Option[Int]] = List(Some(2), Some(2), None, Some(4), None, None, Some(9), Some(9), Some(9))
  private def maxAdjacent(column: List[Option[Int]]): List[Option[Int]] = {
    processAdjacent(column) { l =>
      List.fill(l.length)(Some(l.flatten.max))
    }
  }

  private def calcMaxedWidthsPerLine(widthsPerLine: List[List[(Int, Int)]]) : List[List[Int]] = {
    val maxSpaceTab = (widthsPerLine.flatMap(_.map(_._1)) :+ 0).max
    val tabstopsPerLine2 = widthsPerLine.map(l => l
      .scanLeft(List.fill(maxSpaceTab+1)(0))((acc, v) => List.fill(v._1)(0) ::: ((acc(v._1)+1) +: acc.drop(v._1+1)))
      .drop(1)
      .map(_.reverse)
      .zip(l.map(_._2))
    )
    val allTabstops2 = tabstopsPerLine2.flatten.map(_._1).distinct.sortWith((left, right) => left.zip(right)
      .foldLeft((false, false))((acc, v) => (!acc._2 && (acc._1 || v._1 < v._2), !acc._1 && (acc._2 || v._1 > v._2)))._1)
    return allTabstops2.foldLeft(List(List.fill(tabstopsPerLine2.length)(Option.empty[Int])))((acc, v) => {
      val lengthsPerLineThisTabstop = tabstopsPerLine2.map(_.find(t => t._1 == v).map(_._2))

      val previousTabstopsPerLine = acc.transpose.map(_.flatten.lastOption)
      val vvv = previousTabstopsPerLine.zip(lengthsPerLineThisTabstop).map {
        case (Some(previousTabstop), Some(length)) => Some(previousTabstop + length)
        case (None, Some(length)) => Some(length)
        case (_, None) => None
      }
      acc :+ maxAdjacent(vvv)
    }).drop(1).transpose.map(_.flatten).map(l => l.zip(0 +: l.dropRight(1)).map(pair => pair._1 - pair._2))

    val tabstopsPerLine = widthsPerLine.map(l => if (l.isEmpty) Nil else l.drop(1).scanLeft((0, l.head._1, l.head._2))((acc, v) => (if (v._1 <= acc._2) acc._1 + 1 else acc._1, v._1, v._2)))
    println("tabstopsPerLine: "+tabstopsPerLine)
    val allTabstops = tabstopsPerLine.flatten.map(c => (c._1, c._2)).distinct.sorted
    println("allTabstops: "+allTabstops)
    allTabstops.foldLeft(List(List.fill(tabstopsPerLine.length)(Option.empty[Int])))((acc, v) => {
      val lengthsPerLineThisTabstop = tabstopsPerLine.map(_.find(t => t._1 == v._1 && t._2 == v._2).map(_._3))

      val previousTabstopsPerLine = acc.transpose.map(_.flatten.lastOption)
      val vvv = previousTabstopsPerLine.zip(lengthsPerLineThisTabstop).map {
        case (Some(previousTabstop), Some(length)) => Some(previousTabstop + length)
        case (None, Some(length)) => Some(length)
        case (_, None) => None
      }
      acc :+ maxAdjacent(vvv)
    }).drop(1).transpose.map(_.flatten).map(l => l.zip(0 +: l.dropRight(1)).map(pair => pair._1 - pair._2))
  }

  private def measureWidthsPerLine(cellsPerLine: List[List[(Int, String)]], measureText: String => Int): List[List[(Int, Int)]] =
    cellsPerLine.map(_.map(p => (p._1, measureText(p._2))))

  def calcElasticTabstopPositions(cellsPerLine: List[List[(Int, String)]], measureText: String => Int): List[List[Int]] = {
    val cellWidthsPerLine = measureWidthsPerLine(cellsPerLine, measureText)

    calcMaxedWidthsPerLine(cellWidthsPerLine).map(_.scanLeft(0)(_ + _).drop(1))
  }

  def spaceTabsToSpaces(text: String, nofIndentSpaces: Int): String = {
    val cellPaddingWidthSpaces = 2  // must be at least 2 so we can convert back to tabs
    val cellMinimumWidthSpaces = nofIndentSpaces - cellPaddingWidthSpaces
    val cellsPerLine = split(text, '\n').map(splitAndStrip(_, "( *)\t").toList).toList
    val cellsPerLine2 = split(text, '\n').map("((?:[^\t]*[^ \t])?)( *)\t".r.findAllMatchIn(_).toList.map(m => (m.group(2).length, m.group(1)))).toList
    def calcCellWidth(text: String): Int = math.max(text.length, cellMinimumWidthSpaces) + cellPaddingWidthSpaces
    val maxedWidthsPerLine = calcMaxedWidthsPerLine(measureWidthsPerLine(cellsPerLine2, calcCellWidth))

    maxedWidthsPerLine.zip(cellsPerLine).map { case (widthsThisLine, cellsThisLine) =>
      cellsThisLine.zip(widthsThisLine :+ 0).map { case (cellText, width) =>
        cellText + (" " * (width - cellText.length))
      }.mkString
    }.mkString("\n")
  }

  // Replace each item in a run with None if all of its contents are empty strings.
  // scala>      nullifyEmptyRuns(List(Some(""), Some("a"), None, Some(""), Some("")))
  // res0: List[Option[String]] = List(Some(""), Some("a"), None, None,     None))
  private def nullifyEmptyRuns(column: Array[Option[(Int, String)]]): Array[Option[(Int, String)]] = {
    processAdjacent(column.toList) { l =>
      if (l.forall(_.exists(_._2.isEmpty))) List.fill[Option[(Int, String)]](l.length)(None) else l
    }.toArray
  }

  private def getMatchesPerLine(lines: Array[String], nofIndentSpaces: Int): Array[Map[Int, String]] = {
    // a non-space followed by any number of chars that are either a non-space or a space followed by a non-space
    val cellTextRegEx = "[^ ](?:[^ ]| (?=[^ ]))*".r

    // get maps for each line containing the position of text and the text itself
    lines.map(s => cellTextRegEx.findAllMatchIn(s).map(m => m.start -> m.matched).toMap)
  }

  private def getPossCellsFromText(lines: Array[String], nofIndentSpaces: Int): Array[Array[Option[(Int, String)]]] = {
    val matchesPerLine = getMatchesPerLine(lines, nofIndentSpaces)
    val positionsPerLine = matchesPerLine.map(_.keys)
    val maybeLastPosPerLine = positionsPerLine.map(_.toList.maxOption)

    // get sorted and unique possible cell positions using the flattened positions as varargs
    val allPositions = SortedSet(positionsPerLine.flatten: _*).toArray

    // create Options at every possible cell position
    allPositions.map { pos =>
      matchesPerLine.zip(lines).map { case (matchesThisLine, line) =>
        if (matchesThisLine.contains(pos)) Some((pos, matchesThisLine(pos)))
        else None
      }
    }
  }

  def spacesToSpaceTabs(text: String, nofIndentSpaces: Int): String = { // FIXME
    // split text into lines prepended with a non-whitespace character (so the first cell of each line is not empty)
    val lines = split(text, '\n').map("|" + _)

    val possCellsPerCol = getPossCellsFromText(lines, nofIndentSpaces)

    // replace empty columns with Nones, transpose, remove Nones, and join with tabs
    val textPerLine = possCellsPerCol.toList.map(nullifyEmptyRuns)
      .transpose.map(l => {
        Some((0, l.head.get._2)) +: l.drop(1).map(_.map(t => {
          val previousCell = l.takeWhile(cc => cc.isEmpty || cc.get._1 != t._1).flatten.last
          val nofSpaces = t._1 - (previousCell._1 + previousCell._2.length)
          val indentationLevel = if (nofSpaces % nofIndentSpaces == 0) nofSpaces / nofIndentSpaces else 0
          (indentationLevel, t._2)
        }))
      }).transpose
      .map(processAdjacent(_) { run =>
        if (run.head.isDefined && !run.forall(_.get._1 == run.head.get._1)) run.map(_.map(p => (0, p._2)))
        else run
      })
      .zipWithIndex.map(col => col._1.map(_.map(p => (col._2, p._1, p._2))))
      .transpose.map(_.flatten)
      .map(_.map(t => {
        if (t._2 != 0) "\t" * t._2 + t._3
        else if (t._1 != 0 && t._3.nonEmpty) " " * t._1 + "\t" + t._3
        else t._3
      }).mkString)

    // finally, drop previously prepended non-whitespace character from each line and join with newlines
    textPerLine.map(_.drop(1)).mkString("\n")
  }
}
