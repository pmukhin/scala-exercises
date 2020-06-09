import scala.annotation.tailrec

object MultiplyStrings

object Solution {
  object multiply {
    def apply(x: String, y: String): String =
      (x, y) match {
        case ("0", _) => "0"
        case (_, "0") => "0"
        case _ =>
          new String(
            solutionOnLists(
              x.toCharArray.toList,
              y.toCharArray.toList
            ).toArray
          )
      }

    private def charToInt(c: Char): Int =
      c.toInt - diff

    private def intToChar(i: Int): Char =
      (i + diff).toChar

    private val diff = 48

    private def solutionOnLists(x: List[Char], y: List[Char]): List[Char] = {
      val lists = y.reverse.zipWithIndex.map {
        case (yVal, i) =>
          recX(x, yVal, List.empty, 0, i)
      }
      sumLists(lists, List.empty, 0)
    }

    @tailrec def sumLists(lists: List[List[Char]], res: List[Char], addition: Int): List[Char] =
      lists match {
        case _ if lists.forall(_.isEmpty) && addition == 0 =>
          res

        case _ if lists.forall(_.isEmpty) =>
          val additionChar = intToChar(addition)
          additionChar +: res

        case _ =>
          val lastsSum = lists.filter(_.nonEmpty).map(_.last).map(charToInt).sum + addition
          if (lastsSum > 9) {
            val sumRight     = lastsSum % 10
            val sumLeft      = (lastsSum - sumRight) / 10
            val sumRightChar = intToChar(sumRight)
            val updatedRes   = sumRightChar +: res

            sumLists(lists.map(_.dropRight(1)), updatedRes, sumLeft)
          } else {
            val char       = intToChar(lastsSum)
            val updatedRes = char +: res
            sumLists(lists.map(_.dropRight(1)), updatedRes, 0)
          }
      }

    @tailrec def recX(
      x: List[Char],
      y: Char,
      res: List[Char],
      added: Int,
      appendZeroes: Int
    ): List[Char] =
      x match {
        case Nil if res.nonEmpty && added > 0 =>
          intToChar(added) +: res :++ List.fill(appendZeroes)('0')

        case Nil if res.nonEmpty =>
          res :++ List.fill(appendZeroes)('0')

        case _ =>
          val yInt = charToInt(y)
          val xInt = charToInt(x.last)
          val mlt  = (yInt * xInt) + added

          if (mlt < 10) {
            val mltChar = intToChar(mlt)
            recX(x.dropRight(1), y, mltChar +: res, 0, appendZeroes)
          } else {
            val mltLeft  = mlt % 10
            val mltRight = (mlt - mltLeft) / 10
            val mltChar  = intToChar(mltLeft)
            val newRes   = mltChar +: res

            recX(x.dropRight(1), y, newRes, mltRight, appendZeroes)
          }
      }
  }
}
