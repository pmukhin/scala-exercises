import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object ParenthesesSequenceCorrectness {

  object solution {

    private def isOpening(ch: Char): Boolean =
      ch match {
        case '(' => true
        case '{' => true
        case '[' => true
        case _   => false
      }

    private def isClosing(ch: Char): Boolean =
      ch match {
        case ')' => true
        case '}' => true
        case ']' => true
        case _   => false
      }

    private def isPairing(x: Char, y: Char): Boolean =
      (x, y) match {
        case ('(', ')') => true
        case ('{', '}') => true
        case ('[', ']') => true
        case _          => false
      }

    private def getPair(x: Char): Char =
      x match {
        case '(' => ')'
        case '{' => '}'
        case '[' => ']'
        case ']' => '['
        case ')' => '('
        case '}' => '{'
        case _   => throw new RuntimeException("unreachable")
      }

    def isBalanced(s: String): Boolean =
      isBalanced(s.toCharArray.toList)

    // a classic var based implementation for checks
    def isBalanced(s: List[Char]): Boolean = {
      var st = ListBuffer.empty[Char]
      for (ch <- s) {
        if (isClosing(ch)) {
          if (!isPairing(st.last, ch)) {
            return false
          } else {
            st = st.dropRight(1)
          }
        } else {
          st = st.addOne(ch)
        }
      }
      st.isEmpty
    }

    private object insertWherePossible {
      def apply(history: List[Char], x: Char): List[Char] = {
        val last  = history.last
        val left  = history.dropRight(1)
        val right = List(last, x)
        val stack = List(last)

        rec(left, right, stack, x)
      }

      @tailrec
      private def rec(left: List[Char], right: List[Char], stack: List[Char], x: Char): List[Char] =
        stack match {
          // if stack is empty, the correct slot for opening pair of X is found
          case Nil =>
            val pair = getPair(x)
            left :+ pair :++ right

          // the closing char is found, putting it to the stack, saving in the right part
          case _ if left.nonEmpty && isClosing(left.last) =>
            val leftLast      = left.last
            val stackWithChar = stack :+ leftLast
            rec(left.dropRight(1), leftLast +: right, stackWithChar, x)

          // the opening is found, so popping one from the stack
          // no validation like isPair(x, y) is used since the history should always be correct
          case _ if left.nonEmpty =>
            rec(left.dropRight(1), left.last +: right, stack.dropRight(1), x)

          // we can't be here
          case _ =>
            throw new IllegalStateException()
        }
    }

    @tailrec
    private def rec(stack: List[Char], history: List[Char], charSeq: List[Char]): List[Char] =
      charSeq match {
        // if a space is encountered, just skipping and carrying on
        case x :: xs if x == ' ' =>
          rec(stack, history, xs)

        // opening char is found, just pushing it
        // to the stack and the history
        case x :: xs if isOpening(x) =>
          rec(stack :+ x, history :+ x, xs)

        // closing char is found pairing with the head of the stack
        case x :: xs if isClosing(x) && stack.nonEmpty && isPairing(stack.last, x) =>
          rec(stack.dropRight(1), history :+ x, xs)

        // closing char is found not pairing!:
        // stack is not empty, so we can't just append (pairOfX(x), x) to the history
        // we need to get opening pair, insert it somewhere into
        // the history and then insert the found char
        case x :: xs if isClosing(x) && stack.nonEmpty =>
          val fixedHistory = insertWherePossible(history, x)
          rec(stack, fixedHistory, xs)

        // a closing char with an empty stack
        // just putting opening pair and then X
        case x :: xs if isClosing(x) && stack.isEmpty =>
          val fixedHistory = history :+ getPair(x) :+ x
          rec(stack, fixedHistory, xs)

        // all went well, we are done
        case Nil if stack.isEmpty =>
          history

        // one little thing: the stack is not empty
        // just adding a closing pair and pop _ from the stack
        case Nil =>
          val pair = getPair(stack.last)
          rec(stack.dropRight(1), history :+ pair, Nil)
      }

    def apply(x: String): String = {
      val stack   = List.empty[Char]
      val history = List.empty[Char]
      val charSeq = x.toCharArray.toList
      val result  = rec(stack, history, charSeq)

      new String(result.toArray)
    }
  }
}
