import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ListBuffer

class ParenthesesSequenceCorrectnessSpec extends AnyFunSpec with Matchers {
  import ParenthesesSequenceCorrectness.solution

  def isBalanced(s: String): Boolean =
    isBalanced(s.toCharArray.toList)

  // a classic var based implementation for checks
  def isBalanced(s: List[Char]): Boolean = {
    import solution.{isClosing, isPairing}
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

  describe("if correct seq given") {
    it("should be unchanged") {
      solution("()") shouldBe "()"
      solution("(())") shouldBe "(())"
      solution("[()]") shouldBe "[()]"
      solution("([{()}])") shouldBe "([{()}])"
      solution("()[]{}") shouldBe "()[]{}"
    }
  }

  describe("if incorrect seq given") {
    describe("with an opening char") {
      it("should get corrected with a closing char") {
        solution("(") shouldBe "()"
        solution("((") shouldBe "(())"
        solution("[[") shouldBe "[[]]"
      }
    }
    describe("with a closing char") {
      it("should get corrected with an opening char") {
        solution(")") shouldBe "()"
        solution("))") shouldBe "()()"
        solution("]]") shouldBe "[][]"
      }
    }
    describe("with a mixed in closing char") {
      it("should get corrected with an opening char") {
        isBalanced(solution("[[])]")) shouldBe true
        isBalanced(solution("[[]])")) shouldBe true
      }
    }
  }
}
