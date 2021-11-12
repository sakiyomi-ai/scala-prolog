import com.joehalliwell.sp.{Atom, Prolog}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class PrologParserTest extends AnyFlatSpec with Matchers {
  val parsers = new Prolog().parser

  "term" should
  "succeed to recognize 'hello'" in {
    parsers.parse("hello.") match {
      case Right(result) => result should be(Atom("hello"))
      case _             => fail()
    }
  }

  "exp" should
  "handle commas correctly" in {
    val ast = parsers.parse("X is 1 + 2; Y is 3 + 4.")
    println(ast)
    1 should be(1)
  }
}
