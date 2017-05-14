import org.scalatest.FunSuite

class ManacherSuite extends FunSuite {

  test("W pustym stringu nie ma palindromów") {
    assert(Solution.solution("") == 0)
  }

  test("Pojedynczy znak to nie palindrom") {
    assert(Solution.solution("a") == 0)
  }

  test("Jeden najprostszy palindrom") {
    assert(Solution.solution("aa") == 1)
  }

  test("Cztery takie same znaki to 6 palindromów") {
    assert(Solution.solution("aaaa") == 6)
  }

  test("Przykładowy test z codility") {
    assert(Solution.solution("baababa") == 6)
  }

  test("String w którym jest więcej niż 100000000 palindromów") {
    val longString = (for {i <- 0 to 20000} yield 'a').mkString
    assert(Solution.solution(longString) == -1)
  }

  test("Wcale nie palindrom") {
    assert(Solution.solution("abcdefghijklmnoprstuwvxyzabcdefghijklmnoprstuwvxyz") == 0)
  }
}