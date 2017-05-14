/** Zadanie rekrutacyjne Sentione 2017
  *
  * @author Andrzej Głuszak
  */

object Solution {
  type IntSeq = IndexedSeq[Int]
  val limitOfPalindromes = 100000000

  /** Oblicza ilość palindromów w słowie korzystając z algorytmu Manachera
    *
    * @param input słowo dla którego chcemy znaleźć ilość palindromów
    * @return ilość palindromów
    */
  def solution(input: String): Int = {
    //    doklejamy do stringa strażników i przerabiamy na przypadek o nieparzystej długości
    val s = input.toList.mkString("$|", "|", "|#")
    val lengths = for {c <- s} yield 0

    /** Rozszerza maksymalnie palindrom wokół zadanego znaku
      *
      * @param lengths tablica zawierająca długości palindromów dla każdego znaku w słowie
      * @param i       indeks znaku
      * @return tablicę uaktualnioną o długość rozszerzonego palindromu
      */
    def expand(lengths: IntSeq, i: Int): IntSeq = {
      if (s.charAt(i + 1 + lengths(i)) == s.charAt(i - 1 - lengths(i))) {
        val updatedlengths = lengths.updated(i, lengths(i) + 1)
        expand(updatedlengths, i)
      } else lengths
    }

    /** Pomocnicza funkcja rekurencyjna
      *
      * @param lengths           tablica zawierająca długości palindromów dla każdego znaku w słowie
      * @param i                 indeks znaku
      * @param center            indeks środka zewnętrznego palindromu
      * @param rightBound        indeks prawego końca zewnętrznego palindromu
      * @param palindromesNumber liczba palindromów znaleziona do tej pory
      * @return parę, której pierwszym elementem jest uaktualniona tablica lengths, a drugim ilość palindromów
      */
    //    lengths to długości palindromów o środku w i, i to indeks w stringu
    //    center to , rightBound to
    def aux(lengths: IntSeq, i: Int, center: Int, rightBound: Int, palindromesNumber: Int): (IntSeq, Int) = {
      //      jeśli przekroczyliśmy limit to nie liczymy dalej
      if (palindromesNumber >= limitOfPalindromes) {
        (lengths, -1)
        //      przerywamy rekurencję po dojściu do końca stringa
      } else if (i == s.length - 1) {
        (lengths, palindromesNumber)
      } else {
        val mirror = 2 * center - i
        //        jeśli jesteśmy wewnątrz zewnętrznego palindromu to możemy przekopiować długość palindromu
        //        z pozycji odbitej względem środka jako minimalną, chyba że jesteśmy ograniczeni prawym końcem
        val lengthsUpdated = if (i < rightBound) {
          val min = Math.min(rightBound - i, lengths(mirror))
          lengths.updated(i, min)
        } else lengths
        //        rozszerzamy jak najdalej obecny palindrom
        val lengthsExpanded = expand(lengthsUpdated, i)
        //        aktualizujemy środek i prawy koniec zewnętrznego palindromu jeśli go przekroczyliśmy
        if (i + lengthsExpanded(i) > rightBound) {
          aux(lengthsExpanded, i + 1, i, i + lengthsExpanded(i), palindromesNumber + (lengthsExpanded(i) / 2))
        } else {
          aux(lengthsExpanded, i + 1, center, rightBound, palindromesNumber + (lengthsExpanded(i) / 2))
        }
      }
    }

    aux(lengths, 1, 0, 0, 0)._2
  }
}
