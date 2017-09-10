import org.scalatest._

class SearchSpec extends FlatSpec with Matchers {

  "Parsing a textfile line" should "only return words from a string" in {
    assert(SimpleSearch.parse("simpleword").toList == List("simpleword"))
    assert(SimpleSearch.parse("simple_word").toList == List("simpleword"))
    assert(SimpleSearch.parse("simple_Word hi!.3 hi").toList == List("simpleword", "hi", "3", "hi"))
    assert(SimpleSearch.parse("simple_word 2\n3 Hi").toList == List("simpleword", "2", "3", "hi"))
    assert(SimpleSearch.parse("THIs IS my Test!. What?").toList == List("this", "is", "my", "test", "what"))
    assert(SimpleSearch.parse("last-in-first-out lifo\t").toList == List("last", "in", "first", "out", "lifo"))
  }

  "An addition operation on the index" should "allow to add new words to index" in {
    // -- simple example from empty map
    assert(
      Map("hello" -> Set("1.txt"), "world" -> Set("1.txt")) ==
        SimpleSearch.add("1.txt", "hello world".split(" ").toIterator, Map.empty)
    )
  }

  it should "maintain unique references to each word" in {
    val existingMap1 = Map("hello" -> Set("1.txt"), "world" -> Set("1.txt"))
    assert(
      Map("hello" -> Set("1.txt"), "world" -> Set("1.txt"), "hi" -> Set("1.txt")) ==
        SimpleSearch.add("1.txt", "hi world".split(" ").toIterator, existingMap1)
    )
  }

  it should "allow to have multiple unique references to each word" in {
    val existingMap2 = Map("hello" -> Set("1.txt"), "world" -> Set("1.txt"))
    assert(
      Map("hello" -> Set("1.txt"), "world" -> Set("1.txt", "2.txt"), "hi" -> Set("2.txt")) ==
        SimpleSearch.add("2.txt", "hi world".split(" ").toIterator, existingMap2)
    )
  }

  "A search operation" should "return the files containing any of the searched words" in {
    val words1 = "my cat is green".split(" ").toSet
    val index1 = Map("cat" -> Set("1.txt", "2.txt"), "green" -> Set("1.txt"), "my" -> Set("1.txt"))
    assert(
      SimpleSearch.search(words1, index1) ==
        Map("1.txt" -> 3, "2.txt" -> 1)
    )

    val words2 = "my cat is green".split(" ").toSet
    val index2 = Map("dog" -> Set("1.txt", "2.txt"), "green" -> Set("1.txt"), "my" -> Set("1.txt"))
    assert(
      SimpleSearch.search(words2, index2) ==
        Map("1.txt" -> 2)
    )
  }

  "A ranking method" should "return an ordered sequence according to percentage matched" in {
    val words = "my cat is green".split(" ").toSet
    val occurrences = Map("1.txt" -> 3, "2.txt" -> 1)
    assert(Seq(("1.txt" -> 75), ("2.txt" -> 25)) == SimpleSearch.rank(words, occurrences))
  }
}
