import java.io.File

import scala.io.{Source, StdIn}


object SearchApplication extends App {

  /** List text files in a given directory */
  def listTextFiles(dir: File): Seq[File] = {
    dir.listFiles.filter(_.getName.endsWith(".txt"))
  }

  /** Defines expression(s) to quit the search console */
  def isQuit(s: String): Boolean = {
    s == ":quit"
  }

  // -- console interaction
  args match {
    case Array(dirname, dirnames @ _ *) => {

      val dirs = (dirname +: dirnames)
      val files = dirs.map(dir => new File(dir)).filter(_.exists()).flatMap(file => listTextFiles(file))
      val index = SimpleSearch.add(files, Map.empty[String, Set[String]])
      println(files.size + " files read in dir(s) [" + (dirname +: dirnames).mkString(", ") + "]")

      var continue = true
      while (continue) {
        print("search> ")
        StdIn.readLine match {
          case x if isQuit(x) => println("bye!"); continue = false
          case x => {
            val searchWords = SimpleSearch.parse(x).toSet
            val occurrences = SimpleSearch.search(searchWords, index)
            val rankedFiles = SimpleSearch.rank(searchWords, occurrences)

            rankedFiles.take(10).foreach {
              case (filename, score) => println(s"$filename : $score %" )
            }

            if (rankedFiles.isEmpty) {
              println("no matches found")
            }

          }
        }
      }
    }
    case _ => println("Usage: Need directory name(s)")
  }
}

/** Contains all the methods to build the index, perform the search etc. */
object SimpleSearch
{
  /** Words index type alias just to simplify function signatures */
  type WordsIndex = Map[String, Set[String]]

  /** Parses a string returning an iterator over words only */
  def parse(s: String): Iterator[String] = {
    val wordRegex = raw"(\w+)".r
    wordRegex.findAllIn(s).map(_.replaceAll("_", "").toLowerCase)
  }

  /** Parses file returning words iterator */
  def parse(file: File): Iterator[String] = {
    Source.fromFile(file).getLines flatMap parse
  }

  /** Function to get a index including the given sequence of words
    *
    * @param filename   name of the file we are indexing
    * @param words      Iterator over words to be indexed
    * @param index      Inverted index as a map of (word -> filename(s)) used as initial state
    * @return           Inverted index containing a map of (word -> filename(s)) with given words included
    */
  def add(filename: String, words: Iterator[String], index: WordsIndex): WordsIndex = {
    words.foldLeft(index)((acc, word) =>
      if (acc.contains(word)) {
        acc + (word -> (acc(word) + filename))
      } else {
        acc + (word -> Set(filename))
      }
    )
  }

  /** Function to add file content to an inverted index
    *
    * @param file       Name of the file we are indexing
    * @param index      Inverted index as a map of (word -> filename(s)) used as initial state
    * @return           Inverted index containing a map of (word -> filename(s)) with given words included
    */
  def add(file: File, index: WordsIndex): WordsIndex = {
    val iterator = parse(file)
    val indexedMap = add(file.getName, iterator, index)
    indexedMap
  }

  /** Function to add text files content to an inverted index
    *
    * @param files      Sequence of files to be indexed
    * @param index      Inverted index as a map of (word -> filename(s)) used as initial state
    * @return           Inverted index containing a map of (word -> filename(s)) with given words included
    */
  def add(files: Seq[File], index: WordsIndex): WordsIndex = {
    files.foldLeft(index)((map, file) => {add(file, map)})
  }

  /** Function to search a set of words in the index
    *
    * @param words      Set of unique words to search in index
    * @param index      Inverted index as a map of (word -> filename(s))
    * @return           Returns a map of (filename -> number of words found from the given search set)
    */
  def search(words: Set[String], index: WordsIndex): Map[String, Int] = {
    words.foldLeft(Map.empty[String, Int])((acc, word) => {
      if (index.contains(word)) {
        index(word).foldLeft(acc)( (acco, f) =>
          if (acco.contains(f)) {
            acco + (f -> (acco(f) + 1))
          } else {
            acco + (f -> 1)
          }
        )
      } else {
        acc
      }
    })
  }

  /** Function to rank files based on how many word matches they have
    *
    * @param searchWords    Set of words we are trying to match
    * @param occurrences    Occurrences map as (filename -> number of words found)
    * @return               Ordered sequence of tuples (filename -> matched percentage)
    */
  def rank(searchWords: Set[String], occurrences: Map[String, Int]): Seq[(String, Int)] = {
    val rankedFiles = occurrences.map{ case (f, num) => (f, math.ceil(num * 100 / searchWords.size).toInt)}
    rankedFiles.toSeq.sortWith(_._2 > _._2)
  }
}
