package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences =  w.groupBy(_.toLower).mapValues[Int](_.size).toList.sorted

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    def sumOccurrances(acc:Occurrences, occ: List[Occurrences]) : Occurrences = {
      occ match {
        case Nil => acc
        case occurance :: tail => {
          var newAcc = acc.toMap
          for ((c, o) <- occurance) {
            newAcc = newAcc.updated(c, newAcc.getOrElse(c, 0) + o)
          }
          sumOccurrances(newAcc.toList, tail)
        }
      }
    }
    sumOccurrances(List[(Char, Int)](), s.map(wordOccurrences)).sorted
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {

    val words = dictionary
    var map = Map[Occurrences, List[Word]]()
    for(word:Word <- words){
      val occ:Occurrences = this.wordOccurrences(word)
      if (map.contains(occ))
        map = map.updated(occ, word::map.getOrElse(occ, List[Word]()))
      else
        map += occ->List(word)
    }
    map
  }


  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.get(wordOccurrences(word)).filterNot(_ == word).getOrElse(List[Word]()).sorted

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
    *
    *  lets start with a2 b2 -- we filter out all the nonzero occ of this.. we get a2 b2 lets add to acc
    *  we then subtract first element to get a1 b2..
    *  so we have a2b2 in our bag and try a1b2 and a2b1 etc get the combos for that and merge
    *
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def addOnCombo(acc: List[Occurrences] , occ: Occurrences) : List[Occurrences]={
      val nonZeroOccur:Occurrences = occ.filterNot(_._2 == 0)
      if(nonZeroOccur.isEmpty)
        return acc
      var newAcc = nonZeroOccur :: acc

      for(i <- nonZeroOccur.indices) yield {

        val item = nonZeroOccur(i)
        var newOccurMinusOne: Occurrences = nonZeroOccur.updated(i,(item._1,item._2-1) )
        if(newOccurMinusOne.head._2 == 0)
          newOccurMinusOne = newOccurMinusOne.take(i) ++ newOccurMinusOne.drop(i+1)
        newAcc = addOnCombo(newAcc  ,newOccurMinusOne)
      }
      newAcc

    }
    val emptyOcc:Occurrences = List[(Char, Int)]()
    addOnCombo(List[Occurrences](emptyOcc), occurrences)
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val sub = for (xi <- x) yield  (xi._1, xi._2 - y.find(_._1==xi._1).map(_._2).getOrElse(0))
    sub.filterNot(_._2 == 0).sorted
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = occurenceAnagrams(sentenceOccurrences(sentence))

  def occurenceAnagrams(occurrence: Occurrences): List[Sentence] = {

    val combinationsOccurrence = this.combinations(occurrence)
    /*var remaining = occ
    for (combo <- combinationsOccurrence) {
      remaining = this.subtract(remaining, combo)
    }*/
    val remaining = (occurrence /: combinationsOccurrence)(subtract)


    val remainingSentences = if(remaining.nonEmpty) occurenceAnagrams(remaining) else List[Sentence]()
    /*
    list of words that are buildable by this occurrence
     */
    def getSentence(numChars:Occurrences) : List[Sentence] = {
      val words : List[Word] = this.dictionaryByOccurrences.getOrElse(numChars, List[Word]())

      if(words.isEmpty)
        return List[Sentence]()

      def mergeSentences( words : List[Word], rs: List[Sentence]) : List[Sentence] = {

        val ret : List[Sentence] =
        if(words.isEmpty) {
          if(remainingSentences.isEmpty)
            List[Sentence]()
          else
            remainingSentences
        }else{
          if(remainingSentences.isEmpty)
            words.map(List[Word](_))
          else
            for {word <- words
               rs <- remainingSentences}
            yield word :: rs
        }
        ret
      }
      val mergedWords : List[Sentence] = mergeSentences(words,remainingSentences)
      println(s"got merged $mergedWords from $words and $remainingSentences ")
      mergedWords
    }
    val sentences = for (combo <- combinationsOccurrence) yield getSentence(combo)
    println(sentences + " having size " + sentences.size)

    sentences.flatten

  }
}
