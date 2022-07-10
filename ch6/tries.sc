// A trie is a tree-like data structure (also called a prefix tree)
// which behaves similarly to a Set[String], but with some extra sauce.
// Eg: you can use something like .prefixesMatchingString(s: String) to find all strings
// in a trie which are a prefix of the string s.
// Inversely, you can use .stringsMatchingPrefix(s: String) to find all strings in the trie
// which contain s as a prefix. Useful for things like autocompletes.

// A trie consists of nodes, each one containing a Map from a char to a child node.
// if hasValue is false, that means that this node represents the end of a string
// eg, with x representing hasValue = false: [] - m -> [] - a ->  [] - n -> [x]
//                                                                |- p -> [x]
class Trie() {
  class Node(
      var hasValue: Boolean,
      val children: collection.mutable.Map[Char, Node] =
        collection.mutable.Map()
  )
  val root = new Node(false)
}
