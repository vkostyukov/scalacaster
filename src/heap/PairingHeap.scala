/**
 *
 * -Notes-
 *
 * Pairing heaps are heap-ordered multiway tree structures, and can be considered simplified Fibonacci heaps.
 * The analysis of pairing heaps' time complexity was initially inspired by that of splay trees. The amortized
 * time per delete-min is O(log n). The operations find-min, merge, and insert run in constant time, O(1).
 *
 * Wikipedia: http://en.wikipedia.org/wiki/Pairing_heap
 * The Pairing-Heap: A New Form of Self-Adjusting Heap: http://www.cs.cmu.edu/~sleator/papers/pairing-heaps.pdf
 * Improved upper bounds for pairing heaps: http://john2.poly.edu/papers/swat00/paper.pdf
 * Union-Based Heaps: https://speakerdeck.com/kachayev/union-based-heaps?slide=28
 *
 */

abstract sealed class Heap[+A <% Ordered[A]] {

  /**
   * Min value of this heap.
   */
  def min: A

  /**
   * Subtrees (child of this heap).
   */
  def childs: List[Heap[A]]

  /**
   * Whether this heap is empty or not.
   */
  def isEmpty: Boolean

  /**
   * The 'insert' function might be defined through the 'Heap.merge' function
   */
  def insert[B >: A <% Ordered[B]](x: B): Heap[B] =
    Heap.merge(Heap.make(x), this)

  /**
   * Removes the minimum element from this heap.
   */
  def remove: Heap[A] = Heap.pairing(childs)

  /**
   * Fails with message.
   */
  def fail(m: String) = throw new NoSuchElementException(m)

}

/**
 * Empty node representation
 */
case object Leaf extends Heap[Nothing] {
  def min: Nothing = fail("An empty heap.")
  def childs: List[Heap[Nothing]] = fail("An empty heap.")
  def isEmpty = true
}

/**
 * Non-empty node is an element with left and right childs (Skew Heaps)
 */
case class Branch[A <% Ordered[A]](min: A, childs: List[Heap[A]]) extends Heap[A] {
  def isEmpty = false
}

object Heap {

  /**
   * An empty heap.
   */
  def empty[A]: Heap[A] = Leaf

  /**
   * Makes a heap node.
   */
  def make[A <% Ordered[A]](x: A, subs: List[Heap[A]] = List[Heap[A]]()) =
    Branch(x, subs)

  /**
   * Merges two given heaps. Also known as "union".
   */
  def merge[A <% Ordered[A]](x: Heap[A], y: Heap[A]): Heap[A] = (x, y) match {
    case (_, Leaf) => x
    case (Leaf, _) => y
    case (Branch(x1, subs1), Branch(x2, subs2)) =>
      if (x1 < x2) Branch(x1, Branch(x2, subs2) :: subs1)
      else Branch(x2, Branch(x2, Branch(x1, subs1) :: subs2))
  }

  def pairing[A <% Ordered[A]](subs: Heap[A]): Heap[A] = subs match {
    case Nil => Leaf
    case hd :: Nil => hd
    case h1 :: h2 :: tail => pairing(merge(h1, h2) :: tail)
  }
  
  /**
   * Builds a skew heap from an unordered linked list.
   */
  def fromList[A <% Ordered[A]](ls: List[A]): Heap[A] = {
    def loop(hs: List[Heap[A]]): Heap[A] = hs match {
      case hd :: Nil => hd
      case _ => loop(pass(hs))
    }

    def pass(hs: List[Heap[A]]): List[Heap[A]] = hs match {
      case hd :: nk :: tl => Heap.merge(hd, nk) :: pass(tl)
      case _ => hs
    }

    if (ls.isEmpty) Heap.empty
    else loop(ls.map(Heap.make(_)))
  }

}