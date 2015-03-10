/**
 *
 * -Notes-
 *
 * A skew heap is a self-adjusting form of a leftist heap which attempts to
 * maintain balance by unconditionally swapping all nodes in the merge path
 * when merging two heaps. (The merge operation is also used when adding
 * and removing values.)
 *
 * With no structural constraints, it may seem that a skew heap would be horribly
 * inefficient. However, amortized complexity analysis can be used to demonstrate
 * that all operations on a skew heap can be done in O(log n).
 *
 * Wikipedia: http://en.wikipedia.org/wiki/Skew_heap
 * Self-Adjusting Heaps: http://www.cs.cmu.edu/~sleator/papers/Adjusting-Heaps.htm
 * Union-Based Heaps: https://speakerdeck.com/kachayev/union-based-heaps?slide=22
 * 
 */

abstract sealed class Heap[+A <% Ordered[A]] {

  /**
   * Min value of this heap.
   */
  def min: A

  /**
   * The left child of this heap.
   */
  def left: Heap[A]

  /**
   * The right child of this heap.
   */
  def right: Heap[A]

  /**
   * Whether this heap is empty or not.
   */
  def isEmpty: Boolean

  /**
   * The 'insert' function might be defined through the 'Heap.merge' function
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def insert[B >: A <% Ordered[B]](x: B): Heap[B] =
    Heap.merge(Heap.make(x), this)

  /**
   * Removes the minimum element from this heap.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def remove: Heap[A] = Heap.merge(left, right)

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
  def left: Heap[Nothing] = fail("An empty heap.")
  def right: Heap[Nothing] = fail("An empty heap.")
  def isEmpty = true
}

/**
 * Non-empty node is an element with left and right childs (Skew Heaps)
 */
case class Branch[A <% Ordered[A]](min: A, left: Heap[A], right: Heap[A]) extends Heap[A] {
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
  def make[A <% Ordered[A]](x: A, l: Heap[A] = Leaf, r: Heap[A] = Leaf) =
    Branch(x, l, r)

  /**
   * Merges two given heaps. Also known as "union".
   * 
   * When two skew heaps are to be merged, we can use a similar process as the merge of two leftist heaps:
   * 
   * - Compare roots of two heaps; let p be the heap with the smaller root, and q be the other heap.
   *   Let r be the name of the resulting new heap.
   * - Let the root of r be the root of p (the smaller root), and let r's right subtree be p's left subtree.
   * - Now, compute r's left subtree by recursively merging p's right subtree with q.
   * 
   * More about "merge" operation: http://en.wikipedia.org/wiki/Skew_heap#Operations
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def merge[A <% Ordered[A]](x: Heap[A], y: Heap[A]): Heap[A] = (x, y) match {
    case (_, Leaf) => x
    case (Leaf, _) => y
    case (Branch(x1, l1, r1), Branch(x2, l2, r2)) =>
      if (x1 < x2) Branch(x1, Heap.merge(Branch(x2, l2, r2), r1), l1)
      else Branch(x2, Heap.merge(Branch(x1, l1, r1), r2), l2)
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