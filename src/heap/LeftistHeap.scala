/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Heap http://en.wikipedia.org/wiki/Heap_(data_structure)
 * 
 * -Notes-
 * 
 * This is Okasaki's Leftist Heap implemetation that satisfies two properties:
 *
 *   1. Heap-ordered propery: value(node) <= value(node.left) and also
 *                            value(node) <= value(node.right)
 *
 *   2. Leftist property: rank(node.left) >= rank(node.right)
 *
 * , where the 'rank' is rightmost spine (rightmost path from the root to 
 * an empty node) of the heap. Combining these proprties together, we can
 * guarantee at most O(log n) running time for insert/remove operations of
 * this heap.
 *
 * The heart of this implementation is 'merge' function that merges two given
 * heaps in logarithmic time, which is incredible performance. So, leftist heaps
 * are beautiful in terms of fast merging. The typical use case for such heaps - 
 * functional map/reduce algorithms where the final result is gathering from
 * the pieces by merging them. The 'merge' function can be treated as merging
 * two sorted lists (since the values in the right spine are always sorted).
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
   * The rank of this heap.
   */
  def rank: Int

  /**
   * Whether this heap is empty or not.
   */
  def isEmpty: Boolean

  /**
   * Inserts given element 'x' into this heap.
   *
   * Exercise 3.2 @ PFDS.
   *
   * The 'insert' function might be defined through the 'Heap.merge' function, like
   *
   *   insert(x) = Heap.merge(this, Heap.make(x))
   *
   * We also can define the 'insert' function directly rather then calling merge
   * function. But the new function should also work in O(log n) time. This is the
   * main resriction.
   *
   * Tha main idea behind this function is that we can safely insert new node into
   * the right sub-heap until it's rank less then it's left sibling. Otherwise,
   * we have to _fill_ the left sub-heap enough to have it rank increased. This
   * approach gives us a complete heap as output, instead of unbalanced in case
   * of pure leftist heaps. 
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def insert[B >: A <% Ordered[B]](x: B): Heap[B] =
    if (isEmpty) Heap.make(x)
    else if (left.rank > right.rank) Heap.bubble(min, left, right.insert(x))
    else Heap.bubble(min, left.insert(x), right)

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

case object Leaf extends Heap[Nothing] {
  def min: Nothing = fail("An empty heap.")
  def left: Heap[Nothing] = fail("An empty heap.")
  def right: Heap[Nothing] = fail("An empty heap.")
  def rank: Int = 0
  def isEmpty = true
}

case class Branch[A <% Ordered[A]](min: A, 
                                   left: Heap[A],
                                   right: Heap[A],
                                   rank: Int) extends Heap[A] {

  def isEmpty = false
}

object Heap {

  /**
   * An empty heap.
   */
  def empty[A]: Heap[A] = Leaf

  /**
   * Makes a heap's node with pre-calculated rank value.
   */
  def make[A <% Ordered[A]](x: A, l: Heap[A] = Leaf, r: Heap[A] = Leaf) =
    Branch(x, l, r, r.rank + 1)

  /**
   * A smart constructor for heap's branch.
   *
   * In order to satisfy the leftist property, we have to check the children's ranks
   * and do the swap if needed.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def swap[A <% Ordered[A]](x: A, l: Heap[A] = Leaf, r: Heap[A] = Leaf): Heap[A] = 
    if (l.rank < r.rank) Branch(x, r, l, l.rank + 1)
    else Branch(x, l, r, r.rank + 1)

  /**
   * Bubbles given value up to the heap's root.
   *
   * This function fixes the heap-ordered property violation.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def bubble[A <% Ordered[A]](x: A, l: Heap[A], r: Heap[A]): Heap[A] = (l, r) match {
    case (Branch(y, lt, rt, _), _) if (x > y) => 
      Heap.make(y, Heap.make(x, lt, rt), r)
    case (_, Branch(z, lt, rt, _)) if (x > z) => 
      Heap.make(z, l, Heap.make(x, lt, rt))
    case (_, _) => Heap.make(x, l, r)
  }

  /**
   * Merges two given heaps along their right spine.
   *
   * The 'merge' function make sure that values from right spine will be sorted
   * after merging (satisfying the heap-ordered proerty). This gives us guarantee
   * that the smallest element of result heap will be at root.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def merge[A <% Ordered[A]](x: Heap[A], y: Heap[A]): Heap[A] = (x, y) match {
    case (_, Leaf) => x
    case (Leaf, _) => y
    case (Branch(xx, xl, xr, _), Branch(yy, yl, yr, _)) =>
      if (xx < yy) Heap.swap(xx, xl, Heap.merge(xr, y))
      else Heap.swap(yy, yl, Heap.merge(yr, x))
  }
}
