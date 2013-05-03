/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Heap http://en.wikipedia.org/wiki/Heap_(data_structure)
 * Binary Heap http://en.wikipedia.org/wiki/Binary_heap
 *
 * Find Min - O(1)
 * Remove Min - O(log n)
 * Add - O(log n)
 * Merge - O(n)
 *
 * -Notes-
 * 
 */

abstract class Heap[+A <% Ordered[A]] {

  /**
   * Minumum of this heap.
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
   * The size of this heap.
   */
  def size: Int

  /**
   * The height of this tree
   */
  def height: Int

  /**
   * Checks whether this heap empty or not.
   */
  def isEmpty: Boolean

  /**
   * Inserts given element 'x' into this heap.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def insert[B >: A <% Ordered[B]](x: B): Heap[B] =
    if (isEmpty) Heap(x)
    else if (left.isEmpty) bubbleLeft(min, Heap(x), right)
    else if (right.isEmpty) bubbleRight(min, left, Heap(x))
    else if (left.size < math.pow(2, left.height) - 1) bubbleLeft(min, left.insert(x), right)
    else if (right.size < math.pow(2, right.height) - 1) bubbleRight(min, left, right.insert(x))
    else if (right.height < left.height) bubbleRight(min, left, right.insert(x))
    else bubbleLeft(min, left.insert(x), right)

  /**
   * Bubbles just inserted element 'x' up to left heap.
   *
   * Time - O(1)
   * Space - O(1)
   */
  private[this] def bubbleLeft[B >: A <% Ordered[B]](x: B, l: Heap[B], r: Heap[B]): Heap[B] = 
    if (l.min < x) Heap(l.min, Heap(x, l.left, l.right), r)
    else Heap(x, l, r)

  /**
   * Bubbles just inserted element 'x' up to right heap.
   *
   * Time - O(1)
   * Space - O(1)
   */
  private[this] def bubbleRight[B >: A <% Ordered[B]](x: B, l: Heap[B], r: Heap[B]): Heap[B] =
    if (r.min < x) Heap(r.min, l, Heap(x, r.left, r.right))
    else Heap(x, l, r)
}

class Branch[A <% Ordered[A]](m: A, l: Heap[A], r: Heap[A], s: Int, h: Int) extends Heap[A] {
  def min: A = m
  def left: Heap[A] = l
  def right: Heap[A] = r
  def size: Int = s
  def height: Int = h
  def isEmpty: Boolean = false
}

object Leaf extends Heap[Nothing] {
  def min: Nothing = throw new NoSuchElementException("Leaf.min")
  def left: Heap[Nothing] = throw new NoSuchElementException("Leaf.left")
  def right: Heap[Nothing] = throw new NoSuchElementException("Leaf.right")
  def size: Int = 0
  def height: Int = 0
  def isEmpty: Boolean = true
}

object Heap {

  /**
   * Returns an empty heap.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def empty[A]: Heap[A] = Leaf

  /**
   * Creates a singleton heap for given element 'x'.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def apply[A <% Ordered[A]](x: A, l: Heap[A] = Leaf, r: Heap[A] = Leaf): Heap[A] = 
    new Branch(x, l, r, l.size + r.size + 1, math.max(l.height, r.height) + 1)
}

var h: Heap[Int] = Heap.empty
h = h.insert(10)
h = h.insert(20)
h = h.insert(30)
h = h.insert(40)
h = h.insert(50)
h = h.insert(60)
h = h.insert(70)

println(h.min)
println(h.left.min)
println(h.right.min)
println(h.left.left.min)
println(h.left.right.min)
println(h.right.left.min)
println(h.right.right.min)
