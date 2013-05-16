/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Heap http://en.wikipedia.org/wiki/Heap_(data_structure)
 * Binary Heap http://en.wikipedia.org/wiki/Binary_heap
 *
 * Min - O(1)
 * Remove - O(log n)
 * Insert - O(log n)
 * Merge - O(n)
 *
 * -Notes-
 * 
 * This is an effecient implementation of binary heap that grantees O(log n) running
 * time for insert/remove operations.
 *
 * There are two invariants of binary heaps:
 *
 * 1. Shape-property - the heap - is a complete binary tree.
 * 2. Heap-property - children's elements should be less or equal to parent's.
 *
 * These invariants complete by two main ideas. The first thing is there are (2^n - 1)
 * nodes in a complete binary tree. This can be used in insertion for choosing right
 * search path. To garantee O(log n) insertion running time two node parameters can be
 * used - 'height' and 'size'. So, the condition looks like following:
 *
 *               node.size < math.pow(2, node.height) - 1
 *
 * The second thing is bubbling that goes up to search path and swapping nodes to
 * correspond heap property.
 *
 * Removal it is always pain in the neck. There are three phases of removal minimum
 * element from binary heap:
 *
 * 1. Find latest inserted node. 
 * 2. Replace root value with this node's value.
 * 3. Fix all heap property violations by bubbling root element down.
 *
 * These can be done in O(log n) time.
 *
 * Heap construction can be performed in O(n) time by using following approach:
 *
 * 1. Insert all items into the heap to satisfy 'shape invariant'.
 * 2. Fix all violations of 'heap invariant' by bubbling nodes up.
 *
 */

abstract sealed class Heap[+A <% Ordered[A]] {

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
    if (isEmpty) 
      Heap(x)
    else if (left.size < math.pow(2, left.height) - 1) 
      Heap.bubbleUp(min, left.insert(x), right)
    else if (right.size < math.pow(2, right.height) - 1) 
      Heap.bubbleUp(min, left, right.insert(x))
    else if (right.height < left.height) 
      Heap.bubbleUp(min, left, right.insert(x))
    else 
      Heap.bubbleUp(min, left.insert(x), right)

  /**
   * Removes minumum element from this heap.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def remove: Heap[A] = {
    def bubbleLeftUp[A <% Ordered[A]](x: A, l: Heap[A], r: Heap[A]): Heap[A] = l match {
      case Branch(y, lt, rt, _, _) => Heap(y, Heap(x, lt, rt), r)
      case _ => Heap(x, l, r)
    }

    def bubbleRightUp[A <% Ordered[A]](x: A, l: Heap[A], r: Heap[A]): Heap[A] = r match {
      case Branch(y, lt, rt, _, _) => Heap(y, l, Heap(x, lt, rt))
      case _ => Heap(x, l, r)
    }

    def mergeChildren(l: Heap[A], r: Heap[A]): Heap[A] = 
      if (l.isEmpty && r.isEmpty) 
        Heap.empty
      else if (l.size < math.pow(2, l.height) - 1) 
        bubbleLeftUp(l.min, mergeChildren(l.left, l.right), r)
      else if (r.size < math.pow(2, r.height) - 1)
        bubbleRightUp(r.min, l, mergeChildren(r.left, r.right))
      else if (r.height < l.height)
        bubbleLeftUp(l.min, mergeChildren(l.left, l.right), r)
      else
        bubbleRightUp(r.min, l, mergeChildren(r.left, r.right))

    def bubbleRootDown(h: Heap[A]): Heap[A] = 
      if (h.isEmpty) Heap.empty
      else Heap.bubbleDown(h.min, h.left, h.right)

    if (isEmpty) throw new NoSuchElementException("Empty heap.")
    else bubbleRootDown(mergeChildren(left, right))
  }
}

case class Branch[A <% Ordered[A]](m: A, l: Heap[A], r: Heap[A], s: Int, h: Int) extends Heap[A] {
  def min: A = m
  def left: Heap[A] = l
  def right: Heap[A] = r
  def size: Int = s
  def height: Int = h
  def isEmpty: Boolean = false
}

case object Leaf extends Heap[Nothing] {
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


  /**
   * Creates a new heap from given sorted array 'a'.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def fromSortedArray[A <% Ordered[A]](a: Array[A]): Heap[A] = {
    def loop(i: Int): Heap[A] = 
      if (i < a.length) Heap(a(i), loop(2 * i + 1), loop(2 * i + 2))
      else Heap.empty

    loop(0)
  }

  /**
   * Creates a new heap from given array 'a'.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def fromArray[A <% Ordered[A]](a: Array[A]): Heap[A] = {
    def loop(i: Int): Heap[A] = 
      if (i < a.length) bubbleDown(a(i), loop(2 * i + 1), loop(2 * i + 2))
      else Heap.empty

    loop(0)
  }


  /**
   * Bubbles given heap ('x', 'l', 'r') up.
   *
   * Time - O(1)
   * Space - O(1)
   */
  private[Heap] def bubbleUp[A <% Ordered[A]](x: A, l: Heap[A], r: Heap[A]): Heap[A] = (l, r) match {
    case (Branch(y, lt, rt, _, _), _) if (y < x) => 
      Heap(y, Heap(x, lt, rt), r)
    case (_, Branch(y, lt, rt, _, _)) if (y < x) => 
      Heap(y, l, Heap(x, lt, rt))
    case (_, _) => 
      Heap(x, l, r)
  }

  /**
   * Bubbles given heap ('x', 'l', 'r') down.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  private[Heap] def bubbleDown[A <% Ordered[A]](x: A, l: Heap[A], r: Heap[A]): Heap[A] =  (l, r) match {
    case (Branch(z, _, _, _, _), Branch(y, lt, rt, _, _)) if (y < z && x > y) => 
      Heap(y, l, bubbleDown(x, lt, rt))
    case (Branch(y, lt, rt, _, _), _) if (x > y) => 
      Heap(y, bubbleDown(x, lt, rt), r)
    case (_, _) => 
      Heap(x, l, r)
  }
}
