/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Binary Search Tree http://en.wikipedia.org/wiki/Binary_search_tree
 *
 * Insert - O(log n)
 * Lookup - O(log n)
 * Remove - O(log n)
 *
 */
abstract class Tree[+A <% Ordered[A]] {
  def value: A
  def left: Tree[A]
  def right: Tree[A]

  def isEmpty: Boolean

  def add[B >: A <% Ordered[B]](x: B): Tree[B]  =
    if (isEmpty) new Node(x, Leaf, Leaf)
    else if(x < value) new Node(value, left.add(x), right)
    else new Node(value, left, right.add(x))

  def contains[B >: A <% Ordered[B]](x: B): Boolean =
    if (isEmpty) false
    else if (x < value) left.contains(x)
    else if (x > value) right.contains(x)
    else true
}

object Leaf extends Tree[Nothing] {
  def value: Nothing = throw new NoSuchElementException("Leaf.value")
  def left: Tree[Nothing] = throw new NoSuchElementException("Leaf.left")
  def right: Tree[Nothing] = throw new NoSuchElementException("Leaf.right")

  def isEmpty: Boolean = true
}

class Node[A <% Ordered[A]](v: A, l: Tree[A], r: Tree[A]) extends Tree[A] {
  def value: A = v
  def left: Tree[A] = l
  def right: Tree[A] = r

  def isEmpty: Boolean = false
}
