/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Queue http://en.wikipedia.org/wiki/Queue_(abstract_data_type)
 *
 * -Notes-
 * This queue also known as Banker's Queue.
 *
 * Enqueue - O(1)
 * Dequeue - O(1)
 * Front - O(1)
 * Rear - O(1)
 */

abstract class List[+A] {
  def reverse: List[A] = {
    def loop(s: List[A], d: List[A]): List[A] = s match {
      case Nill => d
      case Cons(h, t) => loop(t, new Cons(h, d))
    }

    loop(this, Nill)
  }
}

case object Nill extends List[Nothing]
case class Cons[A](head: A, tail: List[A]) extends List[A]

class Queue[+A](in: List[A] = Nill, out: List[A] = Nill) {

  /**
   * Check whether this queue is empty or not.
   */
  def isEmpty: Boolean = (in, out) match {
    case (Nill, Nill) => true
    case (_, _) => false
  }

  /**
   * Dequeues the first element from this queue.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def dequeue: (A, Queue[A]) = (in, out) match {
    case (_, Cons(h, t)) => (h, new Queue(in, t))
    case (_, _) => in.reverse match {
      case Cons(h, t) => (h, new Queue(Nill, t))
      case Nill => throw new NoSuchElementException("Empty queue.")
    }
  }

  /**
   * Enqueues given element 'x' into the end of this queue.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def enqueue[B >: A](x: B): Queue[B] = new Queue(new Cons(x, in), out)

  /**
   * Returns the first element of this queue.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def front: A = dequeue match {
    case (a, _) => a
  }

  /**
   * Returns the rear of this queue.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def rear: Queue[A] = dequeue match {
    case (_, q) => q
  }
}

object Queue {

  /**
   * Creates a new empty queue.
   * 
   * Time - O(1)
   * Space - O(1)
   */
  def empty[A]: Queue[A] = new Queue()

  /**
   * Creates a new queue fromm given 'xs' sequence.
   *
   * Time - O(n)
   * Space - O(1)
   */
  def apply[A](xs: A*): Queue[A] = {
    var r: Queue[A] = new Queue()
    for (x <- xs) {
      r = r.enqueue(x)
    }
    r
  }
}
