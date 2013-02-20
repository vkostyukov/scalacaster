/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Queue http://en.wikipedia.org/wiki/Queue_(abstract_data_type)
 *
 * Enqueue - O(1)
 * Dequeue - O(1)
 * Front - O(1)
 *
 */

abstract class List[+A] {
  def head: A
  def tail: List[A]
  def isEmpty: Boolean

  def <+[B >: A](v: B): List[B] = new NonEmptyList(v, this)
  def r: List[A] = {
    def loop(s: List[A], d: List[A]): List[A] = {
      if (s.isEmpty) d
      else loop(s.tail, d <+ s.head)
    }

    loop(this, NIL)
  }
}

object NIL extends List[Nothing] { // since 'Nil' already reserved 
  def head: Nothing = throw new NoSuchElementException("Empty queue.")
  def tail: List[Nothing] = throw new NoSuchElementException("Empty queue.")

  def isEmpty: Boolean = true
}

class NonEmptyList[A](h: A, t: List[A] = NIL) extends List[A] {
  def head: A = h
  def tail: List[A] = t

  def isEmpty: Boolean = false
}

class Queue[+A](val in: List[A] = NIL, val out: List[A] = NIL) {

  def isEmpty: Boolean = in.isEmpty && out.isEmpty

  def dequeue: (A, Queue[A]) = out match {
    case NIL if (!in.isEmpty) => val r = in.r; (r.head, new Queue(NIL, r.tail))
    case o: List[A] => (o.head, new Queue(in, o.tail))
    case _ => throw new NoSuchElementException("Empty queue.")
  }

  def enqueue[B >: A](v: B): Queue[B] = new Queue(in <+ v, out)

  def front: A = 
    if (!out.isEmpty) out.head 
    else if (!in.isEmpty) in.r.head 
    else throw new NoSuchElementException("Empty queue.")
}
