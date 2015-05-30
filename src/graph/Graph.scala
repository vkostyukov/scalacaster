/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * -Notes-
 * 
 * Usage example:
 * var g: Graph[Int, String] = Graph(
 *        ("A", 20, "B"), ("B", 30, "C"), 
 *        ("C", 40, "D"), ("D", 50, "A"), 
 *        ("A", 100, "C"), ("D", 200, "B"))
 * 
 * Creates the following graph:
 * 
 *  |<-------------------|
 *  D ---> A ---> B ---> C 
 *  |------|----->|      |
 *         |------------>|
 *
 */

case class Edge[E, N](source: Graph[E, N], target: Graph[E, N], value: E)

class Graph[E, N](var value: N = null.asInstanceOf[N]) {

  import scala.collection.immutable.Queue

  var inEdges: List[Edge[E, N]] = Nil
  var outEdges: List[Edge[E, N]] = Nil

  /**
   * All successors of this graph.
   *
   * Time - O(n)
   * Space - O(1)
   */
  def succs: List[Graph[E, N]] = outEdges.map(_.target)

  /**
   * All predecessors of this graph.
   *
   * Time - O(n)
   * Space - O(1)
   */
  def preds: List[Graph[E, N]] = inEdges.map(_.source)

  /**
   * Adds new connection to this graph.
   *
   * Time - O()
   * Space - O()
   */
  def connect(from: N, via: E, to: N): (Graph[E, N], Graph[E, N]) = {
    val fromGraph: Graph[E, N] = if (value == null) {value = from; this} else hop(from) match {
      case Some(g) => g
      case None => Graph.one(from)
    }

    val toGraph: Graph[E, N] = hop(to) match {
      case Some(g) => g
      case None => Graph.one(to)
    }

    fromGraph.outEdges = new Edge(fromGraph, toGraph, via) :: fromGraph.outEdges
    toGraph.inEdges = new Edge(fromGraph, toGraph, via) :: toGraph.inEdges

    (fromGraph, toGraph)
  }

  /**
   * Hops to the given node 'n' if its exist in this graph.
   *
   * Time - O(n)
   * Space - O(1)
   */
  def hop(n: N): Option[Graph[E, N]] = graphsByDepth.find(_.value == n)

  /**
   * Returns all nodes of this graph arranged by DFS algorithm.
   *
   * Time - O()
   * Space - O()
   */
  def nodesByDepth: List[N] = graphsByDepth.map(_.value)

  /**
   * Returns all nodes of this graph arranged by BFS algorithm.
   *
   * Time - O()
   * Space - O()
   */
  def nodesByBreadth: List[N] = graphsByBreadth.map(_.value)

  /**
   * Returns all graphs that are connected to this graph arranged by DFS algorithm.
   *
   * Time - O()
   * Space - O()
   */
  def graphsByDepth: List[Graph[E, N]] = {
    def loop(g: Graph[E, N], s: Set[Graph[E, N]]): Set[Graph[E, N]] = 
      if (!s(g)) {
        val ss = g.succs.foldLeft(s + g)((acc, gg) => loop(gg, acc))
        g.preds.foldLeft(ss)((acc, gg) => loop(gg, acc))
      } else s

    loop(this, Set()).toList
  }

  /**
   * Returns all graphs that are connected to this graph arranged by BFS algorithm.
   *
   * Time - O()
   * Space - O()
   */
  def graphsByBreadth: List[Graph[E, N]] = {
    def loop(q: Queue[Graph[E, N]], s: Set[Graph[E, N]]): Set[Graph[E, N]] = 
      if (!q.isEmpty && !s(q.head)) {
        val qq = q.head.succs.foldLeft(q.tail)((acc, gg) => acc :+ gg)
        loop(q.head.preds.foldLeft(qq)((acc, gg) => acc :+ gg), s + q.head)
      } else s

    loop(Queue(this), Set()).toList
  }

  /**
   * Returns the number of nodes connected with this graph.
   *
   * Time - O()
   * Space - O()
   */
  def size: Int = graphsByDepth.size

  override def equals(o: Any): Boolean = o match {
    case g: Graph[_, _] => g.value == value
    case _ => false
  }

  override def toString: String = "Graph(" + value + ")"
}

class WeightedGraph[N](n: N) extends Graph[Double, N](n) {

  /**
   * Searches for the shortest path in this graph.
   *
   * Time - O()
   * Space - O()
   */
  def dijkstra(from: N, to: N): List[N] = {
    var distances = nodesByDepth.map((_, Double.PositiveInfinity)).toMap + (from -> 0.0)
    var spt: Map[N, N] = Map()

    while (!distances.isEmpty) {
      // search for min in distances
      // remove min from distances

      // for all succsesors of mininum update its distance v--e--u
      // if dist(v) + e < dist(u)
      // dist(u) = dist(v) + e
      // spt + (u -> v)
      // v - is min

      val (v, _) = distances.minBy(_._2)
      distances = distances - v

      val vv = hop(v).get
      for (e <- vv.outEdges) {
        val u = e.target.value
        if (distances(u) < distances(v) + e.value) {
          distances = distances + (u -> (distances(v) + e.value))
          spt = spt + (u -> v)
        }
      }
    }

    var step = to
    var path = List(step)
    while (spt.contains(step)) {
      step = spt(step)
      path = step :: path
    }

    path.reverse
  }
  
/**
   * Searches for the shortest path in this graph. Unlike Dijkstra, 
   * Bellman-Ford can be applied also when there are edges with negative
   * values.
   * 
   * Bellman-Ford cannot reach a valid solution if there are negative cycles
   * in the graph, but it can detect such cycles. If there are negative cycles
   * this function returns None, else it returns the shortest path.
   *
   * Time - O(n m) where n=|vertices| and m=|edges|
   * Space - O()
   */
  def bellmanFord(from: N, to: N): Option[List[N]] = {
    
    val graphs = graphsByDepth
    val f = hop(from).get
    val t = hop(to).get
    
    val initDists = graphs.map((_, Double.PositiveInfinity)).toMap + (f -> 0.0)
    var paths = graphs.map((_, List[N]())).toMap
    
    @annotation.tailrec
    def minDists(dists: Map[Graph[Double, N], Double], hops: Int): Map[Graph[Double, N], Double] = {
      if (hops == 0) dists
      else {
        var newDists = dists ++ Map.empty
        
        def minDist(g: Graph[Double, N]): Option[(Graph[Double, N], Double)] = {
          if (g.inEdges.isEmpty) None
          else Some(g.inEdges.map(e => (e.source, dists(e.source) + e.value)).minBy(_._2))
        }
        
        graphs.foreach { g =>
          minDist(g) match {
            case Some((f,d)) if(d < dists(g)) =>
              newDists += (g -> d)
              paths += (g -> (f.value :: paths(f)))
            case _ =>
          }
        }
        
        // If min distances do not change in some hop then they won't
        // change in the next ones, thus we can stop early
        if(newDists == dists) newDists
        else minDists(newDists, hops-1)
      }
    }
    
    val dists = minDists(initDists, graphs.size-1)
    
    // There are neg cycles iff the min distances change again
    if (dists != minDists(dists, 1)) None
    else Some((to :: paths(t)).reverse)
  }
}

object Graph {
  def apply[E, N](tuples: (N, E, N)*): Graph[E, N] = {
    val g: Graph[E, N] = Graph.empty
    for ((from, via, to) <- tuples) {
      g.connect(from, via, to)
    }
    g
  }

  def one[E, N](n: N): Graph[E, N] = new Graph(n)

  def empty[E, N]: Graph[E, N] = new Graph
}

// var g: Graph[Int, String] = Graph(
//        ("A", 20, "B"), ("B", 30, "C"), 
//        ("C", 40, "D"), ("D", 50, "A"), 
//        ("A", 100, "C"), ("D", 200, "B"))

// println(g.nodesByBreadth)
