/*Union - Find Algorithm 
Union: Connects two objects. 
Find: Is there a path between the two objects.
Application: Example of Union - A person on a social network connecting with another person.
			 Example of Find  - Is a person connected to another person on the social network.
*/
class QuickUnion(n:Int) {
	var id:Array[Int] = new Array[Int](n);
	var sz:Array[Int] = new Array[Int](n);

	def init() = {
		for(i <- 0 until n) {
			id(i) = i;
			sz(i) = 1;
		}
	}

	def root(i:Int):Int = {
		var j = i;
		while(j != id(j)) j=id(j);
		return j;
	}

	def connected(p:Int, q:Int):Boolean = {
		return root(p) == root(q);
	}

	def union(p:Int, q:Int) = {
		var rootp = root(p);
		var rootq = root(q);

		if(sz(rootp) >= sz(rootq)) {
			id(rootq) = rootp;
			sz(rootp) += sz(rootq);
		} else {
			sz(rootq) += sz(rootp);
			id(rootp) = rootq;
		}
	}

	def display() = {
		for(i <- 0 until n) print(id(i) + "\t");
		println("\n");
	}
}

object Test {
	def main(args:Array[String]) {
		var client = new QuickUnion(3);
		client.init();

		println("The elements are:");
		client.display();

		println("is connected 0 and 1:");
		println(client.connected(0,1));

		println("union of 0 and 1:");
		client.union(0,1);

		println("is connected 0 and 1:");
		println(client.connected(0,1));
	}
}
