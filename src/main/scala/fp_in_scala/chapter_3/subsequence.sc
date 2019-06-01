// could foldRight and have a tuple with matches, positions, current sequence being passed
// so as we move along a list, we add to the buffer if we match and reset if we dont
// this wont get overlaps thought i.e List(1,1,1,1) should have three matches of List(1,1)
// To make the comparison list, you would be adding to the buffer and then comparing the same elements of the list
// i.e.
// source List(1,2,3,4,5), target(3,4)
// Discard until we get to 3
// buffer = List(3) =>>> if(buffer.size == target.size) buffer == target else buffer == target.take(buffer.size)


// Or

// fold right and for each position, take the size of the target and compare. 

def hasSubsequnce[A](source: Seq[A], target: Seq[A]): Boolean = ???