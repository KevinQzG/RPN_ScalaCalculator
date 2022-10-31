object RPN extends App {

    def solveRPN(eqn: String): Double = {
        val items = eqn.split(" ")
        val accumulator = List[Double]()
        items.foldLeft(accumulator)(foldingFunction).head
    }

    def foldingFunction (stack: List[Double], a: String): List[Double] = {
        a match {
            case "+" => stack.tail.head + stack.head :: stack.tail.tail
            case "-" => stack.tail.head - stack.head :: stack.tail.tail
            case "*" => stack.tail.head * stack.head :: stack.tail.tail
            case "/" => stack.tail.head / stack.head :: stack.tail.tail
            case "sqrt" => Math.sqrt(stack.head) :: stack.tail
            case "negate" => -stack.head :: stack.tail
            case "totalSum" => stack.sum :: stack.tail
            case "totalProduct" => stack.product :: stack.tail
            case "totalPromedy" => stack.sum / stack.length :: stack.tail
            case "condNumber" => condNumber(stack.head) :: stack.tail        
            case _ => a.toDouble :: stack
}

    }

def condNumber (a: Double) ={
        if (a==3) {100}
        else if (a==5){25}
        else {0}
      }


    println ("")
    println ("Welcome to the RPN calculator, RPN Example: 10 4 3 + 2 * - = -4")
    println ("")
    println ("1. Add (10 4 3 2 + +) :")
    println ("The solution is: ")
    println (solveRPN("10 4 3 2 + +"))
    println ("")
    println ("2. Subtract (20 5 4 2 - -) :")
    println ("The solution is: ")
    println (solveRPN("20 5 4 2 - -"))
    println ("")
    println ("3. Multiply (190 5 2 4 * *) :")
    println ("The solution is: ")
    println (solveRPN("190 5 2 4 * *"))
    println ("")
    println ("4. Divide (70 5 6 4 / /) :")
    println ("The solution is: ")
    println (solveRPN("70 5 6 4 / /"))
    println ("")
    println ("5. Negate (10 67 negate) :")
    println ("The solution is: ")
    println (solveRPN("10 67 negate"))
    println ("")
    println ("6. Negate (10 67 negate +) :")
    println ("The solution is: ")
    println (solveRPN("10 67 negate +"))
    println ("")
    println ("7. Negate (10 67 negate *) :")
    println ("The solution is: ")
    println (solveRPN("10 67 negate *"))
    println ("")
    println ("8. Sqrt (10 16 sqrt) :")
    println ("The solution is: ")
    println (solveRPN("10 16 sqrt"))
    println ("")
    println ("9. Sqrt (10 16 sqrt +) :")
    println ("The solution is: ")
    println (solveRPN("10 16 sqrt +"))
    println ("")
    println ("10. condNumber (10 5 condNumber) :")
    println ("The solution is: ")
    println (solveRPN("10 5 condNumber"))
    println ("")
    println ("11. condNumber (10 5 condNumber -) :")
    println ("The solution is: ")
    println (solveRPN("10 5 condNumber -"))
    println ("")
    println ("12. condNumber (10 3 condNumber +) :")
    println ("The solution is: ")
    println (solveRPN("10 3 condNumber +"))
    println ("")
    println ("13. condNumber (10 2 condNumber +) :")
    println ("The solution is: ")
    println (solveRPN("10 2 condNumber +"))
    println ("")
    println ("14. totalSum (10 67 15 totalSum) :")
    println ("The solution is: ")
    println (solveRPN("10 67 15 totalSum"))
    println ("")
    println ("15. totalProduct (10 16 10 totalProduct) :")
    println ("The solution is: ")
    println (solveRPN("10 16 10 totalProduct"))
    println ("")
    println ("16. totalPromedy (120 20 40 120 totalPromedy) :")
    println ("The solution is: ")
    println (solveRPN("120 20 40 120 totalPromedy"))
    println ("")
    println ("Thanks for using the RPN calculator, bye!")
    println ("Created by: Kevin Quiroz and Juan Esteban García")
    println ("")


}






