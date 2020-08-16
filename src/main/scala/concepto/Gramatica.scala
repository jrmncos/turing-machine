package concepto
import scala.util.matching.Regex
import parser.ParserGramatica

//Epsilon: ε Produccion: →
case class Gramatica (producciones: List[Produccion], terminales: Set[Terminal], variables: Set[Variable], S: Variable)  {}

case class Produccion (variable: Variable, produce: List[Simbolo]) {
  val s = produce.foldRight("")(_.toString+_.toString())
  override def toString() = { variable + " → "+ s }
}

class Simbolo(t: String) {
  val value = t
}

case class Variable(v: String) extends Simbolo(v: String) {
  override def toString() = { v }
}

case class Terminal(t: String) extends Simbolo(t: String) {
  override def toString() = { t }
}

object Gramatica {
  def procesar(g: Gramatica, w: String): Boolean = {
      def go(g: Gramatica, pr: List[Produccion],  ladoDerecho: List[Simbolo], w: String, i: Int): Boolean = {
        if(longitudTerminalesContenidasEnLadoDerecho(g, ladoDerecho) > w.size) false
        //Si no tiene mas variables comparo con el string
        else if(!tieneVariables(g, ladoDerecho)) {
      	  if(quitarEpsilon(ladoDerecho.foldRight("")(_.toString+_.toString())) == quitarEpsilon(w)) 
      	    true
      	  else 
      	    false 
        }
        //El i donde estoy parado es una variable
        else if(g.variables.contains(Variable(ladoDerecho(i).value))) {
         val prX = extraerProducciones(g, ladoDerecho(i)) 
         val primerosI = ladoDerecho.take(i)
         val ultimosI = ladoDerecho.drop(i+1)
				 prX.foldLeft(false)((l, p) => l || go(g, prX, primerosI ++ p.produce ++ ultimosI, w, i))
        }
        //El i donde estoy parado es una terminal
        else {
          if(InputWEmpiezaConLasPrimerasiTerminalesDeLadoDerecho(w, ladoDerecho, i))
            go(g, pr, ladoDerecho, w, i+1) 
          else false
        }
      }
      val prX = extraerProducciones(g, g.S)
      prX.foldLeft(false)((l, p) => l ||  go(g, prX, p.produce, w, 0))
  }

  def InputWEmpiezaConLasPrimerasiTerminalesDeLadoDerecho(w: String, ladoDerecho: List[Simbolo], i: Int):Boolean = {
    quitarEpsilon(w).startsWith(quitarEpsilon(ladoDerecho.take(i).foldRight("")(_.toString+_.toString())))
  }
  
  def extraerProducciones(g: Gramatica, S: Simbolo): List[Produccion] = {
    g.producciones.filter(_.variable == S)
  }
    
  def quitarEpsilon(s: String): String = {
    s.replace("ε", "")  
  } 
  
  def longitudTerminalesContenidasEnLadoDerecho(g: Gramatica, LS: List[Simbolo]): Int = {
    val charRegEx: Regex = "[a-z]|[0-9]|\\(|\\)|\\*|\\+".r
    val s = LS.foldRight("")(_.toString+_.toString())
    val ret = charRegEx.findAllIn(s).toList
    ret.size
  }
  
  def tieneVariables(g: Gramatica, LS: List[Simbolo]): Boolean = {
    val charRegEx: Regex = "[A-Z]".r
    val s = LS.foldRight("")(_.toString+_.toString())
    val ret = charRegEx.findAllIn(s).toList
    !ret.isEmpty
  }
  def union(g1: Gramatica, g2: Gramatica): Gramatica = {
      val S = Variable("S")
      val produccionesUnion = ParserGramatica.producciones(List(S.toString()+"→ "+g1.S, S.toString()+"→"+g2.S))
      Gramatica(produccionesUnion++g1.producciones++g2.producciones, g1.terminales++g2.terminales, g1.variables++g2.variables, S)
    }

    def concatenacion(g1: Gramatica, g2: Gramatica): Gramatica = {
      val S = Variable("S")
      val produccionConcatenacion = ParserGramatica.producciones(List(S.toString()+"→ "+g1.S+g2.S))
      Gramatica(produccionConcatenacion++g1.producciones++g2.producciones, g1.terminales++g2.terminales, g1.variables++g2.variables, S)
    }

    def clausura(g1: Gramatica): Gramatica = {
      val produccionConcatenacion = ParserGramatica.producciones(List(g1.S.toString()+" → "+g1.S+g1.S, g1.S.toString()+" → ε"))
      Gramatica(produccionConcatenacion++g1.producciones, g1.terminales, g1.variables, g1.S)
    }
}
  



