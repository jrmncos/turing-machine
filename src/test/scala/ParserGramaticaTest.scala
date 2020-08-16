import concepto._
import parser.ParserGramatica
import org.scalactic.source.Position.apply

class ParserGramaticaTest extends UnitSpec 
{
  "Las terminales extraidas de (a, b, c)" must "ser igual a Set(a, b ,c)" in {
		val result = ParserGramatica.terminales("a, b, c")
		result should equal (Set(Terminal("a"), Terminal("b"), Terminal("c")))
	}
  
  "Las terminales extraidas de (n)" must "ser igual a Set(n)" in {
		val result = ParserGramatica.terminales("n")
		result should equal (Set(Terminal("n")))
	}

  "Las terminales extraidas de ( (, ), /, ., -, ^, & )" must "ser igual a Set( (, ), /, ., -, ^, &)" in {
		val result = ParserGramatica.terminales("(, ), /, ., -, ^, &")
		result should equal (Set(Terminal("("), Terminal(")"), Terminal("/"), Terminal("."), Terminal("-"), Terminal("&"), Terminal("^")))
	}
  
  "Las terminales extraidas de (5, 4, 2, 1, 3)" must "ser igual a Set(1, 2, 3, 4, 5)" in {
		val result = ParserGramatica.terminales("5, 4, 3, 2, 1")
		result should equal (Set(Terminal("1"), Terminal("2"), Terminal("3"), Terminal("4"), Terminal("5")))
	}
  
  "Las terminales extraidas de ()" must "ser igual a Set.empty" in {
    val result = ParserGramatica.terminales("")
    result should equal (Set.empty)
  }
  
  "Las terminales extraidas de (a, b, c, b, A)" must "ser igual a (a, b ,c)" in {
    val result = ParserGramatica.terminales("a, b, c, b, A")
    result should equal (Set(Terminal("a"), Terminal("b"), Terminal("c")))
  }

  "Las terminales extraidas de (A, B, C, N)" must "ser igual a Set.empty" in {
    val result = ParserGramatica.terminales("A, B, C, N")
    result should equal (Set.empty)
  }
  
  "Las variables extraidas de (S, A, B, Z)" must "ser igual a Set(S, A, B, Z)" in {
    val result = ParserGramatica.variables("S, A, B, Z")
    result should equal (Set(Variable("S"), Variable("A"), Variable("B"), Variable("Z")))
  }
  
  "Las variables extraidas de ()" must "ser igual a Set.empty" in {
    val result = ParserGramatica.variables("")
    result should equal (Set.empty)
  }
    
  "Las variables extraidas de (S, T, t, s, G)" must "ser igual a Set(S, T, G)" in {
    val result = ParserGramatica.variables("S, T, t, s, G")
    result should equal (Set(Variable("S"), Variable("T"), Variable("G")))
  }
  
  "Las variables extraidas de (s, g, e, 3, 5, -, a)" must "ser igual a Set.empty" in {
    val result = ParserGramatica.variables("s, g, e, 3, 5, -, a")
    result should equal (Set())
  }
  
  "Las producciones extraidas de S → 01, S → 0S1" must "ser igual a S → 01, S → 0S1" in {
    val result = ParserGramatica.producciones(List("S → 01", "S → 0S1"))
    result should equal (List(Produccion(Variable("S"), List(Terminal("0"), Terminal("1"))),
                              Produccion(Variable("S"), List(Terminal("0"), Variable("S"), Terminal("1")))))
  }

  "Las producciones extraidas de A → 01" must "ser igual a A → 01" in {
    val result = ParserGramatica.producciones(List("A → 01"))
    result should equal (List(Produccion(Variable("A"), List(Terminal("0"), Terminal("1")))))
  }

//  it should "Devolver List.empty cuando se quiera extraer producciones de List.empty" (pending)

  "La Gramatica generada por S → ε" must "contener las terminales (ε), variables (S) y producciones S → ε" in {
    val result = ParserGramatica.gramatica("S → ε")
    result.variables should equal (Set(Variable("S")))
    result.terminales should equal (Set(Terminal("ε")))
    result.producciones should equal (List(Produccion(Variable("S"), List(Terminal("ε")))))
  }
    
  "La Gramatica generada por S → A, A → Aa, A → a" must "contener las terminales (a), variables (S, A) y producciones S → A, A → Aa, A → a" in {
    val result = ParserGramatica.gramatica("S → A, A → Aa, A → a")
    result.variables should equal (Set(Variable("S"), Variable("A")))
    result.terminales should equal (Set(Terminal("a")))
    result.producciones should equal (List(Produccion(Variable("S"), List(Variable("A"))), 
                                           Produccion(Variable("A"), List(Variable("A"), Terminal("a"))),
                                           Produccion(Variable("A"), List(Terminal("a")))))
  }
  
  "La Gramatica generada por S → cAd, A → ab, A → a " must "contener las terminales (a, b, c, d), variables (S, A) y producciones S → cAd, A → ba, A → a" in {
    val result = ParserGramatica.gramatica("S → cAd, A → ba, A → a")
    result.variables should equal (Set(Variable("S"), Variable("A")))
    result.terminales should equal (Set(Terminal("a"), Terminal("b"), Terminal("c"), Terminal("d")))
    result.producciones should equal (List(Produccion(Variable("S"), List(Terminal("c"), Variable("A"), Terminal("d"))), 
                                           Produccion(Variable("A"), List(Terminal("b"), Terminal("a"))),
                                           Produccion(Variable("A"), List(Terminal("a")))))
  }
  
 ("La Gramatica generada por S → TE, E → +TE, E → ε, T → FP, P → *FP, P → ε, F → (S), F → id" 
  must "contener las terminales (ε, *, (, ), +, i, d), variables (S, T, E, F, P) y producciones S → TE, E → +TE, E → ε, T → FP, P → *FP, P → ε, F → (S), F → id") in {
    val result = ParserGramatica.gramatica("S → TE, E → +TE, E → ε, T → FP, P → *FP, P → ε, F → (S), F → id")
    result.variables should equal (Set(Variable("S"), Variable("T"), Variable("E"), Variable("F"), Variable("P")))
    result.terminales should equal (Set(Terminal("ε"), Terminal("*"), Terminal("i"), Terminal("d"), Terminal("("), Terminal(")"), Terminal("+")))
    result.producciones should equal (List(Produccion(Variable("S"), List(Variable("T"), Variable("E"))), 
                                           Produccion(Variable("E"), List(Terminal("+"), Variable("T"), Variable("E"))),
                                           Produccion(Variable("E"), List(Terminal("ε"))),
                                           Produccion(Variable("T"), List(Variable("F"), Variable("P"))),
                                           Produccion(Variable("P"), List(Terminal("*"), Variable("F"), Variable("P"))),
                                           Produccion(Variable("P"), List(Terminal("ε"))),
                                           Produccion(Variable("F"), List(Terminal("("), Variable("S"), Terminal(")"))),
                                           Produccion(Variable("F"), List(Terminal("i"), Terminal("d")))))
  }
 
  ("La Gramatica generada por S → E, S → S * S, S → S + S, S → (S), E → a, E → b, E → Ea, E → Eb, E → E0, E → E1, E → 0, E → 1" 
  must "contener las terminales (*, (, ), +, a, b, 0, 1), variables (S, E) y producciones S → E, S → S * S, S → S + S, S → (S), E → a, E → b, E → Ea, E → Eb, E → E0, E → E1, E → 0, E → 1") in {
    val result = ParserGramatica.gramatica("S → E, S → S * S, S → S + S, S → (S), E → a, E → b, E → Ea, E → Eb, E → E0, E → E1, E → 0, E → 1")
    result.variables should equal (Set(Variable("S"), Variable("E")))
    result.terminales should equal (Set(Terminal("*"), Terminal("+"), Terminal("("), Terminal(")"), Terminal("a"), Terminal("b"), Terminal("0"), Terminal("1")))
    result.producciones should equal (List(Produccion(Variable("S"), List(Variable("E"))), 
                                           Produccion(Variable("S"), List(Variable("S"), Terminal("*"), Variable("S"))),
                                           Produccion(Variable("S"), List(Variable("S"), Terminal("+"), Variable("S"))),
                                           Produccion(Variable("S"), List(Terminal("("), Variable("S"), Terminal(")"))),
                                           Produccion(Variable("E"), List(Terminal("a"))),
                                           Produccion(Variable("E"), List(Terminal("b"))),
                                           Produccion(Variable("E"), List(Variable("E"), Terminal("a"))),
                                           Produccion(Variable("E"), List(Variable("E"), Terminal("b"))),
                                           Produccion(Variable("E"), List(Variable("E"), Terminal("0"))),
                                           Produccion(Variable("E"), List(Variable("E"), Terminal("1"))),
                                           Produccion(Variable("E"), List(Terminal("0"))),
                                           Produccion(Variable("E"), List(Terminal("1")))))
  }
  
}