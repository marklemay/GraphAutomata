package tz

import tz.PStringNfa.Nfa
import tz.PStringNfa.Transition

object ws {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(127); 
  println("Welcome to the Scala worksheet");$skip(354); 

  val nfa = Nfa(Set(
    Transition(1, "a", 0.5, 2),
    Transition(1, "a", 0.5, 4),

    Transition(0, "a", 0.5, 1),
    Transition(0, "a", 0.5, 0),

    Transition(4, "b", 0.5, 4),
    Transition(4, "a", 0.5, 3),
    Transition(3, "c", 0.5, 1),
    Transition(3, "a", 0.5, 0),
    Transition(2, "c", 0.5, 3),
    Transition(2, "b", 0.5, 2)), 1, 3);System.out.println("""nfa  : tz.PStringNfa.Nfa[Int,String] = """ + $show(nfa ));$skip(113); val res$0 = 
  //  val nfa = PStringNfa.randomNFA(Set(0, 1, 2, 3, 4), Set("a", "b", "c"))(2)
  nfa.transitions.mkString("\n");System.out.println("""res0: String = """ + $show(res$0));$skip(33); val res$1 = 

  PStringNfa.randomString(nfa);System.out.println("""res1: List[String] = """ + $show(res$1));$skip(31); val res$2 = 
  PStringNfa.randomString(nfa);System.out.println("""res2: List[String] = """ + $show(res$2));$skip(31); val res$3 = 
  PStringNfa.randomString(nfa);System.out.println("""res3: List[String] = """ + $show(res$3));$skip(31); val res$4 = 
  PStringNfa.randomString(nfa);System.out.println("""res4: List[String] = """ + $show(res$4));$skip(31); val res$5 = 
  PStringNfa.randomString(nfa);System.out.println("""res5: List[String] = """ + $show(res$5));$skip(31); val res$6 = 
  PStringNfa.randomString(nfa);System.out.println("""res6: List[String] = """ + $show(res$6));$skip(31); val res$7 = 
  PStringNfa.randomString(nfa);System.out.println("""res7: List[String] = """ + $show(res$7));$skip(31); val res$8 = 
  PStringNfa.randomString(nfa);System.out.println("""res8: List[String] = """ + $show(res$8));$skip(31); val res$9 = 
  PStringNfa.randomString(nfa);System.out.println("""res9: List[String] = """ + $show(res$9));$skip(31); val res$10 = 
  PStringNfa.randomString(nfa);System.out.println("""res10: List[String] = """ + $show(res$10));$skip(31); val res$11 = 
  PStringNfa.randomString(nfa);System.out.println("""res11: List[String] = """ + $show(res$11));$skip(31); val res$12 = 
  PStringNfa.randomString(nfa);System.out.println("""res12: List[String] = """ + $show(res$12));$skip(31); val res$13 = 
  PStringNfa.randomString(nfa);System.out.println("""res13: List[String] = """ + $show(res$13));$skip(31); val res$14 = 
  PStringNfa.randomString(nfa);System.out.println("""res14: List[String] = """ + $show(res$14));$skip(31); val res$15 = 
  PStringNfa.randomString(nfa);System.out.println("""res15: List[String] = """ + $show(res$15));$skip(31); val res$16 = 
  PStringNfa.randomString(nfa);System.out.println("""res16: List[String] = """ + $show(res$16));$skip(31); val res$17 = 
  PStringNfa.randomString(nfa);System.out.println("""res17: List[String] = """ + $show(res$17));$skip(31); val res$18 = 
  PStringNfa.randomString(nfa);System.out.println("""res18: List[String] = """ + $show(res$18));$skip(31); val res$19 = 
  PStringNfa.randomString(nfa);System.out.println("""res19: List[String] = """ + $show(res$19));$skip(31); val res$20 = 
  PStringNfa.randomString(nfa);System.out.println("""res20: List[String] = """ + $show(res$20));$skip(31); val res$21 = 
  PStringNfa.randomString(nfa);System.out.println("""res21: List[String] = """ + $show(res$21));$skip(47); 

  val example = PStringNfa.randomString(nfa);System.out.println("""example  : List[String] = """ + $show(example ));$skip(46); 
  val example2 = PStringNfa.randomString(nfa);System.out.println("""example2  : List[String] = """ + $show(example2 ));$skip(45); 

  var sample = PStringNfa.minFrom(example);System.out.println("""sample  : tz.PStringNfa.Dfa[Boolean,String] = """ + $show(sample ));$skip(45); val res$22 = 

  PStringNfa.addEvidence(sample, example2);System.out.println("""res22: tz.PStringNfa.Dfa[Boolean,String] = """ + $show(res$22));$skip(103); 

  for (i <- 0.to(100)) {

    sample = PStringNfa.addEvidence(sample, PStringNfa.randomString(nfa))
  };$skip(24); 

  println(sample)}
}
