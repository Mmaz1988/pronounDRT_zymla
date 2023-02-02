/*************************************************************************

         name: betaConversion.pl (Chapter 2)
      version: March 11, 1998
  description: Implementation of Beta-Conversion
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(betaConversion,[betaConvert/2]).

:- use_module(comsemPredicates,[compose/3,substitute/4]).

/*========================================================================
   Beta-Conversion
========================================================================*/

betaConvert(Var,Result):-
   var(Var), !, Result=Var.
betaConvert(app(Functor,Arg),Result):-
   compound(Functor),
   betaConvert(Functor,ConvertedFunctor), 
   apply(ConvertedFunctor,Arg,BetaConverted), !,
   betaConvert(BetaConverted,Result).
betaConvert(Formula,Result):-
   compose(Formula,Functor,Formulas),
   betaConvertList(Formulas,ResultFormulas),
   compose(Result,Functor,ResultFormulas).

betaConvertList([],[]).
betaConvertList([Formula|Others],[Result|ResultOthers]):-
   betaConvert(Formula,Result),
   betaConvertList(Others,ResultOthers).

/*========================================================================
   Application (Unification-Based)
========================================================================*/

%apply(lambda(Argument,Result),Argument,Result).

/*========================================================================
   Application (Substitution-Based)
========================================================================*/

apply(lambda(X,Formula),Argument,Result):-
    substitute(Argument,X,Formula,Result).
