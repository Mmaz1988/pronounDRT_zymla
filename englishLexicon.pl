/*************************************************************************

         name: englishLexicon.pl
      version: November 12, 1997; March 9, 1999.
  description: Lexical entries for a small coverage of English
      authors: Patrick Blackburn & Johan Bos
 
This file contains the lexical entries for a small fragment of
English.  Entries have the form lexicon(Cat,Sym,Phrase,Misc), where
Cat is the syntactic category, Sym the predicate symbol introduced
by the phrase, Phrase a list of the words that form the phrase, and
Misc miscellaneous information depending on the the type of entry.

*************************************************************************/

/*========================================================================
   Determiners: lexicon(det,_,Words,Type)
========================================================================*/

lexicon(det,_,[every],uni).
lexicon(det,_,[a],indef).
lexicon(det,_,[the],def).
lexicon(det,_,[one],card(1)).
lexicon(det,_,[another],alt).
lexicon(det,_,[his],poss(male)).
lexicon(det,_,[her],poss(female)).
lexicon(det,_,[its],poss(nonhuman)).

/*========================================================================
   Nouns: lexicon(noun,Symbol,Words,{[],[Hypernym],Hypernym})
========================================================================*/

lexicon(noun,witch,[witch],female).
lexicon(noun,wizard,[wizard],male).
lexicon(noun,human,[human],[organism]).
lexicon(noun,female,[female],[human]).
lexicon(noun,male,[male],[human]).

%lexicon(noun,princess,[princess],female).
%lexicon(noun,villain,[villain],human).
%lexicon(noun,troll,[troll],human).
%lexicon(noun,reeindeer,[reindeer],animal).
%lexicon(noun,sleigh,[sleigh],[vehicle]).
%lexicon(noun,snowman,[snowman],[organism]).
%lexicon(noun,sunbath,[sunbath],[act]).


/*
lexicon(noun,abstraction,[abstraction],[top]).
lexicon(noun,act,[act],[top]).
lexicon(noun,animal,[animal],[organism]).
lexicon(noun,artifact,[artifact],[object]).
lexicon(noun,beverage,[beverage],[food]).
lexicon(noun,building,[building],[artifact]).
lexicon(noun,container,[container],[instrumentality]).
lexicon(noun,cup,[cup],[container]).
lexicon(noun,device,[device],[instrumentality]).
lexicon(noun,edible,[edible,food],[food]).
lexicon(noun,bkburger,[big,kahuna,burger],[edible]).
lexicon(noun,boxer,[boxer],human).
lexicon(noun,boss,[boss],human).
lexicon(noun,car,[car],[vehicle]).
lexicon(noun,chainsaw,[chainsaw],[device]).
lexicon(noun,criminal,[criminal],human).
lexicon(noun,customer,[customer],human).
lexicon(noun,drug,[drug],[artifact]).
lexicon(noun,entity,[entity],[top]).
lexicon(noun,episode,[episode],abstraction).
lexicon(noun,female,[female],[human]).
lexicon(noun,fdshake,[five,dollar,shake],[beverage]).
lexicon(noun,food,[food],[object]).
lexicon(noun,footmassage,[foot,massage],[act]).
lexicon(noun,gimp,[gimp],human).
lexicon(noun,glass,[glass],[container]).
lexicon(noun,gun,[gun],[weaponry]).
lexicon(noun,hammer,[hammer],[device]).
lexicon(noun,hashbar,[hash,bar],[building]).
lexicon(noun,human,[human],[organism]).
lexicon(noun,husband,[husband],male).
lexicon(noun,instrumentality,[instrumentality],artifact).
lexicon(noun,joke,[joke],abstraction).
lexicon(noun,man,[man],male).
lexicon(noun,male,[male],[human]).
lexicon(noun,medium,[medium],[instrumentality]).
lexicon(noun,needle,[needle],[device]).
lexicon(noun,object,[object],[entity]).
lexicon(noun,organism,[organism],[entity]).
lexicon(noun,owner,[owner],human).
lexicon(noun,piercing,[piercing],[act]).
lexicon(noun,plant,[plant],[organism]).
lexicon(noun,qpwc,[quarter,pounder,with,cheese],[edible]).
lexicon(noun,radio,[radio],[medium]).
lexicon(noun,restaurant,[restaurant],[building]).
lexicon(noun,robber,[robber],human).
lexicon(noun,suitcase,[suitcase],[container]).
lexicon(noun,shotgun,[shotgun],[weaponry]).
lexicon(noun,sword,[sword],[weaponry]).
lexicon(noun,vehicle,[vehicle],[instrumentality]).
lexicon(noun,weaponry,[weaponry],[instrumentality]).
lexicon(noun,woman,[woman],female).
*/
  
/*========================================================================
   Proper Names: lexicon(pn,Symbol,Words,{male,female})
========================================================================*/

lexicon(pn,elsa,[elsa],female).
lexicon(pn,anna,[anna],female).
lexicon(pn,olaf,[olaf],male).
lexicon(pn,christopher,[christopher],male).
lexicon(pn,hans,[hans],male).



/*========================================================================
   Intransitive Verbs: lexicon(iv,Symbol,Words,{fin,inf})
========================================================================*/

lexicon(iv,pretty,[is,pretty],fin).
lexicon(iv,blush,[blushes],fin).


/*========================================================================
   Transitive Verbs: lexicon(tv,Symbol,Words,{fin,inf})
========================================================================*/


lexicon(tv,love,[loves],fin).
lexicon(tv,love,[love],inf).
lexicon(tv,hate,[hates],fin).
lexicon(tv,hate,[hate],inf).
lexicon(tv,trick,[tricks],fin).
lexicon(tv,trick,[trick],inf).
lexicon(tv,pull,[pulls],fin).
lexicon(tv,pull,[pull],inf).
lexicon(tv,trust,[trusts],fin).
lexicon(tv,trust,[trust],inf).
lexicon(tv,attack,[attacks],fin).
lexicon(tv,attack,[attacked],fin).
lexicon(tv,attack,[attack],inf).
lexicon(tv,create,[creates],fin).
lexicon(tv,create,[created],fin).
lexicon(tv,create,[create],inf).
lexicon(tv,melt,[melts],fin).
lexicon(tv,melt,[melt],inf).

/*========================================================================
   Copula
========================================================================*/

lexicon(cop,eq,[is],fin).

/*========================================================================
   Prepositions: lexicon(prep,Symbol,Words,_)
========================================================================*/

lexicon(prep,in,[in],_).
lexicon(prep,of,[of],_).
lexicon(prep,with,[with],_).

/*========================================================================
   Pronouns: lexicon(pro,Sym,Words,{refl,nonrefl})
========================================================================*/

lexicon(pro,male,[he],nonrefl).
lexicon(pro,female,[she],nonrefl).
lexicon(pro,nonhuman,[it],nonrefl).
lexicon(pro,male,[him],nonrefl).
lexicon(pro,female,[her],nonrefl).
lexicon(pro,male,[himself],refl).
lexicon(pro,female,[herself],refl).
lexicon(pro,nonhuman,[itself],refl).

/*========================================================================
   Relative Pronouns: lexicon(relpro,_,Words,_)
========================================================================*/

lexicon(relpro,_,[who],_).
lexicon(relpro,_,[that],_).

/*========================================================================
   Coordinations: lexicon(coord,_,Words,{conj,disj})
========================================================================*/

lexicon(coord,_,[and],conj).
lexicon(coord,_,[or],disj).

/*========================================================================
   Discontinious Coordinations: lexicon(dcoord,W1,W2,{conj,cond,disj})
========================================================================*/

lexicon(dcoord,[if],[then],cond).
lexicon(dcoord,[if],[],cond).
lexicon(dcoord,[either],[or],disj).
lexicon(dcoord,[],[or],disj).
lexicon(dcoord,[],[and],conj).
lexicon(dcoord,[],[],conj).

/*========================================================================
   Modifiers: lexicon(mod,_,Words,Type)
========================================================================*/

lexicon(mod,_,[does,not],neg).
lexicon(mod,_,[did,not],neg).
