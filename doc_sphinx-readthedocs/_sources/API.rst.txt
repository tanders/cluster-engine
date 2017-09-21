==========
TODO Title
==========

    :Author: torsten



API
---

Extracting docs again :)

.. index::
   single: RULES->CLUSTER (function)

*function* (**RULES->CLUSTER** &REST RULES)
    Use this box to collect all rules before inputting them to the Cluster engine. It is possible to input the output of this box to a second Rules->Cluster box (to help organizing your rules in groups).

.. index::
   single: CLUSTERENGINE (function)

*function* (**CLUSTERENGINE** NO-OF-VARIABLES RND? DEBUG? RULES METRIC-DOMAIN LIST-OF-DOMAINS)
    The Cluster Engine - the main function of this library. 

    Pitch domains cannot exist without at least one duration in the rhythm domain. Domains with only one value will not use up any time in the search process.

    See the PWGL tutorials of this library for a detailed discussion of this function.

.. index::
   single: R-RHYTHMS-ONE-VOICE (function)

*function* (**R-RHYTHMS-ONE-VOICE** RULE VOICES INPUT-MODE &OPTIONAL RULE-TYPE WEIGHT)
    :RULE: is a logic statement in he form of a function. The output of the function has to be either true or false. If there are more than one input to the function, they will receive consecutive durations (or consecutive motifs depending on the input-mode). 

    :VOICES: is the number for the voice (starting at 0) that the rule affects. It is possible to give a list of several voice numbers: The rule will then be applied to every voice in the list (independant of each other).

    :INPUT-MODE: determines what type of variables the rule will receive in its inputs:

    - ``:durations`` The rule will receive individual durations, one for each input.

    - ``:dur/time`` As above, but the start-time of the duration will be indicated.
      Format: '(duration start-time), Ex. '(1/4 9/4)

    - ``:motifs`` The rule will receive motifs, one (consecutive) motif for each 
      input. A motif is a collection of durations that are grouped in 
      a list. Motifs are designed in the domain and cannot be 
      redesignedby the engine. Note that a motif may be a single 
      duration (a list with one duration-ratio) if it is defined as 
      such in the domain.

    - ``:motif/time`` As the previous selection, but with the start-time of the first event in the motif added. Format: '(motif start-time)
      Ex. '((1/4 -1/8) 9/8)

    - ``:all-durations`` All durations in the voice that are assigned at the time the rule is checked are given as a list of duration ratios. The list will thus become longer and longer during the search. The rule can only have ONE input in this mode.

    **Optional inputs**

    By expanding the box it is possible to use the rule as a heuristic switch rule. A heuristic switch rule is still using a logic statement (that outputs true or false), but the effect of the rule is different: If the rule is true, the weight (given in the <weight> input) is passed to the engine. If it is false, a weight of 0 will be passed. A candidate that receive a high weight will have a higher priority for being picked when the true/false rules are checked. A heuristic rule can never fail a candidate, nor can it trigger backtracking of the engine. Heuristic rules only sort the candidates locally before the strict rules are applied. Depending on the context, heuristic rules might have more or less of an effect. 

    Heuristic switch rules differs slightly form regular heuristic rules (the latter don't output true or false, but a weight that might vary dependingon the candidate).

    [Backtracking: This rule will trigger backtracking in its own engine.]


Here goes the general text on.
