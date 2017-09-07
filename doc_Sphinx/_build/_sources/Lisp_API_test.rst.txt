Lisp API documentation test
===========================

The following tests report the interface of a Common Lisp function, but using the built-in doc interface for Python functions. It copies the doc-string of the function documentation directly into the RST file. 

.. function:: (clusterengine no-of-variables rnd? debug? rules metric-domain list-of-domains)
		 
	      The Cluster Engine - the main function of this library. 
	      Pitch domains cannot exist without at least one duration in the rhythm domain. Domains with only one value will not use up any time in the search process.
	      See the PWGL tutorials of this library for a detailed discussion of this function.
		 
.. function:: (r-rhythms-one-voice rule voices &optional (rule-type :true/false) (weight 1))

	      We can document Lisp function arguments as in Python documentation.

	      :RULE: is a logic statement in he form of a function. The output of the function has to be either true or false. If there are more than one input to the function, they will receive consecutive durations (or consecutive motifs depending on the input-mode). 
	      :VOICES: is the number for the voice (starting at 0) that the rule affects. It is possible to give a list of several voice numbers: The rule will then be applied to every voice in the list (independant of each other).

	      Now with type information.

	      :param function RULE: is a logic statement in he form of a function. The output of the function has to be either true or false. If there are more than one input to the function, they will receive consecutive durations (or consecutive motifs depending on the input-mode). 
	      :param int-or-list VOICES: is the number for the voice (starting at 0) that the rule affects. It is possible to give a list of several voice numbers: The rule will then be applied to every voice in the list (independant of each other).

	      Note that more complex types are currently difficult to show this way (seemingly needs to be a single word, at least with default settings).


Such Lisp documentation is displayed, but not fully supported. Because they are not recognised as Python function interfaces, they are **not included in the index**, nor are they layouted like Python functions. 


Lets try the Sphinx standard domain instead. It shows that they are no improvement over using the Python domain. The ``program`` directive cannot even contain anything. 


.. option:: (r-rhythms-one-voice rule voices &optional (rule-type :true/false) (weight 1))

	      :param function RULE: is a logic statement in he form of a function. The output of the function has to be either true or false. If there are more than one input to the function, they will receive consecutive durations (or consecutive motifs depending on the input-mode). 
	      :param int-or-list VOICES: is the number for the voice (starting at 0) that the rule affects. It is possible to give a list of several voice numbers: The rule will then be applied to every voice in the list (independant of each other).

	      Note that more complex types are currently difficult to show this way (seemingly needs to be a single word, at least with default settings).


.. object:: (r-rhythms-one-voice rule voices &optional (rule-type :true/false) (weight 1))

	      :param function RULE: is a logic statement in he form of a function. The output of the function has to be either true or false. If there are more than one input to the function, they will receive consecutive durations (or consecutive motifs depending on the input-mode). 
	      :param int-or-list VOICES: is the number for the voice (starting at 0) that the rule affects. It is possible to give a list of several voice numbers: The rule will then be applied to every voice in the list (independant of each other).

	      Note that more complex types are currently difficult to show this way (seemingly needs to be a single word, at least with default settings).



	      
